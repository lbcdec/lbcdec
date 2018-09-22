use std::ops::Range;

use cond_logic::{CondContext, CondTarget};
use instruction_definitions::{Reg, RK, Count};
use free_mark::FreeMark;
use reduce::Reduce;
use view::{RegRange, View, ViewData, ViewOrReg, ViewOrRegOrKst, ViewType, CallFunc, ViewRef, ViewKeyRef};
use view::assignment_info::*;
use view::newtable::{TableInfo, TableEntry};
use view::view_type::{ExpressionInfo, PartialCondInfo};

use ::ViewContext;

// TODO: Make drop panic if not canceled or built

pub struct ViewBuilder<'lb> {
    context: &'lb ViewContext<'lb>,
    instructions: Range<u32>,
    index: ViewRef,
    dependent_view_count: u32,
    current_top: ViewRef,
    last_strong: Option<u32>,
    allocations: Option<RegRange>,
    base: Option<Reg>,
    dependent_reg_top: Option<Reg>,
    free_mark: FreeMark,
    lowest_free_mark: Option<Reg>,
    passive_dependent_regs: bool
}

#[derive(Debug)]
pub enum AssignSource {
    View(ViewRef),
    RegKst(RK)
}

impl From<ViewRef> for AssignSource {
    fn from(value: ViewRef) -> AssignSource {
        AssignSource::View(value)
    }
}

impl From<RK> for AssignSource {
    fn from(value: RK) -> AssignSource {
        AssignSource::RegKst(value)
    }
}

impl AssignSource {
    fn take<'lb>(self, builder: &mut ViewBuilder<'lb>) -> ViewOrRegOrKst {
        match self {
            AssignSource::View(view) => {
                let v = builder.next_view();
                let committed = builder.commit_view(v);
                assert!(committed == view);
                ViewOrRegOrKst::View(view)
            },
            AssignSource::RegKst(rk) => builder.take_reg_or_kst(rk)
        }
    }
}

pub enum TakeAssignmentResult {
    Assignment(AssignmentInfo),
    TableInline(Reg, TableInfo)
}

impl<'lb> ViewBuilder<'lb> {
    pub fn new(context: &'lb ViewContext<'lb>, pc: u32, index: ViewRef, free_mark: FreeMark, lowest_free_mark: Option<Reg>) -> ViewBuilder<'lb> {
        ViewBuilder {
            context: context,
            instructions: pc..pc,
            index: index,
            dependent_view_count: 0,
            current_top: index,
            last_strong: None,
            allocations: None,
            base: None,
            dependent_reg_top: None,
            free_mark: free_mark,
            lowest_free_mark,
            passive_dependent_regs: false
        }
    }

    fn next_view(&self) -> &'lb View {
        self.context.view_at(self.current_top - 1)
    }

    fn commit_view(&mut self, view: &'lb View) -> ViewRef {
        let view_ref = self.current_top - 1;
        self.current_top = view.top;
        self.dependent_view_count += 1;
        self.free_mark = self.context.view_free_mark_at(view.index).incoming_root.clone();
        view_ref.into()
    }

    pub fn depend_on_register(&mut self, reg: Reg) {
        self.dependent_reg_top = if let Some(top) = self.dependent_reg_top {
            if top.is_above(reg) {
                Some(reg)
            } else {
                Some(top)
            }
        } else {
            Some(reg)
        }
    }

    // Uses an expression view with the specified register, returns view id
    pub fn take_reg(&mut self, reg: Reg) -> ViewOrReg {
        if self.current_top.is_first() {
            self.depend_on_register(reg);
            return ViewOrReg::Reg(reg);
        }

        if let Some(lowest_free_mark) = self.lowest_free_mark {
            if !reg.is_at_or_above(lowest_free_mark) {
                // Can never be a view, since this would make the free mark unstable
                self.depend_on_register(reg);
                return ViewOrReg::Reg(reg);
            }
        }

        if !self.free_mark.is_next_allocated(reg) {
            self.depend_on_register(reg);
            return ViewOrReg::Reg(reg);
        }

        let next_view = self.next_view();

        if let &ViewType::Expression { dest: dest_reg, .. } = &next_view.view_type {
            if dest_reg == reg {
                // Check for a pinned expression in to reg in scope
                let has_pinned = self.context.iter_root().any(|view| {
                    if let &ViewType::PinnedExpression { dest } = &view.view_type {
                        dest == reg
                    } else {
                        false
                    }
                });

                // Can inline if the reg hasn't been pinned in the past
                if !has_pinned {
                    let top = self.commit_view(next_view);
                    return ViewOrReg::View(top)
                }
            }

            self.depend_on_register(reg);
            ViewOrReg::Reg(reg)
        } else {
            self.depend_on_register(reg);
            ViewOrReg::Reg(reg)
        }
    }

    pub fn take_reg_or_kst(&mut self, rk: RK) -> ViewOrRegOrKst {
        match rk {
            RK::K(constant) => ViewOrRegOrKst::Kst(constant),
            RK::R(reg) => self.take_reg(reg).into()
        }
    }

    pub fn can_take_reg_strong(&mut self, reg: Reg) -> bool {
        if !self.free_mark.is_next_allocated(reg) {
            return false
        }

        let has_pinned = self.context.iter_root().any(|view| {
            if let &ViewType::PinnedExpression { dest } = &view.view_type {
                dest == reg
            } else {
                false
            }
        });

        if has_pinned {
            return false
        }

        return true
    }

    pub fn take_reg_strong(&mut self, reg: Reg) -> ViewRef {
        if !self.free_mark.is_next_allocated(reg) {
            panic!("Could not take strong register {:?}: Would cause free mark instability with {:?}", reg, self.context.free_mark());
        }

        if self.current_top.is_first() {
            panic!("Could not take strong register {:?}: At top of views", reg);
        }

        if let ViewOrReg::View(res) = self.take_reg(reg) {
            self.last_strong = Some(self.dependent_view_count);

            res
        } else {
            let has_pinned = self.context.iter_root().any(|view| {
                if let &ViewType::PinnedExpression { dest } = &view.view_type {
                    dest == reg
                } else {
                    false
                }
            });

            panic!("Take strong failed for register {:?} (has_pinned: {:?})", reg, has_pinned);
        }
    }

    pub fn is_next_view_self(&self, reg: Reg) -> bool {
        let next_view = self.next_view();

        if let &ViewType::SelfExpression { base } = &next_view.view_type {
            base == reg
        } else {
            false
        }
    }

    pub fn take_call_func(&mut self, reg: Reg) -> CallFunc {
        let next_view = self.next_view();

        if let &ViewType::SelfExpression { base } = &next_view.view_type {
            if base == reg {
                return CallFunc::SelfCall(self.commit_view(next_view));
            }
        }

        CallFunc::Func(self.take_reg_strong(reg))
    }

    pub fn take_varargs(&mut self) -> (ViewRef, Reg) {
        let next_view = self.next_view();

        if let &ViewType::VarArgsExpression { base } = &next_view.view_type {
            (self.commit_view(next_view), base)
        } else {
            panic!("Could not take varargs expression");
        }
    }

    pub fn take_partial_conditional(&mut self) -> (ViewRef, PartialCondInfo) {
        if self.current_top.is_first() {
            panic!("Could not take partial conditional: At top of views");
        }

        let next_view = self.next_view();

        if let &ViewType::PartialConditional(ref info) = &next_view.view_type {
            (self.commit_view(next_view), info.clone())
        } else {
            panic!("Could not take partial conditional: View is not a partial conditional {:?}", next_view)
        }
    }

    pub fn take_views_until<F>(&mut self, predicate: F) -> Vec<ViewRef>
        where F: Fn(&'lb View) -> bool {
        let mut out = vec![];
        loop {
            if self.current_top.is_first() {
                break;
            }

            let next_view = self.next_view();

            if !predicate(next_view) {
                out.push(self.commit_view(next_view));
            } else {
                break;
            }
        }
        out
    }

    pub fn take_scope(&mut self) -> ViewRef {
        if self.current_top.is_first() {
            panic!("Could not take scope: At top of views");
        }

        let next_view = self.next_view();

        if let &ViewType::Scope = &next_view.view_type {
            self.commit_view(next_view)
        } else {
            panic!("Could not take scope: View is not a scope")
        }
    }

    pub fn take_conditional(&mut self, on_true: u32, on_false: u32) -> (CondContext, Vec<ViewRef>) {
        if self.current_top.is_first() {
            panic!("Could not take conditional: At top of views");
        }

        let mut out = vec![];

        let mut context = {
            let next_view = self.next_view();

            if let &ViewType::Conditional { exit, .. } = &next_view.view_type {
                let target = CondTarget::new(next_view.instructions.end, next_view.instructions.end + 1, exit);
                out.push(self.commit_view(next_view));
                CondContext::new(target, on_true, on_false)
            } else {
                panic!("Could not take conditional: View is not a conditional")
            }
        };

        loop {
            if self.current_top.is_first() {
                break;
            }

            let next_view = self.next_view();

            if let &ViewType::Conditional { exit, .. } = &next_view.view_type {
                let target = CondTarget::new(next_view.instructions.end, next_view.instructions.end + 1, exit);
                if let Some(ref op) = context.try_insert(&target) {
                    context.apply(&op, target);
                    out.push(self.commit_view(next_view));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        (context, out)
    }

    pub fn take_conditional_loadbool(&mut self) -> (CondContext, Vec<ViewRef>) {
        if self.current_top.is_first() {
            panic!("Could not take conditional: At top of views");
        }

        let mut out = vec![];

        let mut context = {
            let next_view = self.next_view();

            if let &ViewType::SkippingLoadBool { .. } = &next_view.view_type {
                let target = CondTarget::new(next_view.instructions.end, next_view.instructions.end + 2, next_view.instructions.end + 2);
                out.push(self.commit_view(next_view));
                CondContext::new_multi(target, next_view.instructions.end - 1, next_view.instructions.end + 1, next_view.instructions.end)
            } else {
                panic!("Could not take conditional: View is not a skipping loadbool")
            }
        };

        loop {
            if self.current_top.is_first() {
                break;
            }

            let next_view = self.next_view();

            if let &ViewType::Conditional { exit, .. } = &next_view.view_type {
                let target = CondTarget::new(next_view.instructions.end, next_view.instructions.end + 1, exit);
                if let Some(ref op) = context.try_insert(&target) {
                    context.apply(&op, target);
                    out.push(self.commit_view(next_view));
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        (context, out)
    }

    pub fn take_else(&mut self) -> ViewRef {
        if self.current_top.is_first() {
            panic!("Could not take else: At top of views");
        }

        let next_view = self.next_view();

        if let &ViewType::Else { .. } = &next_view.view_type {
            self.commit_view(next_view)
        } else {
            panic!("Could not take else: View is not an else view")
        }
    }

    // TODO: Support `local m = {}; m.x, m.y, m = 3, m.y, 8`
    // What makes this different is that dest inlines that have no side effects (like m being the table) needs to get stored if m is a destination in the expression
    
    // TODO: Ability to flag a view for passive register dependencies
    // We want to pin things that show up on the lhs and rhs of an assignment _ONLY IF_ they're depended on by another expression (causing them to get inlined)
    // But we don't want to pin something we may inline in a complex assignment expression
    // We want to be able to individually select registers for passive pinning

    fn depend_on_partial_assignment(&mut self, lhs: &PartialAssignmentLHS) {
        match lhs {
            &PartialAssignmentLHS::Table { table, index } => {
                // We want the table and the index to be passively pinned
                self.passive_dependent_regs = true;
                self.depend_on_register(table);
                if let RK::R(index) = index {
                    self.depend_on_register(index);
                }
            },
            &PartialAssignmentLHS::Local(reg) => {
                // We want the local to be actively pinned
                self.depend_on_register(reg);
            },
            &PartialAssignmentLHS::Global(_) => (),
            &PartialAssignmentLHS::Upvalue(_) => (),
            &PartialAssignmentLHS::Multi { .. } => (),
        }
    }

    fn can_inline_into_table(&self, table: Reg, index: RK, source: &AssignSource) -> bool {
        if self.current_top.is_first() {
            return false;
        }
        
        let mut next_view = self.next_view();
        let mut free_mark = self.free_mark.clone();

        if let &AssignSource::RegKst(RK::R(source)) = source {
            if source == table {
                return false;
            }

            if free_mark.is_next_allocated(source) {
                // Check if next_view is source
                if let &ViewType::Expression { dest, .. } = &next_view.view_type {
                    if dest == source {
                        // Skip to next next view
                        if next_view.top.is_first() {
                            return false;
                        }

                        let next_next_view = self.context.view_at(next_view.top - 1);

                        next_view = next_next_view;
                        free_mark = free_mark.deallocate_single(source);
                    }
                }
            }
        }

        if let RK::R(index) = index {
            if index == table {
                return false;
            }

            if free_mark.is_next_allocated(index) {
                // Check if next_view is index
                if let &ViewType::Expression { dest, .. } = &next_view.view_type {
                    if dest == index {
                        // Skip to next next view
                        if next_view.top.is_first() {
                            return false;
                        }

                        let next_next_view = self.context.view_at(next_view.top - 1);

                        next_view = next_next_view;
                        free_mark = free_mark.deallocate_single(index);
                    }
                }
            }
        }

        if !free_mark.is_next_allocated(table) {
            return false;
        }

        if let &ViewType::Expression { dest, info: ExpressionInfo::Table(ref table_info), .. } = &next_view.view_type {
            dest == table && table_info.remaining_hash > 0
        } else {
            false
        }
    }
    
    // TODO: An arbitrary assignment can be the center
    // TODO: Handle MultiExpressions as the center
    pub fn take_for_assignment(&mut self, source: AssignSource, lhs: PartialAssignmentLHS) -> TakeAssignmentResult {
        if self.current_top.is_first() {
            self.depend_on_partial_assignment(&lhs);
            return TakeAssignmentResult::Assignment(AssignmentInfo {
                lhs: AssignmentLHS::Partial(vec![lhs]),
                sources: vec![source.take(self)],
                allocated_local: false
            })
        }

        if let &PartialAssignmentLHS::Table { table, index } = &lhs {
            if self.can_inline_into_table(table, index, &source) {
                let source_view = source.take(self);
                let index_view = self.take_reg_or_kst(index);
                let table_view = self.next_view();
                if let &ViewType::Expression { dest, info: ExpressionInfo::Table(ref table_info), .. } = &table_view.view_type {
                    assert!(dest == table);
                    self.commit_view(table_view);
                    let mut cloned_table_info = table_info.clone();
                    cloned_table_info.remaining_hash -= 1;
                    cloned_table_info.entries.push(TableEntry::Hash { k: index_view, v: source_view });
                    return TakeAssignmentResult::TableInline(table, cloned_table_info);
                } else {
                    panic!("can_inline_into_table lied!");
                }
            }
        }
        
        let center = match &source {
            &AssignSource::RegKst(RK::R(reg)) => {
                // Only if next view is an expression that sets this
                let next_view = self.next_view();
                if let &ViewType::Expression { dest, .. } = &next_view.view_type {
                    dest == reg
                } else {
                    false
                }
            },
            _ => true
        };

        let inner_assignment: Option<(Vec<PartialAssignmentLHS>, Vec<ViewOrRegOrKst>)> = if !center {
            // Previous assignment needs to be center then
            // When we combine assignments the resulting assignment is centered
            let next_view = self.next_view();
            let next_next_view_type = if next_view.top.is_first() {
                None
            } else {
                Some(&self.context.view_at(next_view.top - 1).view_type)
            };
            match (&next_view.view_type, next_next_view_type, &source) {
                (&ViewType::Statement {
                    assignment: Some(AssignmentInfo {
                        lhs: AssignmentLHS::Partial(ref partial),
                        ref sources,
                        allocated_local: false,
                        ..
                    })
                }, ..) => {
                    self.commit_view(next_view);
                    Some((partial.clone(), sources.clone()))
                },
                (&ViewType::Expression { dest, .. }, Some(&ViewType::Expression { dest: other_dest, .. }), &AssignSource::RegKst(RK::R(source))) if source.is_above(dest) && other_dest == source => {
                    let center_view = self.commit_view(next_view);
                    Some((
                        vec![PartialAssignmentLHS::Local(dest)],
                        vec![ViewOrRegOrKst::View(center_view)]
                    ))
                },
                (&ViewType::MultiExpression { base, count }, .., &AssignSource::RegKst(RK::R(source))) if Some(source) == count.top(base) => {
                    // Partial assignment with a multiexpression tail
                    let multiexpr = self.commit_view(next_view);
                    Some((
                        vec![PartialAssignmentLHS::Multi {
                            base: base,
                            count: count.count().unwrap() as u8
                        }],
                        vec![ViewOrRegOrKst::View(multiexpr)]
                    ))
                },
                _ => {
                    Some((
                        vec![],
                        vec![]
                    ))
                }
            }
        } else {
            None
        };

        let (mut partial, mut sources) = inner_assignment.unwrap_or((vec![], vec![]));

        match partial.first() {
            Some(&PartialAssignmentLHS::Multi { base, count }) => {
                let expected = Reg(base.0 + (count - 1));
                match source {
                    AssignSource::RegKst(RK::R(source)) if source == expected => {
                        partial.push(lhs.clone());
                        if count == 1 {
                            partial.remove(0);
                        } else {
                            partial[0] = PartialAssignmentLHS::Multi {
                                base,
                                count: count - 1
                            }
                        }
                    },
                    _ => panic!("Bad register for multi: expected {:?} got {:?}", expected, source)
                }
            },
            _ => {
                partial.push(lhs.clone());
                sources.push(source.take(self));
            }
        }

        // Try to figure out a required base for dest inline
        let required_base = partial.iter()
            .filter_map(|lhs| lhs.pull_base_reg())
            .reduce(|a, b| if a.is_above(b) { a } else { b });

        trace!("Required base: {:?} | {:?}", required_base, self.free_mark);

        if let Some(required_base) = required_base {
            if self.free_mark.is_next_allocated(required_base) && partial.iter().all(|p| p.can_convert_to_full()) {
                // Convert partials into fulls
                let full: Vec<FullAssignmentLHS> = partial
                    .into_iter()
                    .map(move |p| p.to_full(self))
                    .collect();

                // TODO: Depend on full assignments

                return TakeAssignmentResult::Assignment(AssignmentInfo {
                    lhs: AssignmentLHS::Full(full),
                    sources: sources,
                    allocated_local: false
                })
            }
        }

        trace!("{:?} {:?}", partial, sources);

        self.depend_on_partial_assignment(&lhs);
        TakeAssignmentResult::Assignment(AssignmentInfo {
            lhs: AssignmentLHS::Partial(partial),
            sources: sources,
            allocated_local: false
        })
    }

    pub fn take_setlist(&mut self, base: Reg, count: Count) -> TableInfo {
        let mut new_entries: Vec<TableEntry> = vec![];
        let mut next_reg: Option<Reg> = None;
        let mut array_count: usize = 0;
        let mut hash_count: usize = 0;

        loop {
            let next_view = self.next_view();

            if let &ViewType::Expression { dest, ref info, .. } = &next_view.view_type {
                if dest.is_above(base) {
                    if let Some(next_dest) = next_reg {
                        if dest != next_dest {
                            panic!("Found hole in SetList at {:?} (wanted {:?})", dest, next_dest);
                        }
                    }
                    new_entries.push(TableEntry::Array(self.commit_view(next_view)));
                    array_count += 1;
                    next_reg = dest.previous();
                    continue;
                } else if dest == base {
                    // Make sure its a Table expression
                    if let ExpressionInfo::Table(table_info) = info {
                        // Clone the info and add new entries
                        let mut cloned_table_info = table_info.clone();
                        self.commit_view(next_view);
                        if !count.is_varargs() {
                            cloned_table_info.remaining_array -= array_count;
                        }
                        cloned_table_info.remaining_hash -= hash_count;
                        cloned_table_info.entries.extend(new_entries.drain(..).rev());
                        break cloned_table_info;
                    } else {
                        panic!("Expected {:?} to have table info", next_view);
                    }
                }
            } else if let &ViewType::Statement { assignment: Some(AssignmentInfo { lhs: AssignmentLHS::Partial(ref partials), ref sources, allocated_local: false }), .. } = &next_view.view_type {
                if let &[PartialAssignmentLHS::Table { table, index }] = &partials[..] {
                    if table == base {
                        if let &[source] = &sources[..] {
                            self.commit_view(next_view);
                            let index = self.take_reg_or_kst(index);
                            new_entries.push(TableEntry::Hash { k: index, v: source });
                            hash_count += 1;
                            continue;
                        }
                    }
                }
            } else if let &ViewType::VarArgsExpression { base: varargs_base } = &next_view.view_type {
                assert!(next_reg == None);
                new_entries.push(TableEntry::Array(self.commit_view(next_view)));
                array_count += 1;
                next_reg = varargs_base.previous();
                continue;
            }

            panic!("Hit unexpected view {:?} in SetList", next_view);
        }
    }

    pub fn take_for_prep(&mut self) -> ViewRef {
        if self.current_top.is_first() {
            panic!("Could not take for prep: At top of views");
        }

        let next_view = self.next_view();

        if let ViewType::ForPrep { .. } = next_view.view_type {
            self.commit_view(next_view)
        } else if let ViewType::TForPrep { .. } = next_view.view_type {
            self.commit_view(next_view)
        } else {
            panic!("Could not take for prep: View is not a for prep view")
        }
    }

    pub fn take_generic_for_params(&mut self, base: Reg) -> Vec<ViewRef> {
        let mut ret = vec![];

        let mut current_reg = {
            let next_view = self.next_view();

            match next_view.view_type {
                ViewType::MultiExpression { base: expr_base, count } => {
                    assert!(expr_base.is_at_or_above(base));

                    ret.push(self.commit_view(next_view));

                    if count == Count::from(3) {
                        None
                    } else {
                        // We want to panic if we can't get the previous register
                        Some(expr_base.previous().unwrap())
                    }
                },
                ViewType::Expression { dest, .. } => {
                    assert!(dest == Reg(base.0 + 2));

                    ret.push(self.take_reg_strong(Reg(base.0 + 2)));

                    Some(Reg(base.0 + 1))
                },
                _ => {
                    for root_view in self.context.iter_root() {
                        println!("{:?}", root_view);
                    }
                    panic!("Could not pull tail expression for generic for initializer {:?}", next_view)
                }
            }
        };

        while let Some(current) = current_reg {
            ret.push(self.take_reg_strong(current));

            current_reg = current.previous().and_then(|p| if p.is_at_or_above(base) { Some(p) } else { None });
        }

        ret
    }

    pub fn take_loadbool_head(&mut self, dest: Reg) -> Option<ViewRef> {
        let next_view = self.next_view();

        match &next_view.view_type {
            &ViewType::Expression { dest: dest_reg, .. } if dest_reg == dest => {
                Some(self.commit_view(next_view))
            },
            &ViewType::Conditional { .. } => {
                None
            },
            _ => panic!("Could not take loadbool head")
        }
    }

    pub fn take_multi_return_value(&mut self, top: Reg) -> (Reg, ViewRef) {
        let next_view = self.next_view();

        match &next_view.view_type {
            &ViewType::MultiExpression { base, count }
                if top.is_at_or_above(base) && (base + count).unwrap().is_above(top) => {
                (base, self.commit_view(next_view))
            },
            _ => (top, self.take_reg_strong(top))
        }
    }

    pub fn take_simple_cond_tail(&mut self, dest: Reg) -> ViewRef {
        // Try a strong take first
        if self.can_take_reg_strong(dest) {
            return self.take_reg_strong(dest)
        }

        let next_view = self.next_view();

        match &next_view.view_type {
            &ViewType::Expression { dest: dest_reg, .. } if dest_reg == dest => {
                self.depend_on_register(dest);
                self.commit_view(next_view)
            },
            _ => panic!("Could not take loadbool head")
        }
    }

    pub fn allocate(&mut self, alloc: RegRange) {
        match self.allocations {
            None => self.allocations = Some(alloc),
            Some(_) => panic!("Can only call ViewBuilder::allocate once"),
        }
    }

    pub fn allocate_single(&mut self, alloc: Reg) {
        self.allocate(alloc..alloc);
    }

    pub fn base(&mut self, reg: Reg) {
        match self.base {
            None => self.base = Some(reg),
            Some(_) => panic!("Can only call ViewBuilder::base once"),
        }
    }

    pub fn build(self, data: Box<ViewData>, view_type: ViewType, key: ViewKeyRef) -> View {
        View {
            index: self.index,
            data: data,
            view_type: view_type,
            instructions: self.instructions,
            dependent_view_count: self.dependent_view_count,
            strong_dependent_view_count: self.last_strong,
            top: self.current_top,
            allocations: self.allocations,
            base: self.base,
            dependent_reg_top: self.dependent_reg_top,
            passive_dependent_regs: self.passive_dependent_regs,
            key,
        }
    }
}
