use std::ops::Range;

use bytecode_reader::*;
use cond_logic::*;
use instruction_definitions::*;
use instruction_decoder::decode_instruction;
use free_mark::*;
use view::*;

/// Type of instruction for an incoming jump.
#[derive(Debug)]
enum JumpSource {
    Jump,
    ConditionalJump,
    ForLoop,
    ForPrep
}

/// Whether this instruction was skipped by the previous instruction,
/// and if so: what type of instruction.
#[derive(Debug)]
enum SkipInfo {
    None,
    Conditional { test_set: bool },
    LoadBool,
}

/// Extra data used to label non-local information about an instruction.
#[derive(Debug)]
struct InstrExdata {
    /// Backward jump sources
    jump_sources: Vec<(JumpSource, u32)>,
    skipped_by_previous: SkipInfo,
    jump_to: Option<u32>,
}

/// Tags that can be applied to insttructions that affect the generation of views.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ViewTag {
    /// Whether the output of the expression-based view is pinned to a local or not
    /// 
    /// Expressions that are not pinned are candidates for inlining.
    /// Situations may come up where an expression can't be inlined somewhere, typically because it can't be used twice.
    /// When this happens, the expression is marked as pinned and a ViewBail is returned
    Pinned { view_key: ViewKeyRef },
    RepeatUntilTail,
    RepeatUntilLeaf,
}

/// Various free marks for a view.
#[derive(Debug)]
pub struct ViewFreeMarkInfo {
    /// Free mark coming from the previous view.
    /// Equal to `outgoing` of the previous view, or the initial free mark
    /// of the ViewContext.
    pub incoming: FreeMark,
    /// Free mark coming from the last root level view.
    /// Equal to `outgoing` of the last root level view, or the initial free mark
    /// of the ViewContext.
    pub incoming_root: FreeMark,
    /// Free mark going out of this view.
    pub outgoing: FreeMark
}

pub struct RootViewIterator<'a> {
    views: &'a Vec<View>,
    done: bool,
    index: ViewRef
}

impl<'a> Iterator for RootViewIterator<'a> {
    type Item = &'a View;
    fn next(&mut self) -> Option<&'a View> {
        if self.done {
            None
        } else {
            let result = &self.views[self.index.raw() as usize];
            let offset = result.top;
            if offset.is_first() {
                self.done = true;
            } else {
                self.index = offset - 1;
            }
            Some(result)
        }
    }
}

/// Contextual information about the current state of decompilation.
/// Contains data used to translate between instructions and views.
pub struct ViewContext<'lb> {
    chunk: &'lb LuaChunk,
    decoded: Vec<LuaInstruction>,
    instr_ex: Vec<InstrExdata>,
    views: Vec<View>,
    view_free_mark: Vec<ViewFreeMarkInfo>,
    /// View specific tags on each instruction (currently just Pinned)
    view_tags: Vec<Option<ViewTag>>,
    pc: u32,
    actual_param_count: u8,
    initial_free_mark: FreeMark,
    free_mark: FreeMark,
    ralloc: crate::ralloc::RAllocData,
}

impl<'lb> ViewContext<'lb> {
    /// Create and initialize a ViewContext by decoding some initial data from the given chunk.
    pub fn new(chunk: &'lb LuaChunk) -> ViewContext {
        let arg_alloc = if let Some(LuaVarArgInfo { has_arg: true,  .. }) = chunk.is_vararg { 1 } else { 0 };
        let initial_reg_alloc = chunk.num_params + arg_alloc;

        let instr_len = chunk.instructions.len();

        let mut instructions: &[u32] = &chunk.instructions;
        let mut next_skip = SkipInfo::None;
        let mut pc = 0u32;

        let mut decoded = Vec::with_capacity(instr_len);
        let mut instr_ex = Vec::with_capacity(instr_len);
        
        trace!("0:");
        while let Some((next_instr, instr, instr_count)) = decode_instruction(instructions, &chunk.prototypes) {
            trace!("{}:", pc + instr_count);
            instructions = next_instr;

            // Instructions are still in charge of inlining, but we eagerly bail when our base register doesn't match.
            // We still allow strong register takes when the free mark doesn't match.
            // We should modify the algorithm to take a "required free mark range" since base expressions tell us exactly whats allocated and what isn't.
            trace!("{}-{}: {:?} (base: {:?})", pc, pc + instr_count - 1, instr, instr.info());

            // Figure out the skip info for the next instruction
            let next_next_skip = match instr {
                LuaInstruction::LoadBool { skip_next: true, .. } => SkipInfo::LoadBool,
                LuaInstruction::BinCondOp { .. } => SkipInfo::Conditional { test_set: false },
                LuaInstruction::Test { .. } => SkipInfo::Conditional { test_set: false },
                LuaInstruction::TestSet { .. } => SkipInfo::Conditional { test_set: true },
                _ => SkipInfo::None,
            };

            // If this instruction is a jump, figure out the right enum from the instruction type
            let jump_source = match instr {
                LuaInstruction::Jump { .. } => {
                    if let SkipInfo::Conditional { .. } = next_skip {
                        Some(JumpSource::ConditionalJump)
                    } else {
                        Some(JumpSource::Jump)
                    }
                },
                LuaInstruction::ForLoop { .. } => Some(JumpSource::ForLoop),
                LuaInstruction::ForPrep { .. } => Some(JumpSource::ForPrep),
                _ => None,
            };

            // If this instruction is a jump, get the jump offset and apply it to the current program counter
            let jump_location = match instr {
                LuaInstruction::Jump { offset } => Some(offset),
                LuaInstruction::ForLoop { target, .. } => Some(target),
                LuaInstruction::ForPrep { target, .. } => Some(target),
                _ => None,
            }.map(|offset| offset.apply(pc));

            decoded.push(instr);
            instr_ex.push(InstrExdata {
                jump_sources: vec![],
                skipped_by_previous: next_skip,
                jump_to: jump_location,
            });

            // Pad long instructions with semantic no ops
            for _ in 0..(instr_count - 1) {
                decoded.push(LuaInstruction::SemanticNoOp);
                instr_ex.push(InstrExdata {
                    jump_sources: vec![],
                    skipped_by_previous: SkipInfo::None,
                    jump_to: None,
                });
            }

            // If this instruction is jumping backwards somewhere, link the source and destination
            if let Some(dest_pc) = jump_location {
                if dest_pc <= pc {
                    instr_ex[dest_pc as usize].jump_sources.push((jump_source.unwrap(), pc));
                }
            }

            next_skip = next_next_skip;
            pc = pc + instr_count;
        }

        let ralloc = crate::ralloc::run(&decoded[..]);

        let mut context = ViewContext {
            chunk: chunk,
            decoded,
            instr_ex,
            views: vec![],
            view_free_mark: vec![],
            view_tags: Vec::with_capacity(instr_len),
            pc: 0u32,
            actual_param_count: initial_reg_alloc,
            initial_free_mark: FreeMark::new(initial_reg_alloc),
            free_mark: FreeMark::new(initial_reg_alloc),
            ralloc,
        };

        context.view_tags.resize(instr_len, None);

        context
    }

    pub fn decompile(&mut self) {
        let mut pc = 0;
        while pc <= self.decoded.len() {
            if pc == self.decoded.len() {
                let end_view = self.end_view();
                match self.try_finalize(ViewRef::from(0)..end_view) {
                    Ok(can_create) => {
                        if !can_create {
                            println!("Failed to correctly decompile: Non-statement views detected during chunk finalization. Finalization aborted.");
                        }
                    },
                    Err(bail) => {
                        if self.handle_view_bail(bail, &mut pc) {
                            continue
                        }
                    },
                }

                pc += 1;
                continue
            }

            if let Err(bail) = self.process_instruction(pc as u32) {
                if self.handle_view_bail(bail, &mut pc) {
                    continue
                }
            }

            pc += 1;
        }
    }

    fn handle_view_bail(&mut self, bail: ViewBail, pc: &mut usize) -> bool {
        trace!("Bail {:?}", bail);
        match bail {
            ViewBail::PinExpression { mut view } => {
                *pc = self.view_at(view).instructions.end as usize;
                // Adjust view to not include any instructions at or after pc
                loop {
                    if view.is_first() {
                        break
                    }
                    let prev: ViewRef = (view.raw() - 1).into();
                    if (self.view_at(prev).instructions.end as usize) >= *pc {
                        view = prev
                    } else {
                        break
                    }
                }
                // Throw away views until `view-1`
                self.revert_to(view);
                return true;
            },
            ViewBail::ImplicitNil { start, end } => {
                *pc = 0;
                self.revert_to(0.into());

                let view = {
                    let mut builder = self.builder(None);
                    let mut lhs = vec![];
                    {
                        let mut reg = start;
                        loop {
                            lhs.push(FullAssignmentLHS::Local(reg));
                            if reg == end {
                                break;
                            }
                            reg = reg.next();
                        }
                    }
                    builder.allocate(start..end);
                    builder.build(
                        // TODO: Give the ViewType to the debug print fn instead of this
                        Box::new(ImplicitNilViewData {
                            start,
                            end
                        }),
                        ViewType::assignment(AssignmentInfo {
                            lhs: AssignmentLHS::Full(lhs),
                            sources: vec![],
                            allocated_local: true
                        }),
                        make_view_key!(name: "implicit nil", desc: "leading implicit nils of a lua chunk, emitted within a bail")
                    )
                };
                // We shouldn't bail here, ever.
                self.add_view(view).expect("Hit view bail while trying to add ImplicitNilViewData");

                return true;
            },
            ViewBail::ScopeWrap { view, rewind_view, retry } => {
                if let Some(mut rewind_view) = rewind_view {
                    // TODO: Deduplicate bail rewind code
                    *pc = self.view_at(rewind_view).instructions.end as usize;
                    // Adjust view to not include any instructions at or after pc
                    loop {
                        if rewind_view.is_first() {
                            break
                        }
                        let prev: ViewRef = (rewind_view.raw() - 1).into();
                        if (self.view_at(prev).instructions.end as usize) >= *pc {
                            rewind_view = prev
                        } else {
                            break
                        }
                    }
                    // Throw away views until `view-1`
                    self.revert_to(rewind_view);
                }

                self.make_scope_including(view, make_view_key!(name: "do block scope", desc: "emitted within bail for do block")).expect("Hit view bail during ScopeWrap");
                
                let view = {
                    let mut builder = self.builder(None);
                    let main_body = builder.take_scope();
                    builder.build(
                        Box::new(DoStatViewData { main_body }),
                        ViewType::simple_statement(),
                        make_view_key!(name: "do block", desc: "emitted within a bail")
                    )
                };
                
                self.add_view(view).expect("Hit view bail adding DoStatViewData during ScopeWrap");
                
                if retry {
                    return true
                } else {
                    return false
                }
            },
        }
    }

    pub fn chunk(&self) -> &LuaChunk {
        self.chunk
    }

    pub fn view_at(&self, index: ViewRef) -> &View {
        &self.views[index.raw() as usize]
    }

    fn view_at_mut(&mut self, index: ViewRef) -> &mut View {
        &mut self.views[index.raw() as usize]
    }

    fn views_between(&self, range: Range<ViewRef>) -> &[View] {
        &self.views[range.start.raw() as usize..range.end.raw() as usize]
    }

    pub fn view_free_mark_at(&self, index: ViewRef) -> &ViewFreeMarkInfo {
        &self.view_free_mark[index.raw() as usize]
    }

    fn end_view(&self) -> ViewRef {
        (self.views.len() as u32).into()
    }

    pub fn free_mark(&self) -> &FreeMark {
        &self.free_mark
    }

    /// Iterates through all root views from last to first
    pub fn iter_root<'a>(&'a self) -> RootViewIterator<'a> {
        let len = self.views.len();
        RootViewIterator {
            views: &self.views,
            done: len == 0,
            index: if len == 0 { 0 } else { (len - 1) as u32 }.into()
        }
    }

    /// Creates a view builder with parts of the context's state
    fn builder<'a>(&'a self, mut lowest_free_mark: Option<Reg>) -> ViewBuilder<'a> {
        if self.actual_param_count != 0 {
            let param_top = Reg(self.actual_param_count);
            lowest_free_mark = lowest_free_mark
                .map(|lfm| if lfm.is_above(param_top) { lfm } else { param_top })
                .or(Some(param_top));
        }
        ViewBuilder::new(&self, self.pc, self.end_view(), self.free_mark.clone(), lowest_free_mark)
    }

    /// Rewinds the context back to before the given ViewRef.
    /// For example: If the ViewRef points to the first view then the ViewContext is emptied.
    fn revert_to(&mut self, view: ViewRef) {
        let size = view.raw() as usize;
        self.views.truncate(size);
        self.view_free_mark.truncate(size);
        if view.is_first() {
            self.free_mark = self.initial_free_mark.clone();
        } else {
            self.free_mark = self.view_free_mark_at(view - 1).outgoing.clone();
        }
    }

    fn tag_view_instr(&mut self, view: ViewRef, tag: ViewTag) {
        let view_instr_end = self.view_at(view).instructions.end;
        use std::mem;
        let old = mem::replace(&mut self.view_tags[view_instr_end as usize], Some(tag.clone()));
        if let Some(old) = old {
            if old != tag {
                panic!("Tried to tag a {:?} instruction as {:?}", old, tag);
            }
        }
    }

    fn replace_view_tag(&mut self, view: ViewRef, tag: ViewTag) {
        let view_instr_end = self.view_at(view).instructions.end;
        self.view_tags[view_instr_end as usize] = Some(tag.clone());
    }

    fn can_pin_view(&self, view: ViewRef) -> bool {
        let view_instr_end = self.view_at(view).instructions.end;

        let tag = &self.view_tags[view_instr_end as usize];

        tag.is_none()
    }

    fn current_tag(&self) -> &Option<ViewTag> {
        &self.view_tags[self.pc as usize]
    }

    /// Verifies register dependencies for each root view before view_top.
    /// Will attempt to upgrade any unpinned views if passive is false.
    fn verify_dependent_registers(&mut self, dependent_reg_top: Reg, passive: bool, view_top: ViewRef) -> Result<(), ViewBail> {
        #[derive(Debug, Clone)]
        enum PinMode {
            None,
            Pinned,
            Ignored,
            InPlace(ViewRef),
            Backtrack(ViewRef)
        }
        let mut results = vec![PinMode::None; (dependent_reg_top.0 as usize) + 1];
        
        // Annotate chunk parameters as pinned
        {
            let mut param = Reg(0);
            let top_param = (param + Count::from(self.actual_param_count as u16)).unwrap();
            while param != top_param {
                if param.is_above(dependent_reg_top) {
                    break
                }
                trace!("{:?} already pinned because argument", param);
                results[param.0 as usize] = PinMode::Pinned;
                param = param.next();
            }
        };

        for view in self.iter_root() {
            if let &ViewType::Scope = &view.view_type {
                continue
            }

            if view.index >= view_top {
                // We don't want to accidentally pin our dependent views
                continue
            }

            trace!("{:?}", view.index);

            if let &ViewType::Expression { dest, .. } = &view.view_type {
                // Unpinned root expression, upgrade in place
                if dependent_reg_top.is_at_or_above(dest) {
                    // Check if we can pin it
                    if self.can_pin_view(view.index) {
                        if let &PinMode::None = &results[dest.0 as usize] {
                            if passive {
                                trace!("Ignoring expression {:?}, ({:?})", view.index, dest);
                                results[dest.0 as usize] = PinMode::Ignored;
                            } else {
                                trace!("Pinning expression {:?} ({:?}) in place", view.index, dest);
                                results[dest.0 as usize] = PinMode::InPlace(view.index);
                            }
                        }
                    } else {
                        trace!("Skipping expression {:?} because it can't be pinned", view.index);
                    }
                }
            } else if let &ViewType::SelfExpression { base } = &view.view_type {
                for i in 0..2u8 {
                    let dest = Reg(base.0 + i);
                    if dependent_reg_top.is_at_or_above(dest) {
                        if let &PinMode::None = &results[dest.0 as usize] {
                            trace!("Already pinned at {:?} ({:?}) because Self", view.index, dest);
                            results[dest.0 as usize] = PinMode::Pinned;
                        }
                    }
                }
            } else if let &ViewType::PinnedExpression { dest } = &view.view_type {
                if dependent_reg_top.is_at_or_above(dest) {
                    if let &PinMode::None = &results[dest.0 as usize] {
                        trace!("Already pinned at {:?} ({:?})", view.index, dest);
                        results[dest.0 as usize] = PinMode::Pinned;
                    }
                }
            } else if let &ViewType::MultiExpression { base, count } = &view.view_type {
                if dependent_reg_top.is_at_or_above(base) {
                    let mut counter = match count.count() {
                        Some(n) => n as u8,
                        None => panic!("Unreachable!")
                    };
                    let mut top = Reg(base.0 + (counter - 1));
                    while counter > 0 {
                        if dependent_reg_top.is_at_or_above(top) {
                            if let &PinMode::None = &results[top.0 as usize] {
                                trace!("Already pinned at {:?} ({:?})", view.index, top);
                                results[top.0 as usize] = PinMode::Pinned;
                            }
                        }
                        counter -= 1;
                        if counter > 0 {
                            top = top.previous().unwrap();
                        }
                    }
                }
            } else if let &ViewType::ForPrep { base, .. } = &view.view_type {
                for i in 0..4u8 {
                    let dest = Reg(base.0 + i);
                    if dependent_reg_top.is_at_or_above(dest) {
                        if let &PinMode::None = &results[dest.0 as usize] {
                            trace!("Already pinned at {:?} ({:?}) because ForPrep", view.index, dest);
                            results[dest.0 as usize] = PinMode::Pinned;
                        }
                    }
                }
            } else if let ViewType::TForPrep { base, count, .. } = view.view_type {
                for i in 0..(3 + count as u8 + 1) {
                    let dest = Reg(base.0 + i);
                    if dependent_reg_top.is_at_or_above(dest) {
                        if let &PinMode::None = &results[dest.0 as usize] {
                            trace!("Already pinned at {:?} ({:?}) because TForPrep", view.index, dest);
                            results[dest.0 as usize] = PinMode::Pinned;
                        }
                    }
                }
            } else if let ViewType::Statement { assignment: Some(ref assignment), .. } = view.view_type {
                match assignment.lhs {
                    AssignmentLHS::Partial(ref lhs) => {
                        for lhs in lhs {
                            if let &PartialAssignmentLHS::Local(dest) = lhs {
                                if dependent_reg_top.is_at_or_above(dest) {
                                    if let &PinMode::None = &results[dest.0 as usize] {
                                        trace!("Already pinned at {:?} ({:?}) because Assignment", view.index, dest);
                                        results[dest.0 as usize] = PinMode::Pinned;
                                    }
                                }
                            }
                        }
                    },
                    AssignmentLHS::Full(ref lhs) => {
                        for lhs in lhs {
                            if let &FullAssignmentLHS::Local(dest) = lhs {
                                if dependent_reg_top.is_at_or_above(dest) {
                                    if let &PinMode::None = &results[dest.0 as usize] {
                                        trace!("Already pinned at {:?} ({:?}) because Assignment", view.index, dest);
                                        results[dest.0 as usize] = PinMode::Pinned;
                                    }
                                }
                            }
                        }
                    },
                }
            }
            // TODO: Assignments

            // Check dependencies for anything depending on our dependent registers
            // TODO: This comment might be incorrect:
            // TODO: Skip doing this within certain constructs, otherwise they'll break them. This can cause compound conditional expressions to decompose, which yields incorrect code. Compound conditional expressions are a concrete construct that cannot be separated once created. They should be treated like scopes: they cannot be looked inside of.
            // We need to add information telling this part that the sub expressions are concrete and cannot be pinned. We can possibly use a ViewTag for this (NotPinnable).
            // Essentially, if we see a ViewType::Expression that isn't pinned, we check the ViewTag for NotPinnable. If it is not pinnable then we skip it (like it doesn't exist).
            for dep in self.views_between(view.top..view.index).iter().rev() {
                trace!("  sub: {:?}", dep.index);
                if let &ViewType::Expression { dest, .. } = &dep.view_type {
                    if dependent_reg_top.is_at_or_above(dest) {
                        if self.can_pin_view(dep.index) {
                            if let &PinMode::None = &results[dest.0 as usize] {
                                trace!("Pinning expression {:?} ({:?}) and backtracking", view.index, dest);
                                results[dest.0 as usize] = PinMode::Backtrack(dep.index);
                            }
                        } else {
                            trace!("Skipping expression {:?} because it can't be pinned", view.index);
                        }
                    }
                }
            }
        }

        trace!("{:?}", results);

        let mut bail_location: Option<ViewRef> = None;
        for (i, result) in results.iter().enumerate() {
            match result {
                &PinMode::None => {
                    // TODO: Valid code can trigger this panic, we need to ensure that we are the first instruction (e.g. views are empty)
                    // These are implicity nil'd locals

                    if self.views.iter().all(|view| {
                        let free_mark = self.view_free_mark_at(view.index);
                        free_mark.incoming.is_free(Reg(i as u8))
                    }) {
                        let start = Reg(i as u8);
                        let end = Reg((results.len() - 1) as u8);
                        return Err(ViewBail::ImplicitNil { start, end });
                    } else {
                        for view in &self.views {
                            warn!("{:?}", view)
                        }
                        panic!("Could not find r{} in scope (\nresults: {:?},\ntags: {:?},\ndecoded: {:?},\ndependent reg top: {:?},\nview dump:\n{})", i, results, self.view_tags, self.decoded, dependent_reg_top, self.debug_view_dump())
                    }
                },
                &PinMode::Pinned => (), // Nothing to do
                &PinMode::InPlace(view) => {
                    // TODO: Panic if passive?
                    self.view_at_mut(view).view_type = ViewType::PinnedExpression {
                        dest: Reg(i as u8)
                    };
                    let key = self.view_at(view).key;
                    self.tag_view_instr(view, ViewTag::Pinned { view_key: key });
                },
                &PinMode::Backtrack(view) => {
                    // TODO: Panic if passive?
                    self.view_at_mut(view).view_type = ViewType::PinnedExpression {
                        dest: Reg(i as u8)
                    };
                    let key = self.view_at(view).key;
                    self.tag_view_instr(view, ViewTag::Pinned { view_key: key });
                    bail_location = bail_location
                        .map(|l| if l < view { l } else { view })
                        .or(Some(view));
                },
                &PinMode::Ignored => (),
            }
        }

        if let Some(bail_location) = bail_location {
            Err(ViewBail::PinExpression { view: bail_location })
        } else {
            Ok(())
        }
    }

    /// Adds a view to the context, and validates it.
    /// 
    /// If validation fails an Err(ViewBail) will be returned, which has more details.
    fn add_view(&mut self, view: View) -> Result<(), ViewBail> {
        trace!("Adding view {:?}", view);
        
        // Verify dependent registers
        // Everything from dependent_reg_top downwards can't be used as temporaries/need to be pinned
        // To do this, we iterate raw views, and skip the insides of scopes
        // If we run into an expression that isn't pinned, we simply tag it as pinned
        // However, if the expression we ran into has been taken by another view, we have to tag it and
        // throw out work until that point.
        let view_top = view.top;
        if let Some(dependent_reg_top) = view.dependent_reg_top {
            self.verify_dependent_registers(dependent_reg_top, view.passive_dependent_regs, view_top)?;
        }

        let incoming_free_mark = self.free_mark.clone();

        // Free dependent views
        let last_dependent_view = self.iter_root()
            .take(view.dependent_view_count as usize)
            .map(|v| v.index)
            .last();
        if let Some(last_dependent_view) = last_dependent_view {
            self.free_mark = self.view_free_mark_at(last_dependent_view).incoming_root.clone();
        }

        let root_incoming_free_mark = self.free_mark.clone();

        if let &Some(ref allocations) = &view.allocations {
            let mut reg = allocations.start;
            loop {
                if !self.free_mark.is_allocated(reg) && !self.free_mark.is_next_free(reg) {
                    // Oh boy, lets try to fix everything!
                    self.verify_dependent_registers(reg.previous().unwrap(), true, view_top)?;
                }
                self.free_mark = self.free_mark.allocate_single(reg);
                reg = reg.next();
                if reg > allocations.end {
                    break
                }
            }
        }

        // TODO: Base register agreement should be checked in a separate pass to prevent issues
        // Since this is a statement level transform, we don't want to accidentially interfere with expressions
        // Maybe we just have to make sure everything is pinned?
        // Shouldn't everything already be pinned?
        // Not sure if you can have a free mark v base register conflict inside of an expression since an expression
        // context grows the top linearly... and expressions within another expression can't just simply go unused.
        // if view.view_type.is_statement_level() {
        //     // We can only implicitly close statements
        //     if let Some(base) = view.base {
        //         if !root_incoming_free_mark.is_next_free(base) {
        //             // Will bail if successful
        //             self.try_close(base, Some(view.dependent_view_count), true)?
        //         }
        //     }
        // }

        self.views.push(view);
        self.view_free_mark.push(ViewFreeMarkInfo {
            incoming: incoming_free_mark,
            incoming_root: root_incoming_free_mark,
            outgoing: self.free_mark.clone()
        });
        trace!("{:?}", self.view_free_mark.last());

        Ok(())
    }

    fn build_expression<'a, T: 'static + ViewData>(&'a self, builder: ViewBuilder<'a>, view_data: T, dest: Reg, key: ViewKeyRef) -> View {
        self.build_expression_with_info(builder, view_data, dest, ExpressionInfo::None, key)
    }

    fn build_expression_with_info<'a, T: 'static + ViewData>(&'a self, builder: ViewBuilder<'a>, view_data: T, dest: Reg, info: ExpressionInfo, key: ViewKeyRef) -> View {
        let view_type = {
            match self.current_tag() {
                Some(ViewTag::Pinned { view_key }) if *view_key == key => ViewType::PinnedExpression { dest },
                _ => ViewType::Expression { dest, info }
            }
        };
        
        builder.build(
            Box::new(view_data),
            view_type,
            key
        )
    }

    /// Follows a jump instruction.
    /// 
    /// If the pc is out of range, 0xFFFFFFFF is returned.
    /// Otherwise, if the instruction is a jump and the jump isn't being skipped by the previous instruction the
    /// jump's destination is returned, otherwise pc is returned (no jump to follow).
    fn follow_jump(&self, pc: u32) -> u32 {
        if (pc as usize) >= self.decoded.len() {
            0xFFFFFFFFu32
        } else if let &LuaInstruction::Jump { offset } = &self.decoded[pc as usize] {
            if let &SkipInfo::None = &self.instr_ex[pc as usize].skipped_by_previous {
                offset.apply(pc)
            } else {
                pc
            }
        } else {
            pc
        }
    }

    /// Creates and adds a scope from the bottom of the views to the given view ref.
    /// 
    /// Either returns nothing or a bail result.
    fn make_scope_after(&mut self, view: ViewRef, key: ViewKeyRef) -> Result<(), ViewBail> {
        let end_view = self.end_view();
        let all_ok = self.try_finalize((view + 1)..end_view)?;
        if !all_ok {
            eprintln!("Can't create a scope around views that aren't at statement level");
        }
        let view = {
            let mut builder = self.builder(None);
            let statements = builder.take_views_until(|v| v.index <= view);
            builder.build(
                Box::new(ScopeViewData { statements }),
                ViewType::Scope,
                key
            )
        };
        self.add_view(view)
    }

    /// Creates a scope like [make_scope_after], but instead of being exclusive its inclusive.
    fn make_scope_including(&mut self, view: ViewRef, key: ViewKeyRef) -> Result<(), ViewBail> {
        let end_view = self.end_view();
        let all_ok = self.try_finalize(view..end_view)?;
        if !all_ok {
            eprintln!("Can't create a scope around views that aren't at statement level");
        }
        let view = {
            let mut builder = self.builder(None);
            let statements = builder.take_views_until(|v| v.index < view);
            builder.build(
                Box::new(ScopeViewData { statements }),
                ViewType::Scope,
                key
            )
        };
        self.add_view(view)
    }

    /// Simulates a conditional join, returns the top root_view of the conditional
    fn simulate_conditional(&self, root: ViewRef, on_true: u32, on_false: u32) -> (ViewRef, CondContext, Vec<ViewRef>) {
        let mut context = {
            let view = self.view_at(root);

            if let &ViewType::Conditional { exit, .. } = &view.view_type {
                let target = CondTarget::new(view.instructions.end, view.instructions.end + 1, exit);
                CondContext::new(target, on_true, on_false)
            } else {
                panic!("Could not simulate taking a conditional: Root view is not a conditional")
            }
        };
        let mut out = vec![root];
        let root_top: ViewRef = self.view_at(root).top.into();
        let result = if root_top.is_not_first() {
            let mut last_conditional = root_top;
            let mut current = root_top - 1;
            loop {
                let view = self.view_at(current);

                if let &ViewType::Conditional { exit, .. } = &view.view_type {
                    let target = CondTarget::new(view.instructions.end, view.instructions.end + 1, exit);
                    if let Some(ref op) = context.try_insert(&target) {
                        context.apply(&op, target);
                        out.push(current);
                        last_conditional = current;
                        
                        let top: ViewRef = view.top.into();
                        if top.is_first() {
                            break;
                        } else {
                            current = top - 1;
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            last_conditional
        } else {
            root
        };

        (result, context, out)
    }

    fn try_close(&mut self, base: Reg, dependent_view_count: Option<u32>, retry: bool) -> Result<(), ViewBail> {
        let possible_fix = self.iter_root()
            .skip(dependent_view_count.unwrap_or(0) as usize)
            .map(|view| (view.index, &self.view_free_mark_at(view.index).incoming_root))
            .filter(|(_, incoming_root)| incoming_root.is_next_free(base))
            .next()
            .map(|(index, free_mark)| (index, free_mark.clone()));

        let top_view = dependent_view_count.and_then(|dvc| self.iter_root().skip(dvc as usize - 1).next().map(|view| view.index));

        if let Some((view_index, _)) = possible_fix {
            // All root views from top_view-1 to possible_fix need to be statement level, so check them
            let range_end = top_view.unwrap_or(self.end_view());
            let all_ok = self.try_finalize(view_index..range_end)?;

            if !all_ok {
                eprintln!("Can't create a scope around views that aren't at statement level");
                return Ok(())
            }

            Err(ViewBail::ScopeWrap { view: view_index, rewind_view: top_view, retry })
        } else {
            println!("Possible fix not found!");
            Ok(())
        }
    }

    fn try_finalize(&mut self, range: Range<ViewRef>) -> Result<bool, ViewBail> {
        // Check if there are any views to finalize, also check if there are non statement level views
        let mut has_finalizations = false;
        for view in self.iter_root() {
            if view.index < range.start { break }
            if view.index >= range.end { continue }
            if view.view_type.needs_finalization() {
                has_finalizations = true;
            } else if !view.view_type.is_statement_level() {
                println!("Non-statement view type: {:?}", view.view_type);
                return Ok(false) // Non-statement level view, can't finalize range
            }
        }

        if has_finalizations {
            let mut last_exit = self.decoded.len() as u32;
            let mut repeat_until_index = 0;
            let mut bail = None;
            let mut following_base: Option<Reg> = None;

            let mut tags = Vec::new();

            for view in self.iter_root() {
                // println!("{:?} {:?} {:?}", self.view_free_mark_at(view.index), view.view_type, view.base);
                // TODO: Check if following views have a base that isn't allowed for a repeat until
                match view.view_type {
                    ViewType::RepeatUntilMarker { exit } => {
                        // println!("{:?} {:?}", last_exit, exit);
                        let top_index = self.iter_root().filter(|view| view.instructions.end == exit).next().unwrap().index;
                        let top_free_mark = self.view_free_mark_at(top_index);
                        // println!("Free marks: {:?} {:?}", top_free_mark, following_base);
                        let free_mark_ok = match following_base {
                            Some(base) => top_free_mark.outgoing.is_next_free(base),
                            None => true,
                        };
                        if last_exit != exit && free_mark_ok {
                            last_exit = exit;
                            repeat_until_index += 1;

                            // Tag as Tail
                            tags.push((view.index - 1, ViewTag::RepeatUntilTail))
                        } else {
                            // Tag as Leaf
                            tags.push((view.index - 1, ViewTag::RepeatUntilLeaf))
                        }
                        bail = Some(view.index);
                    },
                    _ => {},
                }

                if let Some(base) = view.base {
                    if let Some(ref mut following_base) = following_base {
                        if following_base.is_above(base) {
                            *following_base = base;
                        }
                    } else {
                        following_base = Some(base);
                    }
                }
            }

            for (index, tag) in tags.into_iter() {
                // println!("{:?} {:?}", index, tag);
                self.tag_view_instr(index, tag);
            }

            if let Some(bail) = bail {
                return Err(ViewBail::PinExpression {
                    view: bail
                })
            }
        }

        Ok(true)
    }

    fn debug_view_dump(&self) -> String {
        let mut views = Vec::new();
        let mut indent = String::from("");

        fn dump_recursive<'a>(iter: &mut impl Iterator<Item=&'a View>, output: &mut Vec<String>, indent: &mut String) -> bool {
            let view = iter.next();
            if let Some(view) = view {
                // Emit header
                if view.dependent_view_count != 0 {
                    output.push(format!("{}}}\n", indent));
                    indent.push_str("  ");
                    for _ in 0..view.dependent_view_count {
                        dump_recursive(iter, output, indent);
                    }
                    let i = indent.len();
                    indent.truncate(i - 2);
                }
                output.push(format!("{}{:?}{}\n", indent, view, if view.dependent_view_count != 0 { " {" } else { "" }));
                true
            } else {
                false
            }
        }
        let mut iter = self.views.iter().rev();
        while dump_recursive(&mut iter, &mut views, &mut indent) {}
        
        views.reverse();
        views.join("")
    }

    /// Processes a single instruction at pc.
    /// 
    /// May return a ViewBail if something happened while trying to add the view.
    fn process_instruction(&mut self, pc: u32) -> Result<(), ViewBail> {
        self.pc = pc;
        let last_instr = pc as usize == (self.decoded.len() - 1);

        #[cfg(debug_assertions)]
        println!("Process instruction {} {:?}", pc, &self.decoded[pc as usize]);

        match self.decoded[pc as usize] {
            // Not allowed to utilize ralloc feedback from the following instructions:
            LuaInstruction::Jump {..} => {}
            _ => {
                if let Some(ralloc_incoming) = self.ralloc.incoming_free_mark_at(pc as usize) {
                    if !self.free_mark().is_next_free(ralloc_incoming) {
                        println!("Ralloc mismatch! {:?} {:?}", self.free_mark(), ralloc_incoming);
                        self.try_close(ralloc_incoming, None, true)?;
                        // Fail, set the free mark manually
                        // self.free_mark = FreeMark::new(ralloc_incoming.0);
                    }
                }
            }
        }

        // let instr_info = self.decoded[pc as usize].info();

        // match instr_info {
        //     Some(InstructionInfo::Stack { base, inputs, outputs }) => {
        //         if !self.free_mark().is_next_allocated((base + inputs).unwrap()) {
        //             // Look at previous instructions to see what we need
        //             // If inputs is not empty, look for the first input (at base) (a decouple'd setter)
        //             // let top = &self.decoded[..pc as usize].iter().rev()
        //             //     .skip_while(|instr| !instr.does_use(|reg| {
        //             //         println!("{:?} {}", reg, base.is_above(reg));
        //             //         base.is_above(reg)
        //             //     })).next();

        //             panic!("Free mark doesn't match instruction base at {} {:?}", pc, top);
        //         }
        //     },
        //     None => {},
        // }

        if !self.instr_ex[pc as usize].jump_sources.is_empty() {
            let view = {
                let mut builder = self.builder(None);
                builder.build(
                    Box::new(BackwardsJumpBarrierViewData),
                    ViewType::Statement { assignment: None },
                    make_view_key!(name: "backwards jump barrier", desc: "barrier emitted at backwards jump destinations to prevent inlines")
                )
            };
            self.add_view(view)?;
        }

        let view: Option<View> = {
            match self.decoded[pc as usize] {
                LuaInstruction::Move { dest, source } => {
                    if source.is_above(dest) {
                        // Try to attach to assignment, since this isn't a well formed temporary move
                        Some({
                            let mut builder = self.builder(None);
                            if let TakeAssignmentResult::Assignment(assignment_info) = builder.take_for_assignment(RK::R(source).into(), PartialAssignmentLHS::Local(dest)) {
                                builder.build(
                                    // TODO: Give the ViewType to the debug print fn instead of this
                                    Box::new(MoveAssignViewData(assignment_info.clone())),
                                    ViewType::assignment(assignment_info),
                                    make_view_key!(name: "move assignment", desc: "attached to an assignment because the move isn't a well formed temporary move, may be part of a compound assignment")
                                )
                            } else {
                                unreachable!()
                            }
                        })
                    } else {
                        // Temporary move
                        Some({
                            let mut builder = self.builder(Some(dest));
                            builder.allocate_single(dest);
                            builder.depend_on_register(source);
                            self.build_expression(
                                builder,
                                MoveViewData { source },
                                dest,
                                make_view_key!(name: "temporary move", desc: "temporary move from one register to another")
                            )
                        })
                    }
                },
                LuaInstruction::GetGlobal { dest, index } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            GetGlobalViewData { index },
                            dest,
                            make_view_key!(name: "get global", desc: "gets a global value")
                        )
                    })
                },
                LuaInstruction::SetGlobal { index, source } => {
                    Some({
                        let mut builder = self.builder(None);
                        if let TakeAssignmentResult::Assignment(assignment_info) = builder.take_for_assignment(RK::R(source).into(), PartialAssignmentLHS::Global(index)) {
                            builder.build(
                                // TODO: Give the ViewType to the debug print fn instead of this
                                Box::new(SetGlobalViewData(assignment_info.clone())),
                                ViewType::assignment(assignment_info),
                                make_view_key!(name: "set global", desc: "sets a global value, may be part of a compound assignment")
                            )
                        } else {
                            unreachable!()
                        }
                    })
                },
                LuaInstruction::GetUpvalue { dest, upvalue } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            GetUpvalueViewData { upvalue },
                            dest,
                            make_view_key!(name: "get upvalue", desc: "gets an upvalue")
                        )
                    })
                },
                LuaInstruction::SetUpvalue { upvalue, source } => {
                    Some({
                        let mut builder = self.builder(None);
                        if let TakeAssignmentResult::Assignment(assignment_info) = builder.take_for_assignment(RK::R(source).into(), PartialAssignmentLHS::Upvalue(upvalue)) {
                            builder.build(
                                // TODO: Give the ViewType to the debug print fn instead of this
                                Box::new(SetGlobalViewData(assignment_info.clone())),
                                ViewType::assignment(assignment_info),
                                make_view_key!(name: "set upvalue", desc: "sets an upvalue, may be part of a compound assignment")
                            )
                        } else {
                            unreachable!()
                        }
                    })
                },
                LuaInstruction::LoadK { dest, kst } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            LoadKViewData { kst: kst },
                            dest,
                            make_view_key!(name: "load constant", desc: "loads a constant from the constant pool")
                        )
                    })
                },
                LuaInstruction::LoadNil { dest_start, dest_end } => {
                    let key = make_view_key!(name: "load nil", desc: "loads one or more nils into consecutive registers");

                    let count = dest_end.0 - dest_start.0 + 1;
                    let view_type = if dest_start == dest_end {
                        None
                    } else {
                        Some(ViewType::MultiExpression { base: dest_start, count: Count::from((dest_end.0 - dest_start.0 + 1).into()) })
                    };

                    Some({
                        let mut builder = self.builder(Some(dest_start));
                        builder.allocate(dest_start..dest_end);
                        if let Some(view_type) = view_type {
                            builder.build(
                                Box::new(LoadNilViewData(count)),
                                view_type,
                                key
                            )
                        } else {
                            self.build_expression(
                                builder,
                                LoadNilViewData(count),
                                dest_start,
                                key
                            )
                        }
                    })
                },
                LuaInstruction::LoadBool { dest, value, skip_next: false } => {
                    let prev = if pc != 0 {
                        Some(&self.decoded[(pc - 1) as usize])
                    } else {
                        None
                    };

                    if let Some(LuaInstruction::LoadBool { skip_next: true, .. }) = prev {
                        None
                    } else {
                        Some({
                            let mut builder = self.builder(Some(dest));
                            builder.allocate_single(dest);
                            self.build_expression(
                                builder,
                                LoadBoolViewData(value),
                                dest,
                                make_view_key!(name: "load bool", desc: "loads a simple boolean without any extra associated behaviors")
                            )
                        })
                    }
                },
                LuaInstruction::LoadBool { dest, skip_next: true, .. } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        let tail = builder.take_loadbool_head(dest);
                        let has_tail = tail.is_some();
                        builder.allocate_single(dest);
                        builder.build(
                            Box::new(SkippingLoadBoolViewData(tail)),
                            ViewType::SkippingLoadBool {
                                dest,
                                has_tail
                            },
                            make_view_key!(name: "skipping load bool", desc: "a loadbool that skips over the next instruction, which is another loadbool")
                        )
                    })
                },
                LuaInstruction::GetTable { dest, table, index } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        let index_view = builder.take_reg_or_kst(index);
                        let table_view = builder.take_reg(table);
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            GetTableViewData { table: table_view, index: index_view },
                            dest,
                            make_view_key!(name: "get table", desc: "indexes a value from a table")
                        )
                    })
                },
                LuaInstruction::SetTable { table, index, source } => {
                    Some({
                        let mut builder = self.builder(None);
                        match builder.take_for_assignment(source.into(), PartialAssignmentLHS::Table { table, index }) {
                            TakeAssignmentResult::Assignment(assignment_info) => {
                                builder.build(
                                    // TODO: Give the ViewType to the debug print fn instead of this
                                    Box::new(SetTableViewData(assignment_info.clone())),
                                    ViewType::assignment(assignment_info),
                                    make_view_key!(name: "assignment set table", desc: "sets a value inside of a table, may be part of a compound assignment")
                                )
                            },
                            TakeAssignmentResult::TableInline(table_reg, table_info) => {
                                builder.allocate_single(table_reg);
                                self.build_expression_with_info(
                                    builder,
                                    TableViewData(table_info.clone()),
                                    table_reg,
                                    ExpressionInfo::Table(table_info),
                                    make_view_key!(name: "table constructor with hash key", desc: "table constructor with a hash key value")
                                )
                            }
                        }
                    })
                },
                LuaInstruction::BinOp { dest, lhs, op, rhs } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        let taken_rhs = builder.take_reg_or_kst(rhs);
                        let taken_lhs = builder.take_reg_or_kst(lhs);
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            BinOpViewData { lhs: taken_lhs, op, rhs: taken_rhs },
                            dest,
                            make_view_key!(name: "binary op", desc: "binary operation between two values")
                        )
                    })
                },
                LuaInstruction::UnOp { dest, op, rhs } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        let taken_rhs = builder.take_reg(rhs);
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            UnOpViewData { op: op, rhs: taken_rhs },
                            dest,
                            make_view_key!(name: "unary op", desc: "unary operation on a single value")
                        )
                    })
                },
                LuaInstruction::Test { source, inverted } => {
                    Some({
                        let mut builder = self.builder(None);
                        let taken = builder.take_reg(source);
                        builder.build(
                            Box::new(TestViewData { source: taken, inverted }),
                            ViewType::PartialConditional(PartialCondInfo::Test {
                                inlined_source: if let ViewOrReg::View(..) = taken {
                                    true
                                } else {
                                    false
                                },
                                source_reg: source
                            }),
                            make_view_key!(name: "conditional test", desc: "partially built conditional testing a value for truthiness")
                        )
                    })
                },
                LuaInstruction::TestSet { dest, source, inverted } => {
                    Some({
                        let mut builder = self.builder(None);
                        let taken = builder.take_reg(source);
                        builder.build(
                            Box::new(TestSetViewData { source: taken, inverted }),
                            ViewType::PartialConditional(PartialCondInfo::TestSet { dest_reg: dest }),
                            make_view_key!(name: "conditional test and set", desc: "partially built conditional testing a value for truthiness, if true sets the destination to the value")
                        )
                    })
                },
                LuaInstruction::BinCondOp { inverted, lhs, op, rhs } => {
                    let flipped = match (lhs, rhs) {
                        (RK::R(lhs), RK::R(rhs)) => lhs.is_above(rhs),
                        _ => false
                    };

                    Some({
                        let mut builder = self.builder(None);
                        let mut taken_rhs = builder.take_reg_or_kst(if flipped { lhs } else { rhs });
                        let mut taken_lhs = builder.take_reg_or_kst(if flipped { rhs } else { lhs });
                        builder.build(
                            Box::new(BinCondOpViewData { lhs: taken_lhs, op, rhs: taken_rhs, inverted, flipped }),
                            ViewType::PartialConditional(PartialCondInfo::BinCondOp),
                            make_view_key!(name: "conditional binary op", desc: "partially built conditional comparing two values")
                        )
                    })
                },
                LuaInstruction::Jump { .. } => {
                    let ex = &self.instr_ex[pc as usize];
                    let dest = ex.jump_to.unwrap();
                    if let SkipInfo::Conditional { .. } = ex.skipped_by_previous {
                        Some({
                            let mut builder = self.builder(None);
                            let (cond, info) = builder.take_partial_conditional();
                            builder.build(
                                Box::new(ConditionalJumpViewData { cond }),
                                ViewType::Conditional { exit: dest, info },
                                make_view_key!(name: "conditional", desc: "a destination for the preceeding partial conditional")
                            )
                        })
                    } else {
                        if let LuaInstruction::TForLoop { base, count } = self.decoded[dest as usize] {
                            if let LuaInstruction::Jump { offset: tfor_jump_offset } = self.decoded[(dest + 1) as usize] {
                                if tfor_jump_offset.apply(dest + 1) == pc + 1 {
                                    Some({
                                        let mut builder = self.builder(Some(base));

                                        let values = builder.take_generic_for_params(base);

                                        builder.allocate(base..Reg(base.0 + 3 + (count as u8 - 1)));

                                        builder.build(
                                            Box::new(TForPrepViewData { values, base, count }),
                                            ViewType::TForPrep { base, count, end: dest + 2 },
                                            make_view_key!(name: "generic for loop prep", desc: "preparation for a generic for loop")
                                        )
                                    })
                                } else {
                                    None
                                }
                            } else {
                                panic!("Invalid TFORLOOP")
                            }
                        } else {
                            None
                        }
                    }
                },
                LuaInstruction::Call { base, args: arg_count, ret: ret_count } | LuaInstruction::TailCall { base, args: arg_count, ret: ret_count } => {
                    let view_type = match ret_count.count() {
                        Some(0) => Some(ViewType::simple_statement()),
                        Some(1) => None,
                        Some(_) => Some(ViewType::MultiExpression { base, count: ret_count }),
                        None => Some(ViewType::VarArgsExpression { base }),
                    };
                    
                    Some({
                        let mut builder = self.builder(Some(base));
                        builder.base(base);
                        let mut args = vec![];
                        let mut top = match arg_count.count() {
                            Some(_) => (base + arg_count).unwrap(),
                            None => {
                                let (view, base) = builder.take_varargs();
                                args.push(view);
                                base.previous().unwrap()
                            }
                        };
                        while top.is_above(base) {
                            if top == base.next() && builder.is_next_view_self(base) {
                                // Self call, don't try to take the first argument
                                break
                            }
                            let (new_top, view) = builder.take_multi_return_value(top);
                            top = new_top;
                            args.push(view);
                            top = top.previous().unwrap();
                        }
                        let func = builder.take_call_func(base);
                        if !(ret_count.is_empty() || ret_count.is_varargs()) {
                            builder.allocate(base..(base + ret_count).unwrap().previous().unwrap());
                        }
                        let key = make_view_key!(name: "call", desc: "a function call with varying amounts of parameters and return results");
                        if let Some(view_type) = view_type {
                            builder.build(
                                Box::new(CallViewData { func, args }),
                                view_type,
                                key
                            )
                        } else {
                            self.build_expression(
                                builder,
                                CallViewData { func, args },
                                base,
                                key
                            )
                        }
                    })
                },
                LuaInstruction::Self_ { base, object, method } => {
                    if let RK::R(_) = method {
                        panic!("Self instruction cannot use a register as the method in a well formed program");
                    }

                    Some({
                        let mut builder = self.builder(Some(base));
                        let object = if object == base {
                            ViewOrReg::View(builder.take_reg_strong(object))
                        } else {
                            builder.take_reg(object)
                        };
                        let method = if let RK::K(constant) = method {
                            constant
                        } else {
                            unreachable!()
                        };
                        builder.base(base);
                        builder.allocate(base..(base.next()));
                        builder.build(
                            Box::new(SelfViewData { object, method }),
                            ViewType::SelfExpression { base },
                            make_view_key!(name: "self call setup", desc: "setup for a self call")
                        )
                    })
                },
                LuaInstruction::Return { base, count } => {
                    if !last_instr {
                        Some({
                            let mut builder = self.builder(Some(base));
                            let view_data = if count.is_empty() {
                                ReturnViewData::None
                            } else if count == Count::from(1) {
                                let single = builder.take_reg(base);
                                match single {
                                    ViewOrReg::Reg(reg) => ReturnViewData::SingleReg(reg),
                                    ViewOrReg::View(view) => ReturnViewData::Views(vec![view])
                                }
                            } else {
                                let mut views = vec![];
                                let mut top = match count.count() {
                                    Some(_) => (base + count).unwrap(),
                                    None => {
                                        let (view, base) = builder.take_varargs();
                                        views.push(view);
                                        base
                                    }
                                };
                                while top.is_above(base) {
                                    top = top.previous().unwrap();
                                    let (new_top, view) = builder.take_multi_return_value(top);
                                    top = new_top;
                                    views.push(view);
                                }
                                ReturnViewData::Views(views)
                            };
                            builder.build(
                                Box::new(view_data),
                                ViewType::simple_statement(),
                                make_view_key!(name: "return", desc: "exits from the function returning 0 or more values")
                            )
                        })
                    } else {
                        None
                    }
                },
                LuaInstruction::NewTable { dest, array_count, hash_count } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        builder.allocate_single(dest);
                        self.build_expression_with_info(
                            builder,
                            NewTableViewData,
                            dest,
                            ExpressionInfo::Table(TableInfo {
                                remaining_array: array_count as usize,
                                remaining_hash: hash_count as usize,
                                entries: vec![]
                            }),
                            make_view_key!(name: "new table", desc: "creates a new table with the possible preallocation")
                        )
                    })
                },
                LuaInstruction::SetList { base, count, .. } => {
                    Some({
                        let mut builder = self.builder(Some(base));
                        let table_info = builder.take_setlist(base, count);
                        builder.allocate_single(base);
                        self.build_expression_with_info(
                            builder,
                            TableViewData(table_info.clone()),
                            base,
                            ExpressionInfo::Table(table_info),
                            make_view_key!(name: "set list", desc: "copies array based values into the table")
                        )
                    })
                },
                LuaInstruction::Closure { dest, prototype, ref captures } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        for cap in captures {
                            if let &ClosureCapture::Register(reg) = cap {
                                builder.depend_on_register(reg);
                            }
                        }
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            PrototypeViewData(prototype, Vec::from(&captures[..])),
                            dest,
                            make_view_key!(name: "closure", desc: "creates a closure from the given prototype using the configured upvalues")
                        )
                    })
                },
                LuaInstruction::Close { base } => {
                    // We try to hit the implicit close path
                    Some({
                        let mut builder = self.builder(Some(base));
                        builder.base(base);
                        builder.build(
                            Box::new(CloseViewData { base }),
                            ViewType::Statement { assignment: None },
                            make_view_key!(name: "close", desc: "closes local upvalues from a previous closure capture")
                        )
                    })
                },
                LuaInstruction::ForPrep { base, target } => {
                    Some({
                        let mut builder = self.builder(Some(base));
                        let step = builder.take_reg_strong(Reg(base.0 + 2));
                        let limit = builder.take_reg_strong(Reg(base.0 + 1));
                        let init = builder.take_reg_strong(base);
                        builder.allocate(base..(Reg(base.0 + 3)));
                        builder.build(
                            Box::new(ForPrepViewData { base, init, limit, step }),
                            ViewType::ForPrep { base, end: target.apply(pc) },
                            make_view_key!(name: "numeric for prep", desc: "prepares a numeric for loop")
                        )
                    })
                },
                LuaInstruction::ForLoop { .. } => None,
                LuaInstruction::TForLoop { .. } => None,
                LuaInstruction::Concat { dest, source_start, source_end } => {
                    Some({
                        let mut builder = self.builder(Some(dest));
                        let mut views = Vec::with_capacity((source_end.0 - source_start.0 + 1) as usize);
                        let mut current = source_end;
                        loop {
                            views.push(builder.take_reg_strong(current));
                            if current == source_start {
                                break
                            } else {
                                current = current.previous().unwrap();
                            }
                        }
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            ConcatViewData(views),
                            dest,
                            make_view_key!(name: "concat", desc: "concatenates consecutive values")
                        )
                    })
                },
                LuaInstruction::VarArg { base, count } => {
                    let view_type = match count.count() {
                        Some(0) => unreachable!(),
                        Some(1) => None,
                        Some(_) => Some(ViewType::MultiExpression { base, count: count }),
                        None => Some(ViewType::VarArgsExpression { base }),
                    };
                    Some({
                        let mut builder = self.builder(Some(base));
                        builder.base(base);
                        if !(count.is_empty() || count.is_varargs()) {
                            builder.allocate(base..(base + count).unwrap().previous().unwrap());
                        }
                        let key = make_view_key!(name: "var args", desc: "unboxes one or more values from variadic storage");
                        if let Some(view_type) = view_type {
                            builder.build(
                                Box::new(VarArgsViewData),
                                view_type,
                                key
                            )
                        } else {
                            self.build_expression(
                                builder,
                                VarArgsViewData,
                                base,
                                key
                            )
                        }
                    })
                },
                LuaInstruction::SemanticNoOp => None,
                ref instr @ _ => panic!("Unimplemented instruction: {:?}", instr),
            }
        };

        let has_view = view.is_some();

        if let Some(v) = view {
            self.add_view(v)?;
        }

        let mut used_jump_instr = has_view || if let &LuaInstruction::Jump { .. } = &self.decoded[pc as usize] {
            false
        } else {
            true
        };

        enum JumpUsage {
            None,
            MakeBreak
        }

        let jump_usage = 'used_jump: loop {
            if !used_jump_instr {
                if let &LuaInstruction::Jump { offset } = &self.decoded[pc as usize] {
                    // Search to see if its breaking a loop.
                    // We do this by looking for the nearest loop end and checking if we jump to the end + 1 (followed)

                    let target = offset.apply(pc);

                    for (i, (instr, ex)) in self.decoded[(pc + 1) as usize..].iter().zip(&self.instr_ex[(pc + 1) as usize..]).enumerate() {
                        let pc = pc + 1 + (i as u32);
                        match (instr, ex) {
                            // TODO: Allow conditional skips when repeat until is implemented
                            (LuaInstruction::Jump { offset }, InstrExdata { skipped_by_previous: SkipInfo::None, .. }) if offset.is_backward() => {
                                if self.follow_jump(pc + 1) == target {
                                    break 'used_jump JumpUsage::MakeBreak;
                                }
                            },
                            (LuaInstruction::ForLoop { .. }, ..) => {
                                if self.follow_jump(pc + 1) == target {
                                    break 'used_jump JumpUsage::MakeBreak;
                                }
                            },
                            (LuaInstruction::TForLoop { .. }, ..) => {
                                if self.follow_jump(pc + 1) == target {
                                    break 'used_jump JumpUsage::MakeBreak;
                                }
                            },
                            _ => ()
                        }
                    }
                }
            }
            break JumpUsage::None;
        };

        match jump_usage {
            JumpUsage::None => (),
            JumpUsage::MakeBreak => {
                let view = {
                    let mut builder = self.builder(None);
                    builder.build(
                        Box::new(BreakViewData),
                        ViewType::simple_statement(),
                        make_view_key!(name: "break", desc: "breaks out of the surrounding loop body")
                    )
                };
                self.add_view(view)?;
                used_jump_instr = true;
            }
        }

        #[derive(Debug)]
        enum IterResult {
            IfStat { cond_index: ViewRef, else_end_pc: Option<u32>, exit: u32 },
            ElseStat { cond_index: ViewRef, else_index: ViewRef },
            WhileStat { cond_index: ViewRef, exit: u32 },
            For { prep_index: ViewRef },
            SimpleCondExpr { cond_index: ViewRef, dest: Reg },
            LoadBoolCondExpr { cond_index: ViewRef, dest: Reg, has_tail: bool },
            CondAssign { cond_index: ViewRef, dest: Reg },
            PartialRepeatUntil { cond_index: ViewRef, exit: u32 },
            RepeatUntil { cond_index: ViewRef, exit: u32 },
            None
        }

        let mut handled_repeat_until = false;

        // Iterate through root views looking for any conditional jumps landing on the next instruction
        // If a conditional jump lands on the next instruction, then there is an if statement.
        // If the above is true AND this instruction is a forward jump, an if else statement is formed.
        loop {
            let result: IterResult = 'outer: loop {
                let instr = &self.decoded[pc as usize];
                let ex = &self.instr_ex[pc as usize];

                let next_pc = self.follow_jump(pc + 1);

                for root_view in self.iter_root() {
                    if let ViewType::Conditional { exit, .. } = root_view.view_type {
                        if exit == next_pc {
                            // Form a scope from last root view to the root view before the conditional
                            // Then, take the conditional
                            // This forms the if statement

                            let else_end_pc = match (instr, ex) {
                                (LuaInstruction::Jump { offset }, InstrExdata { skipped_by_previous: SkipInfo::None, .. }) if !used_jump_instr =>
                                    Some(offset.apply(pc)),
                                _ => None
                            };

                            if let Some(else_pc) = else_end_pc {
                                used_jump_instr = true;

                                if else_pc <= pc {
                                    let top = self.simulate_conditional(root_view.index, root_view.instructions.end + 1, exit).0;
                                    let top_top: ViewRef = self.view_at(top).top.into();
                                    let top_pc = self.view_at(top_top).instructions.start;

                                    if top_pc == else_pc {
                                        break 'outer IterResult::WhileStat { cond_index: root_view.index, exit };
                                    }
                                }
                            }

                            // If there is an else detected, check for the presence of a loadbool that skips over another loadbool
                            // Otherwise, if the body is a single expression, check for either a TESTSET or the usage of the expression's register in the conditional TEST or BinCondOps
                            if let Some(..) = else_end_pc {
                                if let LuaInstruction::LoadBool { skip_next: true, .. } = self.decoded[(pc + 1) as usize] {
                                    // Just handle it in the loadbool doing the skip
                                    break;
                                }
                            } else {
                                // Check if body is a single expression
                                let body_view_count = self.iter_root().take_while(|v| v.index > root_view.index).count();
                                if body_view_count == 1 {
                                    let body = self.iter_root().next().unwrap();
                                    if let ViewType::Expression { dest, .. } = body.view_type {
                                        let (_, _, views) = self.simulate_conditional(root_view.index, exit, root_view.instructions.end + 1);
                                        // Look for either a TEST or a TESTSET
                                        // The TEST must test `dest`, and it must depend on a view (which would be it's expression)
                                        let ok = views.iter().cloned().any(|v| {
                                            if let ViewType::Conditional { ref info, .. } = self.view_at(v).view_type {
                                                match *info {
                                                    PartialCondInfo::Test { inlined_source: true, source_reg } if source_reg == dest => true,
                                                    PartialCondInfo::TestSet { dest_reg } if dest_reg == dest => true,
                                                    _ => false
                                                }
                                            } else {
                                                false
                                            }
                                        });

                                        if ok {
                                            break 'outer IterResult::SimpleCondExpr { cond_index: root_view.index, dest };
                                        }
                                    }
                                } else if body_view_count == 0 {
                                    // Else, check for ANY TESTSET in the conditional, and grab its destination reg
                                    // An assignment needs to be emitted
                                    let (_, _, views) = self.simulate_conditional(root_view.index, exit, root_view.instructions.end + 1);

                                    let mut dest: Option<Reg> = None;
                                    for v in views.iter().cloned() {
                                        if let ViewType::Conditional { ref info, .. } = self.view_at(v).view_type {
                                            match (info, dest) {
                                                (&PartialCondInfo::TestSet { dest_reg }, Some(some_dest)) if dest_reg != some_dest => {
                                                    dest = None;
                                                    break;
                                                },
                                                (&PartialCondInfo::TestSet { dest_reg }, None) => {
                                                    dest = Some(dest_reg);
                                                    break;
                                                },
                                                _ => ()
                                            }
                                        }
                                    }

                                    if let Some(dest) = dest {
                                        break 'outer IterResult::CondAssign { cond_index: root_view.index, dest };
                                    }
                                }
                            }

                            let (_, _, views) = self.simulate_conditional(root_view.index, exit, root_view.instructions.end + 1);
                            // Look for either a TEST or a TESTSET
                            // The TEST must test `dest`, and it must depend on a view (which would be it's expression)
                            let has_testset = views.iter().cloned().any(|v| {
                                if let ViewType::Conditional { ref info, .. } = self.view_at(v).view_type {
                                    match *info {
                                        PartialCondInfo::TestSet { .. } => true,
                                        _ => false
                                    }
                                } else {
                                    false
                                }
                            });

                            if has_testset {
                                let body_view_count = self.iter_root().take_while(|v| v.index > root_view.index).count();
                                panic!("Invalid IfStat slipped through the cracks! {:?}", body_view_count)
                            }

                            break 'outer IterResult::IfStat { cond_index: root_view.index, else_end_pc, exit };
                        } else if exit < pc && pc == root_view.instructions.start && !handled_repeat_until {
                            handled_repeat_until = true;

                            match self.current_tag() {
                                Some(ViewTag::RepeatUntilLeaf) => {
                                    break
                                },
                                Some(ViewTag::RepeatUntilTail) => {
                                    break 'outer IterResult::RepeatUntil { cond_index: root_view.index, exit };
                                },
                                _ => (),
                            }

                            // Game plan: tag with PartialRepeatUntil, in block/scope finalization pass we find partial repeat until strides and we replace the tag with RepeatUntil, and then we start rebuilding after the earliest RepeatUntil


                            // i guess i can emit a repeat until at each unhandled backwards jump
                            // and then collapse them if needed
                            // repeat body until x and y would generate repeat body until x first, then it would wrap it in a repeat % until y
                            // and i could detect this and emit a tag that tells it to not emit repeat % until x so that it'll do until x and y
                            // but
                            // i'd rather not do that

                            // potential scope issues: if we get a base register disagreement, we have to check to see if there's a repeat until before us where the base register does agree
                            // if there is, we exclude the trailing conditional from creating a repeat until and we bail
                            // key word: where the base register _does_ agree
                            // could potentially be an issue if the register is an implicit nil, an allocation happens inside of the repeat until, and an unrelated based expr causes the repeat until to break apart
                            break 'outer IterResult::PartialRepeatUntil { cond_index: root_view.index, exit };
                        } else {
                            break;
                        }
                    } else if let ViewType::Else { cond_index, end } = root_view.view_type {
                        if end == next_pc {
                            break 'outer IterResult::ElseStat { cond_index, else_index: root_view.index };
                        } else {
                            break;
                        }
                    } else if let ViewType::ForPrep { end, .. } = root_view.view_type {
                        if end == pc + 1 {
                            break 'outer IterResult::For { prep_index: root_view.index };
                        } else {
                            break;
                        }
                    } else if let ViewType::TForPrep { end, .. } = root_view.view_type {
                        if end == pc + 1 {
                            used_jump_instr = true;
                            break 'outer IterResult::For { prep_index: root_view.index };
                        } else {
                            break;
                        }
                    } else if let ViewType::SkippingLoadBool { dest, has_tail, .. } = root_view.view_type {
                        break 'outer IterResult::LoadBoolCondExpr { cond_index: root_view.index, dest, has_tail };
                    }
                }

                break IterResult::None;
            };

            trace!("{:?} {}", result, pc);

            match result {
                IterResult::IfStat { cond_index, else_end_pc, exit } => {
                    self.make_scope_after(cond_index, make_view_key!(name: "if scope", desc: "scope for an if statement"))?;

                    if let Some(else_pc) = else_end_pc {
                        let view = {
                            let mut builder = self.builder(None);
                            builder.build(
                                Box::new(ElseViewData),
                                ViewType::Else { cond_index: cond_index, end: else_pc },
                                make_view_key!(name: "pending else statement", desc: "marks that an else statement is expected later")
                            )
                        };
                        self.add_view(view)?;
                    } else {
                        let view = {
                            let start = self.view_at(cond_index).instructions.end + 1;
                            let mut builder = self.builder(None);
                            let main_body = builder.take_scope();
                            let (cond_context, cond_views) = builder.take_conditional(start, exit);
                            builder.build(
                                Box::new(IfStatViewData { main_body, cond_context, cond_views }),
                                ViewType::simple_statement(),
                                make_view_key!(name: "if statement", desc: "conditionally runs the contents of one scope or an optional other")
                            )
                        };
                        self.add_view(view)?;
                    }
                },
                IterResult::ElseStat { cond_index, else_index } => {
                    self.make_scope_after(else_index, make_view_key!(name: "else scope", desc: "scope for an else statement"))?;

                    let view = {
                        let start = self.view_at(cond_index).instructions.end + 1;
                        let exit = self.view_at(else_index).instructions.end + 1;
                        let mut builder = self.builder(None);
                        let else_body = builder.take_scope();
                        builder.take_else();
                        let main_body = builder.take_scope();
                        let (cond_context, cond_views) = builder.take_conditional(start, exit);
                        builder.build(
                            Box::new(IfElseStatViewData { main_body, else_body, cond_context, cond_views }),
                            ViewType::simple_statement(),
                            make_view_key!(name: "if else statement", desc: "conditionally runs the contents of the if scope or the else scope")
                        )
                    };
                    self.add_view(view)?;
                },
                IterResult::WhileStat { cond_index, exit } => {
                    self.make_scope_after(cond_index, make_view_key!(name: "while scope", desc: "scope for a while loop"))?;

                    let view = {
                        let start = self.view_at(cond_index).instructions.end + 1;
                        let mut builder = self.builder(None);
                        let main_body = builder.take_scope();
                        let (cond_context, cond_views) = builder.take_conditional(start, exit);
                        builder.build(
                            Box::new(WhileStatViewData { main_body, cond_context, cond_views }),
                            ViewType::simple_statement(),
                            make_view_key!(name: "while loop", desc: "runs the contents of the given scope while the condition is false")
                        )
                    };
                    self.add_view(view)?;
                },
                IterResult::For { prep_index } => {
                    self.make_scope_after(prep_index, make_view_key!(name: "for scope", desc: "scope for a for loop"))?;

                    let view = {
                        let mut builder = self.builder(None);
                        let body = builder.take_scope();
                        let prep = builder.take_for_prep();
                        builder.build(
                            Box::new(ForViewData { prep, body }),
                            ViewType::simple_statement(),
                            make_view_key!(name: "for loop", desc: "runs the contents of the given scope for each value")
                        )
                    };
                    self.add_view(view)?;
                },
                IterResult::SimpleCondExpr { cond_index, dest } => {
                    let tail: ViewRef;
                    let view = {
                        let start = self.view_at(cond_index).instructions.end + 1;
                        let exit = self.follow_jump(pc + 1);
                        let mut builder = self.builder(None);
                        tail = builder.take_simple_cond_tail(dest);
                        let (cond_context, cond_views) = builder.take_conditional(exit, start);
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            CondExprViewData { tail: Some(ViewOrReg::View(tail)), cond_context, cond_views, invert_last: true },
                            dest,
                            make_view_key!(name: "simple conditional expression", desc: "chooses between different values depending on the result of a conditional expression")
                        )
                    };
                    self.add_view(view)?;
                },
                IterResult::LoadBoolCondExpr { dest, has_tail, .. } => {
                    let view = {
                        let mut builder = self.builder(None);
                        let (cond_context, cond_views) = builder.take_conditional_loadbool();
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            CondExprViewData { tail: None, cond_context, cond_views, invert_last: !has_tail },
                            dest,
                            make_view_key!(name: "load bool conditional expression", desc: "chooses between different values depending on the result of a conditional expression and returns true or false")
                        )
                    };
                    self.add_view(view)?;
                },
                IterResult::CondAssign { cond_index, dest } => {
                    let view = {
                        let start = self.view_at(cond_index).instructions.end + 1;
                        let exit = self.follow_jump(pc + 1);
                        let mut builder = self.builder(None);
                        let (cond_context, cond_views) = builder.take_conditional(exit, start);
                        builder.allocate_single(dest);
                        self.build_expression(
                            builder,
                            CondExprViewData { tail: Some(ViewOrReg::Reg(dest)), cond_context, cond_views, invert_last: true },
                            dest,
                            make_view_key!(name: "value part of a conditional assignment", desc: "chooses between a value that is also assigned to a destination")
                        )
                    };
                    let view_ref = self.end_view();
                    self.add_view(view)?;
                    let view = {
                        let mut builder = self.builder(None);
                        if let TakeAssignmentResult::Assignment(assignment_info) = builder.take_for_assignment(view_ref.into(), PartialAssignmentLHS::Local(dest)) {
                            builder.build(
                                // TODO: Give the ViewType to the debug print fn instead of this
                                Box::new(SetGlobalViewData(assignment_info.clone())),
                                ViewType::assignment(assignment_info),
                                make_view_key!(name: "conditional assignment", desc: "conditionally chooses what value to assign to an existing value")
                            )
                        } else {
                            unreachable!()
                        }
                    };
                    self.add_view(view)?;
                },
                IterResult::PartialRepeatUntil { exit, .. } => {
                    let view = {
                        let mut builder = self.builder(None);
                        builder.take_single_view();
                        builder.build(
                            Box::new(RepeatUntilMarkerViewData),
                            ViewType::RepeatUntilMarker { exit },
                            make_view_key!(name: "repeat until marker", desc: "marker view for a partially decompiled repeat until statement")
                        )
                    };
                    self.add_view(view)?;
                },
                IterResult::RepeatUntil { cond_index, exit } => {
                    // This part of the pass should have complete statements in the block
                    // If we have something invalid at the statement level then we should bail and flag all conditional instructions as not part of a repeat until

                    let view = 'repeat: loop {
                        let start = self.view_at(cond_index).instructions.end + 1;

                        let mut builder = self.builder(None);
                        let cond = builder.take_conditional(start, exit);
                        let block = builder.take_views_until(|view| view.instructions.start < exit);
                        for index in block.iter() {
                            if !self.view_at(*index).view_type.is_statement_level() {
                                println!("WARN: Repeat until caused invalid block potentially");
                                break 'repeat Err(cond.1)
                            }
                        }
                        break 'repeat Ok(builder.build(
                            Box::new(RepeatUntilViewData { cond, body: block }),
                            ViewType::simple_statement(),
                            make_view_key!(name: "repeat until", desc: "repeat until loop")
                        ))
                    };
                    match view {
                        Ok(view) => self.add_view(view)?,
                        Err(reset_conditionals) => {
                            for view in reset_conditionals {
                                self.replace_view_tag(view, ViewTag::RepeatUntilLeaf)
                            }
                        }
                    }
                },
                IterResult::None => break
            }
        }

        if !used_jump_instr {
            if let &LuaInstruction::Jump { offset } = &self.decoded[pc as usize] {
                if offset.is_backward() {
                    // while true do loop
                    let target = offset.apply(pc);
                    let top = self.iter_root()
                        .take_while(|view| self.view_at(view.top).instructions.start >= target)
                        .map(|v| v.index)
                        .last()
                        .unwrap_or(self.end_view());
                    self.make_scope_including(top, make_view_key!(name: "while true scope", desc: "scope of a while true loop"))?;
                    let view = {
                        let mut builder = self.builder(None);
                        let main_body = builder.take_scope();
                        builder.build(
                            Box::new(WhileTrueStatViewData { main_body }),
                            ViewType::simple_statement(),
                            make_view_key!(name: "while true loop", desc: "while loop that has no condition")
                        )
                    };
                    self.add_view(view)?;
                }
            }
        }

        Ok(())
    }
}
