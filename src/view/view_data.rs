use cond_logic::{CondContext, CondGroupID, CondTargetID, CondJoin, CondVisitor};
use instruction_definitions::{Reg, Kst, BinCondOp, UnOp, BinOp, Upvalue, ClosureCapture};
use dump::{DumpContext, DumpType};
use view::{CallFunc, ViewData, ViewOrReg, ViewOrRegOrKst, ViewRef};
use view::assignment_info::AssignmentInfo;
use view::newtable::TableInfo;

#[derive(Debug)]
pub struct MoveViewData {
    pub source: Reg
}
impl ViewData for MoveViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_reg(self.source);
    }
}

#[derive(Debug)]
pub struct MoveAssignViewData(pub AssignmentInfo);
impl ViewData for MoveAssignViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.0.dump(context);
    }
}

#[derive(Debug)]
pub struct GetGlobalViewData {
    pub index: Kst,
}
impl ViewData for GetGlobalViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_name(self.index);
    }
}

#[derive(Debug)]
pub struct SetGlobalViewData(pub AssignmentInfo);
impl ViewData for SetGlobalViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.0.dump(context);
    }
}

#[derive(Debug)]
pub struct GetUpvalueViewData {
    pub upvalue: Upvalue,
}
impl ViewData for GetUpvalueViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_upvalue(self.upvalue);
    }
}

#[derive(Debug)]
pub struct SetUpvalueViewData(pub AssignmentInfo);
impl ViewData for SetUpvalueViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.0.dump(context);
    }
}

#[derive(Debug)]
pub struct LoadKViewData {
    pub kst: Kst
}
impl ViewData for LoadKViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_constant(self.kst);
    }
}

#[derive(Debug)]
pub struct LoadNilViewData(pub u8);
impl ViewData for LoadNilViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        let mut n = self.0;
        while n > 0 {
            context.write_str("nil");
            if n != 1 {
                context.write_str(", ");
            }
            n -= 1;
        }
    }
}

#[derive(Debug)]
pub struct LoadBoolViewData(pub bool);
impl ViewData for LoadBoolViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        if self.0 {
            context.write_str("true");
        } else {
            context.write_str("false");
        }
    }
}

#[derive(Debug)]
pub struct SkippingLoadBoolViewData(pub Option<ViewRef>);
impl ViewData for SkippingLoadBoolViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        if let Some(view) = self.0 {
            context.write_view(view, DumpType::Expression)
        } else {
            context.write_str("conditional loadbool")
        }
    }
}

#[derive(Debug)]
pub struct TestViewData {
    pub source: ViewOrReg,
    pub inverted: bool,
}
impl ViewData for TestViewData {
    fn dump(&self, context: &mut DumpContext, typ: DumpType) {
        // TODO: if this is within a conditional expression and this test is acting like a TESTSET, invert self.inverted

        let inverted = typ.handle_invert(self.inverted);

        if inverted {
            context.write_str("not ");
        }

        self.source.dump(context);
    }
}

#[derive(Debug)]
pub struct TestSetViewData {
    pub source: ViewOrReg,
    pub inverted: bool,
}
impl ViewData for TestSetViewData {
    fn dump(&self, context: &mut DumpContext, typ: DumpType) {
        let inverted = typ.handle_invert(!self.inverted);

        if !inverted {
            context.write_str("not ");
        }

        self.source.dump(context);
    }
}

#[derive(Debug)]
pub struct BinCondOpViewData {
    pub lhs: ViewOrRegOrKst,
    pub op: BinCondOp,
    pub rhs: ViewOrRegOrKst,
    pub inverted: bool,
    pub flipped: bool,
}
impl ViewData for BinCondOpViewData {
    fn dump(&self, context: &mut DumpContext, typ: DumpType) {
        self.lhs.dump(context);

        enum ExpandedCond {
            Eq, Neq, Lt, Le, Gt, Ge
        }

        impl ExpandedCond {
            fn from(op: BinCondOp, flipped: bool) -> ExpandedCond {
                match op {
                    BinCondOp::Eq => ExpandedCond::Eq,
                    BinCondOp::Lt => if !flipped { ExpandedCond::Lt } else { ExpandedCond::Gt },
                    BinCondOp::Le => if !flipped { ExpandedCond::Le } else { ExpandedCond::Le }
                }
            }

            fn invert(self, inverted: bool) -> ExpandedCond {
                if inverted {
                    match self {
                        ExpandedCond::Eq => ExpandedCond::Neq,
                        ExpandedCond::Neq => ExpandedCond::Eq,
                        ExpandedCond::Lt => ExpandedCond::Ge,
                        ExpandedCond::Ge => ExpandedCond::Lt,
                        ExpandedCond::Le => ExpandedCond::Gt,
                        ExpandedCond::Gt => ExpandedCond::Le
                    }
                } else {
                    self
                }
            }
        }

        let inverted = typ.handle_invert(self.inverted);
        let exop = ExpandedCond::from(self.op, self.flipped).invert(inverted);

        context.write_str(match exop {
            ExpandedCond::Eq => " == ",
            ExpandedCond::Neq => " ~= ",
            ExpandedCond::Lt => " < ",
            ExpandedCond::Ge => " >= ",
            ExpandedCond::Le => " <= ",
            ExpandedCond::Gt => " > "
        });

        self.rhs.dump(context);
    }
}

#[derive(Debug)]
pub struct ConditionalJumpViewData {
    pub cond: ViewRef,
}
impl ViewData for ConditionalJumpViewData {
    fn dump(&self, context: &mut DumpContext, typ: DumpType) {
        context.write_view(self.cond, DumpType::Conditional { inverted: typ.handle_invert(false) });
    }
}

#[derive(Debug)]
pub struct BinOpViewData {
    pub lhs: ViewOrRegOrKst,
    pub op: BinOp,
    pub rhs: ViewOrRegOrKst,
}
impl ViewData for BinOpViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.lhs.dump(context);

        match self.op {
            BinOp::Add => context.write_str(" + "),
            BinOp::Sub => context.write_str(" - "),
            BinOp::Mul => context.write_str(" * "),
            BinOp::Div => context.write_str(" / "),
            BinOp::Mod => context.write_str(" % "),
            BinOp::Pow => context.write_str(" ^ ")
        }

        self.rhs.dump(context);
    }
}

#[derive(Debug)]
pub struct UnOpViewData {
    pub op: UnOp,
    pub rhs: ViewOrReg
}
impl ViewData for UnOpViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        match self.op {
            UnOp::Len => context.write_str("#"),
            UnOp::Not => context.write_str("not "),
            UnOp::Unm => context.write_str("-")
        }

        self.rhs.dump(context);
    }
}

#[derive(Debug)]
pub struct GetTableViewData {
    pub table: ViewOrReg,
    pub index: ViewOrRegOrKst
}
impl ViewData for GetTableViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.table.dump(context);
        self.index.dump_index(context);
    }
}

#[derive(Debug)]
pub struct SetTableViewData(pub AssignmentInfo);
impl ViewData for SetTableViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.0.dump(context);
    }
}

#[derive(Debug)]
pub struct CallViewData {
    pub func: CallFunc,
    pub args: Vec<ViewRef>
}
impl ViewData for CallViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        match self.func {
            CallFunc::Func(view) => context.write_view(view, DumpType::Expression),
            CallFunc::SelfCall(view) => context.write_view(view, DumpType::Expression),
        }
        context.write_str("(");
        for (i, arg) in self.args.iter().rev().enumerate() {
            if i != 0 {
                context.write_str(", ");
            }
            context.write_view(*arg, DumpType::Expression);
        }
        context.write_str(")");
    }
}

#[derive(Debug)]
pub struct SelfViewData {
    pub object: ViewOrReg,
    pub method: Kst
}
impl ViewData for SelfViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.object.dump(context);
        context.write_str(":");
        context.write_name(self.method);
    }
}

#[derive(Debug)]
pub struct NewTableViewData;
impl ViewData for NewTableViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("{}");
    }
}

#[derive(Debug)]
pub struct TableViewData(pub TableInfo);
impl ViewData for TableViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        self.0.dump(context);
    }
}

#[derive(Debug)]
pub struct PrototypeViewData(pub u32, pub Vec<ClosureCapture>); // ProtoIdx
impl ViewData for PrototypeViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_proto(self.0, &self.1);
    }
}

#[derive(Debug)]
pub struct ConcatViewData(pub Vec<ViewRef>);
impl ViewData for ConcatViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        for (i, view) in self.0.iter().rev().enumerate() {
            if i != 0 {
                context.write_str("..");
            }
            context.write_view(*view, DumpType::Expression);
        }
    }
}

#[derive(Debug)]
pub struct VarArgsViewData;
impl ViewData for VarArgsViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("...");
    }
}

#[derive(Debug)]
pub struct ImplicitNilViewData {
    pub start: Reg,
    pub end: Reg
}
impl ViewData for ImplicitNilViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("local ");
        let mut reg = self.start;
        loop {
            context.write_reg(reg);
            if reg == self.end {
                break;
            }
            context.write_str(", ");
            reg = reg.next();
        }
    }
}

#[derive(Debug)]
pub struct ScopeViewData {
    pub statements: Vec<ViewRef>,
}
impl ViewData for ScopeViewData {
    fn dump(&self, context: &mut DumpContext, typ: DumpType) {
        let indent = if let DumpType::Statement { last } = typ { last } else { false };
        if !self.statements.is_empty() {
            if indent { context.indent() }
            for (i, view) in self.statements.iter().rev().enumerate() {
                let last = i == self.statements.len()-1;
                context.write_view(*view, DumpType::Statement { last });
                if indent {
                    if last {
                        context.unindent();
                    }
                    context.write_newline();
                } else {
                    if !last {
                        context.write_newline();
                    }
                }
            }
        }
    }
}

struct CondDumper<'a> {
    context: &'a mut DumpContext,
    cond_context: &'a CondContext,
    cond_views: &'a [ViewRef],
    root: bool,
    inverted: bool,
    invert_last: bool,
    just_update_inverted: bool,
}

impl<'a> CondDumper<'a> {
    fn new(context: &'a mut DumpContext, cond_context: &'a CondContext, cond_views: &'a [ViewRef]) -> CondDumper<'a> {
        CondDumper {
            context,
            cond_context,
            cond_views,
            root: true,
            inverted: false,
            invert_last: false,
            just_update_inverted: false,
        }
    }

    fn output_join(&mut self, join: CondJoin) {
        match join {
            CondJoin::And => {
                self.context.write_str(" and ");
            },
            CondJoin::Or => {
                self.context.write_str(" or ");
            },
            CondJoin::Unknown => {
                self.context.write_str(" unknown ");
            },
            CondJoin::Unspecified => {
                self.context.write_str(" error ");
            }
        }
    }
}

impl<'a> CondVisitor for CondDumper<'a> {
    type Item = ();

    // TODO: The logic here is invalid. If self.invert is true while going into a group, the last item in the group is the one to be inverted, not the first item in the group.

    fn on_group(&mut self, group: CondGroupID, target_count: usize, join: CondJoin, last_in_group: bool) -> Self::Item {
        if self.just_update_inverted {
            self.inverted = match join {
                CondJoin::Or | CondJoin::Unknown => true,
                _ => false
            }
        } else {
            // In the original implementation in lbcdec.dart, conditionals were processed from right to left
            // We need to process them the same way, but its problematic because we're writing to a stream of characters
            // So what we do is we recursively call a function that lets us process the conditionals backwards while allowing us to emit them forward
            
            let was_root = self.root;
            if !was_root {
                self.context.write_str("(");
            }
            self.root = false;

            fn recursive_handle_target<'a>(this: &mut CondDumper<'a>, group: CondGroupID, index: usize) {
                let current_invert = this.inverted;

                if index != 0 {
                    // This polls the join to the right of us to figure out if the next target needs to be inverted or not

                    this.just_update_inverted = true;
                    // TODO: Use a different visitor for invert polling
                    this.cond_context.visit_group_target(group, index-1, this);
                    this.just_update_inverted = false;

                    recursive_handle_target(this, group, index-1);
                }

                // After processing inside out, emit with proper invert
                this.inverted = current_invert;
                this.cond_context.visit_group_target(group, index, this);
            }

            if self.invert_last {
                self.inverted = !self.inverted;
            }

            recursive_handle_target(self, group, target_count - 1);

            if !was_root {
                self.context.write_str(")");
            }
            self.root = was_root;
            
            if !last_in_group {
                self.output_join(join);
            }
        }
    }

    fn on_target(&mut self, target: CondTargetID, join: CondJoin, last_in_group: bool) -> Self::Item {
        if self.just_update_inverted {
            self.inverted = match join {
                CondJoin::Or | CondJoin::Unknown => true,
                _ => false
            }
        } else {
            self.context.write_view(self.cond_views[target.0], DumpType::Conditional { inverted: self.inverted });

            if !last_in_group {
                self.output_join(join);
            }
        }
    }
}

#[derive(Debug)]
pub struct CondExprViewData {
    pub tail: Option<ViewOrReg>,
    pub cond_context: CondContext,
    pub cond_views: Vec<ViewRef>,
    pub invert_last: bool,
}
impl ViewData for CondExprViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        {
            let mut cond_dumper = CondDumper::new(context, &self.cond_context, &self.cond_views);
            cond_dumper.inverted = self.invert_last;

            self.cond_context.visit(&mut cond_dumper);
        }
        if let Some(tail) = self.tail {
            context.write_str(" or ");
            tail.dump(context);
        }
    }
}

#[derive(Debug)]
pub struct IfStatViewData {
    pub main_body: ViewRef,
    pub cond_context: CondContext,
    pub cond_views: Vec<ViewRef>,
}
impl ViewData for IfStatViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("if ");
        {
            let mut cond_dumper = CondDumper::new(context, &self.cond_context, &self.cond_views);

            self.cond_context.visit(&mut cond_dumper);
        }
        context.write_str(" then");
        context.write_newline();
        context.write_view(self.main_body, DumpType::Statement { last: true });
        context.write_str("end");
    }
}

#[derive(Debug)]
pub struct ElseViewData;
impl ViewData for ElseViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("pending else");
    }
}

#[derive(Debug)]
pub struct IfElseStatViewData {
    pub main_body: ViewRef,
    pub else_body: ViewRef,
    pub cond_context: CondContext,
    pub cond_views: Vec<ViewRef>,
}
impl ViewData for IfElseStatViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("if ");
        {
            let mut cond_dumper = CondDumper::new(context, &self.cond_context, &self.cond_views);

            self.cond_context.visit(&mut cond_dumper);
        }
        context.write_str(" then");
        context.write_newline();
        context.write_view(self.main_body, DumpType::Statement { last: true });
        context.write_str("else");
        context.write_newline();
        context.write_view(self.else_body, DumpType::Statement { last: true });
        context.write_str("end");
    }
}

#[derive(Debug)]
pub struct WhileStatViewData {
    pub main_body: ViewRef,
    pub cond_context: CondContext,
    pub cond_views: Vec<ViewRef>,
}
impl ViewData for WhileStatViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("while ");
        {
            let mut cond_dumper = CondDumper::new(context, &self.cond_context, &self.cond_views);

            self.cond_context.visit(&mut cond_dumper);
        }
        context.write_str(" do");
        context.write_newline();
        context.write_view(self.main_body, DumpType::Statement { last: true });
        context.write_str("end");
    }
}

#[derive(Debug)]
pub struct WhileTrueStatViewData {
    pub main_body: ViewRef,
}
impl ViewData for WhileTrueStatViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("while true do");
        context.write_newline();
        context.write_view(self.main_body, DumpType::Statement { last: true });
        context.write_str("end");
    }
}

#[derive(Debug)]
pub enum ReturnViewData {
    None,
    SingleReg(Reg),
    Views(Vec<ViewRef>)
}
impl ViewData for ReturnViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("return");

        match self {
            ReturnViewData::None => (),
            &ReturnViewData::SingleReg(reg) => {
                context.write_str(" ");
                context.write_reg(reg);
            },
            &ReturnViewData::Views(ref views) => {
                context.write_str(" ");
                for (i, arg) in views.iter().cloned().rev().enumerate() {
                    if i != 0 {
                        context.write_str(", ");
                    }
                    context.write_view(arg, DumpType::Expression);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct BreakViewData;
impl ViewData for BreakViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("break");
    }
}

#[derive(Debug)]
pub struct ForPrepViewData {
    pub base: Reg,
    pub init: ViewRef,
    pub limit: ViewRef,
    pub step: ViewRef,
}
impl ViewData for ForPrepViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("for ");
        context.write_reg(Reg(self.base.0 + 3));
        context.write_str(" = ");
        context.write_view(self.init, DumpType::Expression);
        context.write_str(", ");
        context.write_view(self.limit, DumpType::Expression);
        context.write_str(", ");
        context.write_view(self.step, DumpType::Expression);
        context.write_str(" do");
    }
}

#[derive(Debug)]
pub struct ForViewData {
    pub prep: ViewRef,
    pub body: ViewRef,
}
impl ViewData for ForViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_view(self.prep, DumpType::Expression);
        context.write_newline();
        context.write_view(self.body, DumpType::Statement { last: true });
        context.write_str("end");
    }
}

#[derive(Debug)]
pub struct TForPrepViewData {
    pub values: Vec<ViewRef>,
    pub base: Reg,
    pub count: u16
}
impl ViewData for TForPrepViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("for ");
        let mut reg = Reg(self.base.0 + 3);
        let mut counter = self.count;
        while counter > 0 {
            context.write_reg(reg);
            reg = reg.next();
            counter -= 1;
            if counter != 0 {
                context.write_str(", ");
            }
        }
        context.write_str(" in ");
        for (i, arg) in self.values.iter().cloned().rev().enumerate() {
            if i != 0 {
                context.write_str(", ");
            }
            context.write_view(arg, DumpType::Expression);
        }
        context.write_str(" do");
    }
}

#[derive(Debug)]
pub struct DoStatViewData {
    pub main_body: ViewRef,
}
impl ViewData for DoStatViewData {
    fn dump(&self, context: &mut DumpContext, typ: DumpType) {
        if let DumpType::Statement { last: true } = typ {
            context.write_view(self.main_body, DumpType::Statement { last: false });
        } else {
            context.write_str("do");
            context.write_newline();
            context.write_view(self.main_body, DumpType::Statement { last: true });
            context.write_str("end");
        }
    }
}

#[derive(Debug)]
pub struct CloseViewData {
    pub base: Reg,
}
impl ViewData for CloseViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("-- explicit close to ");
        context.write_reg(self.base);
    }
}

#[derive(Debug)]
pub struct BackwardsJumpBarrierViewData;
impl ViewData for BackwardsJumpBarrierViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("-- backwards jump barrier");
    }
}

#[derive(Debug)]
pub struct RepeatUntilMarkerViewData;
impl ViewData for RepeatUntilMarkerViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("-- repeat until marker");
    }
}

#[derive(Debug)]
pub struct RepeatUntilViewData {
    pub cond: (CondContext, Vec<ViewRef>),
    pub body: Vec<ViewRef>,
}
impl ViewData for RepeatUntilViewData {
    fn dump(&self, context: &mut DumpContext, _: DumpType) {
        context.write_str("repeat");
        context.write_newline();
        if !self.body.is_empty() {
            context.indent();
            for (i, view) in self.body.iter().rev().enumerate() {
                let last = i == self.body.len()-1;
                context.write_view(*view, DumpType::Statement { last });
                if last {
                    context.unindent();
                }
                context.write_newline();
            }
        }
        context.write_str("until ");
        {
            let mut cond_dumper = CondDumper::new(context, &self.cond.0, &self.cond.1);

            self.cond.0.visit(&mut cond_dumper);
        }
    }
}
