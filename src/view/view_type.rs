use instruction_definitions::{Reg, Count};

use view::AssignmentInfo;
use view::TableInfo;
use view::ViewRef;

#[derive(Debug)]
pub enum ViewType {
    Expression { 
        dest: Reg,
        info: ExpressionInfo,
    },
    SelfExpression { base: Reg },
    PinnedExpression { dest: Reg },
    VarArgsExpression { base: Reg },
    MultiExpression { base: Reg, count: Count },
    Statement {
        assignment: Option<AssignmentInfo>,
    },
    Scope,
    PartialConditional(PartialCondInfo),
    Conditional { exit: u32, info: PartialCondInfo },
    Else { cond_index: ViewRef, end: u32 },
    ForPrep { base: Reg, end: u32 },
    TForPrep { base: Reg, count: u16, end: u32 },
    SkippingLoadBool { dest: Reg, has_tail: bool },
    RepeatUntilMarker { exit: u32 },
}

impl ViewType {
    pub fn simple_statement() -> ViewType {
        ViewType::Statement {
            assignment: None
        }
    }

    pub fn assignment(info: AssignmentInfo) -> ViewType {
        ViewType::Statement {
            assignment: Some(info)
        }
    }

    pub fn is_statement_level(&self) -> bool {
        match self {
            ViewType::Statement {..} => true,
            ViewType::PinnedExpression {..} => true,
            ViewType::Expression {..} => true,
            ViewType::MultiExpression {..} => true,

            ViewType::SelfExpression {..} => false,
            ViewType::VarArgsExpression {..} => false,
            ViewType::Scope {..} => false,
            ViewType::PartialConditional {..} => false,
            ViewType::Conditional {..} => false,
            ViewType::Else {..} => false,
            ViewType::ForPrep {..} => false,
            ViewType::TForPrep {..} => false,
            ViewType::SkippingLoadBool {..} => false,
            ViewType::RepeatUntilMarker {..} => false,
        }
    }

    pub fn needs_finalization(&self) -> bool {
        match self {
            // Expression types need finalization (but not really)
            ViewType::Expression {..} => true,
            ViewType::RepeatUntilMarker {..} => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum ExpressionInfo {
    None,
    Table(TableInfo)
}

#[derive(Debug, Clone)]
pub enum PartialCondInfo {
    Test { inlined_source: bool, source_reg: Reg },
    BinCondOp,
    TestSet { dest_reg: Reg },
}
