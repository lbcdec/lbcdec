use dump::DumpContext;
use instruction_definitions::{Reg, Kst, RK, Upvalue};
use view::{ViewOrRegOrKst, ViewOrReg};

use ::ViewBuilder;

#[derive(Debug, Clone)]
pub struct AssignmentInfo {
    // An expression like `x.y = z` will have `x` be an input on the lhs
    // lhs inputs are evaluated before the rhs of the entire assignment,
    // so complex acrobatics have to be done to grab them.
    pub lhs: AssignmentLHS,
    pub sources: Vec<ViewOrRegOrKst>,
    // Local variable allocations cannot be collapsed with non-new-local variable
    // assignments.
    pub allocated_local: bool,
}

impl AssignmentInfo {
    pub fn dump(&self, context: &mut DumpContext) {
        match &self.lhs {
            &AssignmentLHS::Partial(ref p) => Self::format_partial_lhs(context, &p),
            &AssignmentLHS::Full(ref f) => Self::format_full_lhs(context, &f),
        }

        context.write_str(" = ");

        for (i, src) in self.sources.iter().rev().enumerate() {
            if i != 0 {
                context.write_str(", ");
            }
            src.dump(context);
        }
    }

    fn format_partial_lhs(context: &mut DumpContext, partial: &Vec<PartialAssignmentLHS>) {
        for (i, p) in partial.iter().rev().enumerate() {
            if i != 0 {
                context.write_str(", ");
            }
            match p {
                &PartialAssignmentLHS::Table { table, index } => {
                    context.write_reg(table);
                    match index {
                        RK::R(reg) => {
                            context.write_str("[");
                            context.write_reg(reg);
                            context.write_str("]");
                        },
                        RK::K(constant) => {
                            let valid_name = context.is_valid_name(constant);
                            if valid_name {
                                context.write_str(".");
                                context.write_name(constant);
                            } else {
                                context.write_str("[");
                                context.write_constant(constant);
                                context.write_str("]");
                            }
                        },
                    }
                },
                &PartialAssignmentLHS::Global(kst) => {
                    context.write_name(kst);
                },
                &PartialAssignmentLHS::Upvalue(upval) => {
                    context.write_upvalue(upval);
                },
                &PartialAssignmentLHS::Local(reg) => {
                    context.write_reg(reg);
                },
                &PartialAssignmentLHS::Multi { base, count } => {
                    context.write_str("[");
                    context.write_reg(base);
                    context.write_str("..");
                    context.write_reg(Reg(base.0 + count - 1));
                    context.write_str("]");
                }
            }
        }
    }

    fn format_full_lhs(context: &mut DumpContext, full: &Vec<FullAssignmentLHS>) {
        for (i, f) in full.iter().rev().enumerate() {
            if i != 0 {
                context.write_str(", ");
            }
            match f {
                &FullAssignmentLHS::Table { table, index } => {
                    table.dump(context);
                    index.dump_index(context);
                },
                &FullAssignmentLHS::Global(kst) => {
                    context.write_name(kst);
                },
                &FullAssignmentLHS::Upvalue(upval) => {
                    context.write_upvalue(upval);
                },
                &FullAssignmentLHS::Local(reg) => {
                    context.write_reg(reg);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PartialAssignmentLHS {
    Table {
        table: Reg,
        index: RK
    },
    Global(Kst),
    Upvalue(Upvalue),
    Local(Reg),
    Multi {
        base: Reg,
        count: u8
    }
}

impl PartialAssignmentLHS {
    pub fn pull_base_reg(&self) -> Option<Reg> {
        if let &PartialAssignmentLHS::Table {table, index} = &self {
            Some(if let RK::R(index_reg) = index {
                if index_reg.is_above(*table) {
                    *index_reg
                } else {
                    *table
                }
            } else {
                *table
            })
        } else {
            None
        }
    }

    pub fn can_convert_to_full(&self) -> bool {
        match &self {
            &PartialAssignmentLHS::Multi { .. } => false,
            _ => true
        }
    }

    pub fn to_full(self, builder: &mut ViewBuilder) -> FullAssignmentLHS {
        match self {
            PartialAssignmentLHS::Table { table, index } => {
                let index_view = builder.take_reg_or_kst(index);
                let table_view = builder.take_reg(table);
                FullAssignmentLHS::Table {
                    table: table_view,
                    index: index_view
                }
            },
            PartialAssignmentLHS::Global(k) => FullAssignmentLHS::Global(k),
            PartialAssignmentLHS::Upvalue(u) => FullAssignmentLHS::Upvalue(u),
            PartialAssignmentLHS::Local(r) => FullAssignmentLHS::Local(r),
            PartialAssignmentLHS::Multi { .. } => panic!("Cannot convert {:?} to full assignment", self)
        }
    }
}

#[derive(Debug, Clone)]
pub enum FullAssignmentLHS {
    Table {
        table: ViewOrReg,
        index: ViewOrRegOrKst
    },
    Global(Kst),
    Upvalue(Upvalue),
    Local(Reg)
}

#[derive(Debug, Clone)]
pub enum AssignmentLHS {
    // Both vecs are "reversed"
    Partial(Vec<PartialAssignmentLHS>),
    Full(Vec<FullAssignmentLHS>)
}
