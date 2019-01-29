mod assignment_info;
mod builder;
mod newtable;
mod view_data;
mod view_type;

use std;
use std::ops::Range;

use instruction_definitions::{Reg, Kst};
use dump::{DumpContext, DumpType};

pub use self::assignment_info::*;
pub use self::view_type::*;
pub use self::builder::{ViewBuilder, TakeAssignmentResult};
pub use self::newtable::*;
pub use self::view_data::*;

pub trait ViewData: std::fmt::Debug {
    fn dump(&self, context: &mut DumpContext, typ: DumpType);
}

pub type RegRange = Range<Reg>;

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub struct ViewRef(u32);

impl ViewRef {
    pub fn raw(&self) -> u32 {
        self.0
    }

    pub fn is_first(&self) -> bool {
        self.0 == 0
    }

    pub fn is_not_first(&self) -> bool {
        !self.is_first()
    }
}

impl From<u32> for ViewRef {
    fn from(value: u32) -> ViewRef {
        ViewRef(value)
    }
}

impl std::ops::Add<u32> for ViewRef {
    type Output = ViewRef;

    fn add(self, v: u32) -> ViewRef {
        ViewRef(self.0 + v)
    }
}

impl std::ops::Sub<u32> for ViewRef {
    type Output = ViewRef;

    fn sub(self, v: u32) -> ViewRef {
        ViewRef(self.0 - v)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ViewOrReg {
    View(ViewRef),
    Reg(Reg)
}

impl ViewOrReg {
    pub fn dump(&self, context: &mut DumpContext) {
        match self {
            &ViewOrReg::View(view) => context.write_view(view, DumpType::Expression),
            &ViewOrReg::Reg(reg) => context.write_reg(reg),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ViewOrRegOrKst {
    View(ViewRef),
    Reg(Reg),
    Kst(Kst)
}

impl ViewOrRegOrKst {
    pub fn dump(&self, context: &mut DumpContext) {
        match self {
            &ViewOrRegOrKst::View(view) => context.write_view(view, DumpType::Expression),
            &ViewOrRegOrKst::Reg(reg) => context.write_reg(reg),
            &ViewOrRegOrKst::Kst(kst) => context.write_constant(kst),
        }
    }

    pub fn dump_index(&self, context: &mut DumpContext) {
        match self {
            &ViewOrRegOrKst::View(view) => {
                context.write_str("[");
                context.write_view(view, DumpType::Expression);
                context.write_str("]");
            },
            &ViewOrRegOrKst::Reg(reg) => {
                context.write_str("[");
                context.write_reg(reg);
                context.write_str("]");
            },
            &ViewOrRegOrKst::Kst(kst) => {
                if context.is_valid_name(kst) {
                    context.write_str(".");
                    context.write_name(kst);
                } else {
                    context.write_str("[");
                    context.write_constant(kst);
                    context.write_str("]");
                }
            },
        }
    }
}

impl From<ViewOrReg> for ViewOrRegOrKst {
    fn from(v: ViewOrReg) -> ViewOrRegOrKst {
        match v {
            ViewOrReg::View(view) => ViewOrRegOrKst::View(view),
            ViewOrReg::Reg(reg) => ViewOrRegOrKst::Reg(reg)
        }
    }
}

#[derive(Debug)]
pub struct View {
    pub index: ViewRef,
    pub data: Box<ViewData>,
    pub view_type: ViewType,
    pub instructions: Range<u32>,
    pub dependent_view_count: u32,
    pub strong_dependent_view_count: Option<u32>, // Number of dependent views that are strong (both directly and indirectly)
    pub top: ViewRef, // Like dependent view count, but is the backwards relative offset to the top
    pub allocations: Option<RegRange>,
    pub base: Option<Reg>,
    pub dependent_reg_top: Option<Reg>,
    pub passive_dependent_regs: bool,
    pub key: ViewKeyRef,
}

#[derive(Debug)]
pub enum ViewBail {
    // Pins an expression at the given view.
    // All work is thrown out up until that point.
    // Also tags the tail instruction of the view with ViewTag::Pinned.
    PinExpression { // TODO: Rename to Rewind
        view: ViewRef
    },
    // Retries the given view.
    ImplicitNil {
        start: Reg,
        end: Reg
    },
    // Rewinds to the view before [rewind_view], encapsulates all views up to [view] in a scope, adds a DoStatViewData, and retries from [rewind_view]
    ScopeWrap {
        view: ViewRef,
        rewind_view: Option<ViewRef>,
        retry: bool,
    },
}

#[derive(Debug)]
pub enum CallFunc {
    Func(ViewRef),
    SelfCall(ViewRef)
}

#[derive(Debug)]
pub struct ViewKey {
    pub line: u32,
    pub name: &'static str,
    pub description: &'static str,
}

pub type ViewKeyRef = &'static ViewKey;

impl PartialEq for ViewKeyRef {
    fn eq(&self, other: &Self) -> bool {
        (*self as *const ViewKey) == (*other as *const ViewKey)
    }
}

impl Eq for ViewKeyRef {}

macro_rules! make_view_key {
    (name: $name:expr , desc: $description:expr) => {
        {
            mod _view_key {
                use super::ViewKey;
                pub static KEY: ViewKey = ViewKey {
                    line: line!(),
                    name: $name,
                    description: $description
                };
            }

            &_view_key::KEY
        }
    }
}
