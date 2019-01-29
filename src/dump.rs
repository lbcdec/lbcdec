use instruction_definitions::{Reg, Kst, Upvalue, ClosureCapture};
use view::ViewRef;

#[derive(Debug)]
pub enum DumpType {
    Statement { last: bool },
    Expression,
    Conditional { inverted: bool }
}

impl DumpType {
    pub fn handle_invert(&self, inverted: bool) -> bool {
        if let &DumpType::Conditional { inverted: cond_invert } = self {
            if inverted {
                !cond_invert
            } else {
                cond_invert
            }
        } else {
            inverted
        }
    }
}

pub trait DumpContext {
    fn write_root(&mut self);
    fn write_str(&mut self, s: &str);
    fn write_view(&mut self, index: ViewRef, typ: DumpType);
    fn write_newline(&mut self);
    fn write_name(&mut self, kst: Kst);
    fn write_constant(&mut self, kst: Kst);
    fn is_valid_name(&self, kst: Kst) -> bool;
    fn write_reg(&mut self, reg: Reg);
    fn write_proto(&mut self, proto: u32, captures: &[ClosureCapture]);
    fn write_upvalue(&mut self, upvalue: Upvalue);
    fn indent(&mut self);
    fn unindent(&mut self);
}
