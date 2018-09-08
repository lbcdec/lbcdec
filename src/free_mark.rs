use instruction_definitions::Reg;

#[derive(Debug, Clone, PartialEq)]
pub struct FreeMark(Reg);
impl FreeMark {
    pub fn new(param_count: u8) -> FreeMark {
        FreeMark(Reg(param_count))
    }

    pub fn is_free(&self, r: Reg) -> bool {
        r.is_at_or_above(self.0)
    }

    pub fn is_allocated(&self, r: Reg) -> bool {
        !self.is_free(r)
    }

    pub fn is_next_free(&self, r: Reg) -> bool {
        self.0 == r
    }

    pub fn is_next_allocated(&self, r: Reg) -> bool {
        if let Some(prev) = self.0.previous() {
            prev == r
        } else {
            false
        }
    }

    pub fn allocate_single(&self, r: Reg) -> FreeMark {
        if self.is_allocated(r) {
            // Ignore double allocations
            FreeMark { .. *self }
        } else {
            if !self.is_next_free(r) {
                panic!("Invalid free mark allocation: {:?}, tried to allocate {:?} (hole of {} registers)", self, r, r.0 - (self.0).0)
            }
            FreeMark(self.0.next())
        }
    }

    pub fn deallocate_single(&self, r: Reg) -> FreeMark {
        if self.is_next_allocated(r) {
            FreeMark(self.0.previous().unwrap())
        } else {
            panic!("Invalid free mark deallocation: {:?}, tried to deallocate {:?}", self, r)
        }
    }
}
