use std;

#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub struct Reg(pub u8);
impl Reg {
    pub fn is_at_or_above(&self, other: Reg) -> bool {
        self.0 >= other.0
    }

    pub fn is_above(&self, other: Reg) -> bool {
        self.0 > other.0
    }

    pub fn previous(&self) -> Option<Reg> {
        if self.0 == 0 {
            None
        } else {
            Some(Reg(self.0 - 1))
        }
    }

    pub fn next(&self) -> Reg {
        Reg(self.0 + 1)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Kst(pub u32);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RK {
    R(Reg), K(Kst)
}

#[derive(Debug, Copy, Clone)]
pub struct Upvalue(pub u16);

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod, Pow
}

#[derive(Debug, Copy, Clone)]
pub enum UnOp {
    Unm, Not, Len
}

#[derive(Debug, Copy, Clone)]
pub enum BinCondOp {
    Eq, Lt, Le
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Count(u16);
impl Count {
    pub fn raw(n: u16) -> Count {
        Count(n)
    }

    pub fn from(n: u16) -> Count {
        Count(n + 1)
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 1
    }

    pub fn is_varargs(&self) -> bool {
        self.0 == 0
    }

    pub fn count(&self) -> Option<u16> {
        if self.0 != 0 {
            Some(self.0 - 1)
        } else {
            None
        }
    }

    // base: Reg(0), self: VARARGS or EMPTY => None
    // base: Reg(0), self: Count(n) => Reg(base.0 + n - 1)
    pub fn top(&self, base: Reg) -> Option<Reg> {
        if self.0 > 0 {
            Some(Reg(base.0 + ((self.0 - 2) as u8)))
        } else {
            None
        }
    }

    pub const VARARGS: Count = Count(0);
    pub const EMPTY: Count = Count(1);
}
impl std::fmt::Debug for Count {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "Count(")?;
        if self.0 == 0 {
            write!(f, "VarArgs)")
        } else {
            write!(f, "{})", self.0 - 1)
        }
    }
}

impl std::ops::Add<Count> for Reg {
    type Output = Option<Reg>;
    fn add(self, other: Count) -> Option<Reg> {
        if other.is_varargs() {
            None
        } else {
            Some(Reg(self.0 + ((other.0 - 1) as u8)))
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PCOffset(i32);
impl PCOffset {
    pub fn new(offset: i32) -> PCOffset {
        PCOffset(offset)
    }

    pub fn apply(&self, pc: u32) -> u32 {
        ((pc as i32) + self.0) as u32
    }

    pub fn is_forward(&self) -> bool {
        self.0 > 0
    }

    pub fn is_backward(&self) -> bool {
        !self.is_forward()
    }
}

#[derive(Debug, Clone)]
pub enum ClosureCapture {
    Register(Reg),
    Upvalue(Upvalue)
}

#[derive(Debug)]
pub enum LuaInstruction {
    Move { dest: Reg, source: Reg },
    LoadK { dest: Reg, kst: Kst },
    LoadBool { dest: Reg, value: bool, skip_next: bool },
    LoadNil { dest_start: Reg, dest_end: Reg },
    GetUpvalue { dest: Reg, upvalue: Upvalue },
    GetGlobal { dest: Reg, index: Kst },
    GetTable { dest: Reg, table: Reg, index: RK },
    SetGlobal { source: Reg, index: Kst },
    SetUpvalue { source: Reg, upvalue: Upvalue },
    SetTable { table: Reg, index: RK, source: RK },
    NewTable { dest: Reg, array_count: u32, hash_count: u32 },
    Self_ { base: Reg, object: Reg, method: RK },
    BinOp { dest: Reg, lhs: RK, op: BinOp, rhs: RK },
    UnOp { dest: Reg, op: UnOp, rhs: Reg },
    Concat { dest: Reg, source_start: Reg, source_end: Reg },
    Jump { offset: PCOffset },
    BinCondOp { inverted: bool, lhs: RK, op: BinCondOp, rhs: RK },
    Test { source: Reg, inverted: bool },
    TestSet { dest: Reg, source: Reg, inverted: bool },
    Call { base: Reg, args: Count, ret: Count },
    TailCall { base: Reg, args: Count, ret: Count },
    Return { base: Reg, count: Count },
    ForLoop { base: Reg, target: PCOffset },
    ForPrep { base: Reg, target: PCOffset },
    TForLoop { base: Reg, count: u16 },
    SetList { base: Reg, count: Count, set: u32 },
    Close { base: Reg },
    Closure { dest: Reg, prototype: u32, captures: Vec<ClosureCapture> },
    VarArg { base: Reg, count: Count },
    SemanticNoOp
}

impl LuaInstruction {
    pub fn fb2int(x: u16) -> u32 {
        let e = (x >> 3) & 31;
        if e == 0 {
            x.into()
        } else {
            (((x & 7) + 8) as u32) << (e - 1)
        }
    }
}
