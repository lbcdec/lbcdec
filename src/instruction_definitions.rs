use std;
use crate::Either;

#[derive(Debug, PartialOrd, PartialEq, Eq, Copy, Clone)]
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

#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub enum RegOrTop {
    Reg(Reg),
    Top
}

impl RegOrTop {
    pub fn as_reg(self) -> Option<Reg> {
        match self {
            RegOrTop::Reg(reg) => Some(reg),
            _ => None
        }
    }

    pub fn as_reg_mut(&mut self) -> Option<&mut Reg> {
        match self {
            RegOrTop::Reg(ref mut reg) => Some(reg),
            _ => None
        }
    }
}

impl From<Option<Reg>> for RegOrTop {
    fn from(v: Option<Reg>) -> RegOrTop {
        v.map(RegOrTop::Reg).unwrap_or(RegOrTop::Top)
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

    pub fn add(&self, n: u16) -> Count {
        if self.is_varargs() {
            *self
        } else {
            Count(self.0 + n)
        }
    }

    // base: Reg(0), self: VARARGS or EMPTY => None
    // base: Reg(0), self: Count(n) => Reg(base.0 + n - 1)
    pub fn top(&self, base: Reg) -> Option<Reg> {
        if self.0 > 0 {
            if self.0 == 1 {
                // EMPTY
                return Some(base)
            }
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

pub enum InstructionIO {
    Stack(Count),
    Single(Reg),
    Range(std::ops::Range<Reg>),
}

#[derive(Debug)]
pub enum InstructionInfo {
    Stack {
        base: Reg,
        inputs: Count,
        outputs: Count,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UsageType {
    Read,
    ReadVarArgs,
    Write,
    WriteVarArgs,
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

    pub fn base(&self) -> Option<Reg> {
        match self {
            LuaInstruction::Self_ { base, .. } => Some(base),
            LuaInstruction::Concat { source_start, .. } => Some(source_start),
            LuaInstruction::Call { base, .. } => Some(base),
            LuaInstruction::TailCall { base, .. } => Some(base),
            LuaInstruction::Return { base, count } if count.count().is_none() || count.count().unwrap() > 1 => Some(base),
            LuaInstruction::ForLoop { base, .. } => Some(base),
            LuaInstruction::ForPrep { base, .. } => Some(base),
            LuaInstruction::TForLoop { base, .. } => Some(base),
            LuaInstruction::SetList { base, .. } => Some(base),
            LuaInstruction::Close { base } => Some(base),
            LuaInstruction::VarArg { base, count } if count.count() != Some(1) => Some(base),
            _ => None,
        }.map(|base| *base)
    }

    pub fn info(&self) -> Option<InstructionInfo> {
        use InstructionInfo::*;

        match *self {
            LuaInstruction::LoadNil { dest_start, dest_end } if dest_end.is_above(dest_start) => Some(Stack { base: dest_start, inputs: Count::EMPTY, outputs: Count::from((dest_end.0 - dest_start.0 + 1) as u16) }),
            LuaInstruction::Self_ { base, object, .. } => Some(Stack { base, inputs: if object == base { Count::from(1) } else { Count::EMPTY }, outputs: Count::from(2) }),
            LuaInstruction::Concat { source_start, source_end, .. } => Some(Stack { base: source_start, inputs: Count::from((source_end.0 - source_start.0 + 1) as u16), outputs: Count::EMPTY }),
            LuaInstruction::Call { base, args, ret } => Some(Stack { base, inputs: args.add(1), outputs: ret }),
            LuaInstruction::TailCall { base, args, ret } => Some(Stack { base, inputs: args.add(1), outputs: ret }),
            LuaInstruction::Return { base, count } if count.count().is_none() || count.count().unwrap() > 1 => Some(Stack { base, inputs: count, outputs: Count::EMPTY }),
            // LuaInstruction::ForLoop { base, .. } => Some(Stack { base, inputs: Count::from(4), outputs: Count::from(4) }),
            LuaInstruction::ForPrep { base, .. } => Some(Stack { base, inputs: Count::from(3), outputs: Count::from(4) }),
            LuaInstruction::TForLoop { base, count } => Some(Stack { base, inputs: Count::from(3), outputs: Count::from(3 + count) }),
            LuaInstruction::SetList { base, count, .. } => Some(Stack { base, inputs: count.count().map(|x| x + 2).map(Count::from).unwrap_or(Count::VARARGS), outputs: Count::from(1) }),
            LuaInstruction::Close { base } => Some(Stack { base, inputs: Count::EMPTY, outputs: Count::EMPTY }),
            LuaInstruction::VarArg { base, count } if count.count() != Some(1) => Some(Stack { base, inputs: Count::EMPTY, outputs: count }),
            _ => None,
        }
    }

    pub fn does_use<F: FnMut(Reg) -> bool>(&self, mut func: F) -> bool {
        let mut res = false;
        self.fold_io((), |_, reg, usage| {
            if func(reg) {
                res = true;
                None
            } else {
                Some(())
            }
        });
        res
    }

    pub fn does_read(&self, r: Reg) -> bool {
        let mut res = false;
        self.fold_io((), |_, reg, usage| {
            if usage == UsageType::Read && reg == r {
                res = true;
                None
            } else if usage == UsageType::ReadVarArgs && r.is_at_or_above(reg) {
                res = true;
                None
            } else {
                Some(())
            }
        });
        res
    }

    pub fn does_read_at_or_above(&self, r: Reg) -> bool {
        let mut res = false;
        self.fold_io((), |_, reg, usage| {
            if usage == UsageType::Read && reg.is_at_or_above(r) {
                res = true;
                None
            } else if usage == UsageType::ReadVarArgs && reg.is_at_or_above(r) {
                res = true;
                None
            } else {
                Some(())
            }
        });
        res
    }

    pub fn does_write(&self, r: Reg) -> bool {
        let mut res = false;
        self.fold_io((), |_, reg, usage| {
            if usage == UsageType::Write && reg == r {
                res = true;
                None
            } else if usage == UsageType::WriteVarArgs && r.is_at_or_above(reg) {
                res = true;
                None
            } else {
                Some(())
            }
        });
        res
    }

    pub fn does_write_at_or_above(&self, r: Reg) -> bool {
        let mut res = false;
        self.fold_io((), |_, reg, usage| {
            if usage == UsageType::Write && reg.is_at_or_above(r) {
                res = true;
                None
            } else if usage == UsageType::WriteVarArgs && reg.is_at_or_above(r) {
                res = true;
                None
            } else {
                Some(())
            }
        });
        res
    }

    pub fn top_read(&self) -> Option<Reg> {
        self.fold_io(None, |current, reg, usage| {
            if let Some(current) = *current {
                if usage == UsageType::Read && reg.is_above(current) {
                    return Some(Some(reg))
                }
            } else {
                if usage == UsageType::Read {
                    return Some(Some(reg))
                }
            }

            return Some(*current)
        })
    }

    pub fn base_write(&self) -> Option<Reg> {
        self.fold_io(None, |current, reg, usage| {
            if let Some(current) = *current {
                if usage == UsageType::Write && current.is_above(reg) {
                    return Some(Some(reg))
                }
            } else {
                if usage == UsageType::Write {
                    return Some(Some(reg))
                }
            }

            return Some(*current)
        })
    }

    // Runs the given fn until it returns None, returns the last value before it returns None
    pub fn fold_io<T, F: FnMut(&T, Reg, UsageType) -> Option<T>>(&self, mut current: T, mut func: F) -> T {
        use LuaInstruction::*;

        macro_rules! handle_fold {
            ($($($pattern:pat)|* => { $($rest:tt)* })*) => {
                match *self {
                    $($($pattern)|* => {
                        handle_fold!(inner $($rest)*);
                    })*
                    _ => unimplemented!()
                }
            };
            (inner read $reg:expr, $($rest:tt)*) => {
                match func(&current, $reg, UsageType::Read) {
                    Some(c) => {
                        current = c;
                    },
                    None => return current
                }

                handle_fold!(inner $($rest)*);
            };
            (inner read_rk $reg:expr, $($rest:tt)*) => {
                match $reg {
                    RK::R(reg) => {
                        match func(&current, reg, UsageType::Read) {
                            Some(c) => {
                                current = c;
                            },
                            None => return current
                        }
                    },
                    _ => ()
                }

                handle_fold!(inner $($rest)*);
            };
            (inner read_range $range:expr, $($rest:tt)*) => {
                let range = $range;
                let mut reg = range.start;
                while reg != range.end {
                    match func(&current, reg, UsageType::Read) {
                        Some(c) => {
                            current = c;
                        },
                        None => return current
                    }
                    reg = reg.next();
                }

                handle_fold!(inner $($rest)*);
            };
            (inner read_count $ex:expr, $($rest:tt)*) => {
                let (base, count) = $ex;
                if let Some(top) = count.top(base) {
                    let top = top.next();
                    let mut reg = base;
                    while reg != top {
                        match func(&current, reg, UsageType::Read) {
                            Some(c) => {
                                current = c;
                            },
                            None => return current
                        }
                        reg = reg.next();
                    }
                } else {
                    match func(&current, base, UsageType::ReadVarArgs) {
                        Some(c) => {
                            current = c;
                        },
                        None => return current
                    }
                }

                handle_fold!(inner $($rest)*);
            };
            (inner write $reg:expr, $($rest:tt)*) => {
                match func(&current, $reg, UsageType::Write) {
                    Some(c) => {
                        current = c;
                    },
                    None => return current
                }

                handle_fold!(inner $($rest)*);
            };
            (inner write_range $range:expr, $($rest:tt)*) => {
                let range = $range;
                let mut reg = range.start;
                while reg != range.end {
                    match func(&current, reg, UsageType::Write) {
                        Some(c) => {
                            current = c;
                        },
                        None => return current
                    }
                    reg = reg.next();
                }

                handle_fold!(inner $($rest)*);
            };
            (inner write_count $ex:expr, $($rest:tt)*) => {
                let (base, count) = $ex;
                if let Some(top) = count.top(base) {
                    let top = top.next();
                    let mut reg = base;
                    while reg != top {
                        match func(&current, reg, UsageType::Write) {
                            Some(c) => {
                                current = c;
                            },
                            None => return current
                        }
                        reg = reg.next();
                    }
                } else {
                    match func(&current, base, UsageType::WriteVarArgs) {
                        Some(c) => {
                            current = c;
                        },
                        None => return current
                    }
                }

                handle_fold!(inner $($rest)*);
            };
            (inner closure_capture $ex:expr, $($rest:tt)*) => {
                let caps = $ex;
                for cap in caps {
                    match *cap {
                        ClosureCapture::Register(reg) => {
                            match func(&current, reg, UsageType::Read) {
                                Some(c) => {
                                    current = c;
                                },
                                None => return current
                            }
                        }
                        _ => {}
                    }
                }

                handle_fold!(inner $($rest)*);
            };
            (inner) => {};
        }

        handle_fold! {
            Move { dest, source } => {
                read source,
                write dest,
            }
            LoadK { dest, .. } | LoadBool { dest, .. } | GetUpvalue { dest, .. } | GetGlobal { dest, .. } | NewTable { dest, .. } => {
                write dest,
            }
            GetTable { dest, table, index } => {
                read table,
                read_rk index,
                write dest,
            }
            LoadNil { dest_start, dest_end } => {
                write_range dest_start..(dest_end.next()),
            }
            SetGlobal { source, .. } | SetUpvalue { source, .. } => {
                read source,
            }
            SetTable { table, index, source } => {
                read table,
                read_rk index,
                read_rk source,
            }
            Self_ { base, object, method } => {
                read object,
                read_rk method,
                write base,
                write base.next(),
            }
            BinOp { dest, lhs, rhs, .. } => {
                read_rk lhs,
                read_rk rhs,
                write dest,
            }
            UnOp { dest, rhs, .. } => {
                read rhs,
                write dest,
            }
            Concat { dest, source_start, source_end } => {
                read_range source_start..(source_end.next()),
                write dest,
            }
            Jump { .. } => {}
            BinCondOp { lhs, rhs, .. } => {
                read_rk lhs,
                read_rk rhs,
            }
            Test { source, .. } => {
                read source,
            }
            TestSet { dest, source, .. } => {
                read source,
                write dest,
            }
            Call { base, args, ret } => {
                read base,
                read_count (base.next(), args),
                write_count (base, ret),
            }
            TailCall { base, args, .. } => {
                read base,
                read_count (base.next(), args),
            }
            Return { base, count } => {
                read_count (base, count),
            }
            SetList { base, count, .. } => {
                read_count (base, count.count().map(|x| x + 2).map(Count::from).unwrap_or(Count::VARARGS)),
                write base, // Just so ralloc works correctly, can be used as an alias for the source table
            }
            TForLoop { base, count } => {
                read_count (base, Count::from(3)),
                write_count (base, Count::from(3 + count + 1)),
            }
            Closure { dest, ref captures, .. } => {
                closure_capture captures,
                write dest,
            }
            Close {..} => {}
            VarArg { base, count } => {
                write_count (base, count),
            }
            ForLoop {..} => {}
            ForPrep { base, .. } => {
                read_count (base, Count::from(3)),
                write_count (base, Count::from(4)),
            }
            // TODO: ForLoop, ForPrep
            SemanticNoOp => {}
        }

        current
    }
}
