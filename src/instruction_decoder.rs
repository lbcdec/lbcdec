use enum_primitive::FromPrimitive;

use instruction_definitions::*;
use bytecode_reader::LuaChunk;

fn decode_op(instr: u32) -> u8 { (instr & 0x3F) as u8 }

struct ABC {
    a: u8,
    b: u16,
    c: u16
}
fn decode_abc(instr: u32) -> ABC {
    ABC { 
        a: ((instr >> 6) & 0xFF) as u8,
        b: ((instr >> 23) & 0x1FF) as u16,
        c: ((instr >> 14) & 0x1FF) as u16,
    } 
}
impl ABC {
    fn reg_a(&self) -> Reg { Reg(self.a) }
    fn reg_b(&self) -> Reg {
        if self.b > 0xFF {
            panic!("Encountered out of bounds value for operand b")
        } else {
            Reg(self.b as u8)
        }
    }
    fn reg_c(&self) -> Reg {
        if self.c > 0xFF {
            panic!("Encountered out of bounds value for operand c")
        } else {
            Reg(self.c as u8)
        }
    }

    fn bool_a(&self) -> bool {
        self.a != 0
    }
    fn bool_b(&self) -> bool {
        self.b != 0
    }
    fn bool_c(&self) -> bool {
        self.c != 0
    }

    fn rk_b(&self) -> RK {
        if self.b > 0xFF {
            RK::K(Kst((self.b - 256) as u32))
        } else {
            RK::R(Reg(self.b as u8))
        }
    }
    fn rk_c(&self) -> RK {
        if self.c > 0xFF {
            RK::K(Kst((self.c - 256) as u32))
        } else {
            RK::R(Reg(self.c as u8))
        }
    }

    fn count_b(&self) -> Count {
        Count::raw(self.b)
    }
    fn count_c(&self) -> Count {
        Count::raw(self.c)
    }

    fn upvalue_b(&self) -> Upvalue {
        Upvalue(self.b)
    }
}

struct ABx {
    a: u8,
    b: u32
}
fn decode_abx(instr: u32) -> ABx {
    ABx {
        a: ((instr >> 6) & 0xFF) as u8,
        b: (instr >> 14) & 0x3FFFF,
    }
}
impl ABx {
    fn kst(&self) -> Kst { Kst(self.b) }
}

struct AsBx {
    a: u8,
    b: i32
}
fn decode_asbx(instr: u32) -> AsBx {
    AsBx {
        a: ((instr >> 6) & 0xFF) as u8,
        b: ((instr >> 14) & 0x3FFFF) as i32 - 0x1FFFF,
    }
}
impl AsBx {
    fn pc_offset(&self) -> PCOffset { PCOffset::new(self.b + 1) }
}

enum_from_primitive! {
    #[derive(Debug, PartialEq)]
    enum Opcode {
        Move = 0,
        LoadK,
        LoadBool,
        LoadNil,
        GetUpval,
        GetGlobal,
        GetTable,
        SetGlobal,
        SetUpval,
        SetTable,
        NewTable,
        Self_,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        Pow,
        Unm,
        Not,
        Len,
        Concat,
        Jump,
        Eq,
        Lt,
        Le,
        Test,
        TestSet,
        Call,
        TailCall,
        Return,
        ForLoop,
        ForPrep,
        TForLoop,
        SetList,
        Close,
        Closure,
        VarArg,
    }
}

pub fn decode_instruction<'a>(mut instructions: &'a [u32], prototypes: &'a [LuaChunk]) -> Option<(&'a [u32], LuaInstruction, u32)> {
    if instructions.is_empty() {
        return None
    }
    
    let instr = instructions[0];

    let mut instruction_count = 1u32;

    let op = decode_op(instr);
    let abc = decode_abc(instr);
    let abx = decode_abx(instr);
    let asbx = decode_asbx(instr);

    trace!("{:08X}: {:?} ({})", instr, Opcode::from_u8(op), op);

    let decoded = match Opcode::from_u8(op) {
        Some(Opcode::Move) => LuaInstruction::Move { dest: abc.reg_a(), source: abc.reg_b() },
        Some(Opcode::LoadK) => {
            match instructions.get(1).cloned() {
                Some(next) if Opcode::from_u8(decode_op(next)) == Some(Opcode::Self_) && decode_abc(next).rk_c() == RK::R(abc.reg_a()) => {
                    // Work around a Lua 5.1 bug where the load for an overflowing constant index is loaded in SELF's base+2 instead of base+1
                    instruction_count += 1;
                    let next_abc = decode_abc(next);
                    LuaInstruction::Self_ { base: next_abc.reg_a(), object: next_abc.reg_b(), method: RK::K(abx.kst()) }
                }
                _ => LuaInstruction::LoadK { dest: abc.reg_a(), kst: abx.kst() }
            }
        },
        Some(Opcode::LoadBool) => LuaInstruction::LoadBool { dest: abc.reg_a(), value: abc.bool_b(), skip_next: abc.bool_c() },
        Some(Opcode::LoadNil) => LuaInstruction::LoadNil { dest_start: abc.reg_a(), dest_end: abc.reg_b() },
        Some(Opcode::GetUpval) => LuaInstruction::GetUpvalue { dest: abc.reg_a(), upvalue: abc.upvalue_b() },
        Some(Opcode::GetGlobal) => LuaInstruction::GetGlobal { dest: abc.reg_a(), index: abx.kst() },
        Some(Opcode::GetTable) => LuaInstruction::GetTable { dest: abc.reg_a(), table: abc.reg_b(), index: abc.rk_c() },
        Some(Opcode::SetGlobal) => LuaInstruction::SetGlobal { source: abc.reg_a(), index: abx.kst() },
        Some(Opcode::SetUpval) => LuaInstruction::SetUpvalue { source: abc.reg_a(), upvalue: abc.upvalue_b() },
        Some(Opcode::SetTable) => LuaInstruction::SetTable { table: abc.reg_a(), index: abc.rk_b(), source: abc.rk_c() },
        Some(Opcode::NewTable) => LuaInstruction::NewTable { dest: abc.reg_a(), array_count: LuaInstruction::fb2int(abc.b), hash_count: LuaInstruction::fb2int(abc.c) },
        Some(Opcode::Self_) => LuaInstruction::Self_ { base: abc.reg_a(), object: abc.reg_b(), method: abc.rk_c() },
        Some(Opcode::Add) => LuaInstruction::BinOp { dest: abc.reg_a(), lhs: abc.rk_b(), op: BinOp::Add, rhs: abc.rk_c() },
        Some(Opcode::Sub) => LuaInstruction::BinOp { dest: abc.reg_a(), lhs: abc.rk_b(), op: BinOp::Sub, rhs: abc.rk_c() },
        Some(Opcode::Mul) => LuaInstruction::BinOp { dest: abc.reg_a(), lhs: abc.rk_b(), op: BinOp::Mul, rhs: abc.rk_c() },
        Some(Opcode::Div) => LuaInstruction::BinOp { dest: abc.reg_a(), lhs: abc.rk_b(), op: BinOp::Div, rhs: abc.rk_c() },
        Some(Opcode::Mod) => LuaInstruction::BinOp { dest: abc.reg_a(), lhs: abc.rk_b(), op: BinOp::Mod, rhs: abc.rk_c() },
        Some(Opcode::Pow) => LuaInstruction::BinOp { dest: abc.reg_a(), lhs: abc.rk_b(), op: BinOp::Pow, rhs: abc.rk_c() },
        Some(Opcode::Unm) => LuaInstruction::UnOp { dest: abc.reg_a(), op: UnOp::Unm, rhs: abc.reg_b() },
        Some(Opcode::Not) => LuaInstruction::UnOp { dest: abc.reg_a(), op: UnOp::Not, rhs: abc.reg_b() },
        Some(Opcode::Len) => LuaInstruction::UnOp { dest: abc.reg_a(), op: UnOp::Len, rhs: abc.reg_b() },
        Some(Opcode::Concat) => LuaInstruction::Concat { dest: abc.reg_a(), source_start: abc.reg_b(), source_end: abc.reg_c() },
        Some(Opcode::Jump) => LuaInstruction::Jump { offset: asbx.pc_offset() },
        Some(Opcode::Eq) => LuaInstruction::BinCondOp { inverted: abc.bool_a(), lhs: abc.rk_b(), op: BinCondOp::Eq, rhs: abc.rk_c() },
        Some(Opcode::Lt) => LuaInstruction::BinCondOp { inverted: abc.bool_a(), lhs: abc.rk_b(), op: BinCondOp::Lt, rhs: abc.rk_c() },
        Some(Opcode::Le) => LuaInstruction::BinCondOp { inverted: abc.bool_a(), lhs: abc.rk_b(), op: BinCondOp::Le, rhs: abc.rk_c() },
        Some(Opcode::Test) => LuaInstruction::Test { source: abc.reg_a(), inverted: abc.bool_c() },
        Some(Opcode::TestSet) => LuaInstruction::TestSet { dest: abc.reg_a(), source: abc.reg_b(), inverted: abc.bool_c() },
        Some(Opcode::Call) => LuaInstruction::Call { base: abc.reg_a(), args: abc.count_b(), ret: abc.count_c() },
        Some(Opcode::TailCall) => LuaInstruction::TailCall { base: abc.reg_a(), args: abc.count_b(), ret: abc.count_c() },
        Some(Opcode::Return) => LuaInstruction::Return { base: abc.reg_a(), count: abc.count_b() },
        Some(Opcode::ForLoop) => LuaInstruction::ForLoop { base: abc.reg_a(), target: asbx.pc_offset() },
        Some(Opcode::ForPrep) => LuaInstruction::ForPrep { base: abc.reg_a(), target: asbx.pc_offset() },
        Some(Opcode::TForLoop) => LuaInstruction::TForLoop { base: abc.reg_a(), count: abc.c },
        Some(Opcode::SetList) => {
            let set = if abc.c == 0 {
                instruction_count += 1;
                instructions[1]
            } else {
                abc.c.into()
            };
            LuaInstruction::SetList { base: abc.reg_a(), count: abc.count_b(), set }
        },
        Some(Opcode::Close) => LuaInstruction::Close { base: abc.reg_a() },
        Some(Opcode::Closure) => {
            let prototype = abx.b;
            let proto = &prototypes[prototype as usize];
            let captures: Vec<ClosureCapture> = instructions.iter().cloned()
                .skip(1)
                .take(proto.num_upvalues as usize)
                .map(|instr| {
                    let op = decode_op(instr);
                    let abc = decode_abc(instr);
                    match Opcode::from_u8(op) {
                        Some(Opcode::Move) => ClosureCapture::Register(abc.reg_b()),
                        Some(Opcode::GetUpval) => ClosureCapture::Upvalue(abc.upvalue_b()),
                        _ => panic!("Unknown closure capture instruction {}", op)
                    }
                })
                .collect();
            instruction_count += proto.num_upvalues as u32;
            LuaInstruction::Closure { dest: abc.reg_a(), prototype, captures }
        },
        Some(Opcode::VarArg) => LuaInstruction::VarArg { base: abc.reg_a(), count: abc.count_b() },
        _ => {
            panic!("Decode failed")
        }
    };

    instructions = &instructions[(instruction_count as usize)..];

    trace!("{:?}", decoded);

    Some((instructions, decoded, instruction_count))
}
