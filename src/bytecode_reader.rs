use nom;

use nom::{be_u8, be_i8};

#[derive(Clone,Debug,PartialEq,Eq)]
pub struct LuaHeader {
    pub format_version: u8,
    pub big_endian: bool,
    pub integer_size: u8,
    pub size_t_size: u8,
    pub instruction_size: u8,
    pub number_size: u8,
    pub number_integral: bool
}

impl LuaHeader {
    fn endian(&self) -> nom::Endianness {
        if self.big_endian {
            nom::Endianness::Big
        } else {
            nom::Endianness::Little
        }
    }
}

#[derive(Debug)]
pub enum LuaNumber {
    Integral(i64),
    Floating(f64)
}

#[derive(Debug)]
pub enum LuaConstant {
    Null,
    Bool(bool),
    Number(LuaNumber),
    String(Vec<u8>)
}

#[derive(Debug)]
pub struct LuaLocal {
    pub name: Vec<u8>,
    pub start_pc: u64,
    pub end_pc: u64
}

#[derive(Debug)]
pub struct LuaVarArgInfo {
    pub has_arg: bool,
    pub needs_arg: bool,
}

#[derive(Debug)]
pub struct LuaChunk {
    pub name: Vec<u8>,
    pub line_defined: u64,
    pub last_line_defined: u64,
    pub num_upvalues: u8,
    pub num_params: u8,
    pub is_vararg: Option<LuaVarArgInfo>,
    pub max_stack: u8,
    pub instructions: Vec<u32>,
    pub constants: Vec<LuaConstant>,
    pub prototypes: Vec<LuaChunk>,
    pub source_lines: Vec<u64>,
    pub locals: Vec<LuaLocal>,
    pub upvalue_names: Vec<Vec<u8>>,
}

#[derive(Debug)]
pub struct LuaBytecode {
    pub header: LuaHeader,
    pub main_chunk: LuaChunk
}

named!(pub lua_header<LuaHeader>,
    do_parse!(
        tag!(b"\x1BLua") >>
        tag!(b"\x51") >>
        format_version: be_u8 >>
        big_endian: be_u8 >>
        integer_size: be_u8 >>
        size_t_size: be_u8 >>
        instruction_size: be_u8 >>
        number_size: be_u8 >>
        number_integral: be_u8 >>
        (LuaHeader {
            format_version: format_version,
            big_endian: big_endian != 1,
            integer_size: integer_size,
            size_t_size: size_t_size,
            instruction_size: instruction_size,
            number_size: number_size,
            number_integral: number_integral != 0
        })
    )
);

named_args!(lua_integer<'a>(header: &'a LuaHeader) <u64>,
    switch!(
        value!(header.integer_size),
        1 => map!(be_u8, |v| v as u64) |
        2 => map!(u16!(header.endian()), |v| v as u64) |
        4 => map!(u32!(header.endian()), |v| v as u64) |
        8 => map!(u64!(header.endian()), |v| v as u64)
    )
);

named_args!(lua_size_t<'a>(header: &'a LuaHeader) <u64>,
    switch!(
        value!(header.size_t_size),
        1 => map!(be_u8, |v| v as u64) |
        2 => map!(u16!(header.endian()), |v| v as u64) |
        4 => map!(u32!(header.endian()), |v| v as u64) |
        8 => map!(u64!(header.endian()), |v| v as u64)
    )
);

named_args!(lua_instruction<'a>(header: &'a LuaHeader) <u32>,
    switch!(
        value!(header.instruction_size),
        4 => u32!(header.endian())
    )
);

macro_rules! f32 ( ($i:expr, $e:expr) => ( {if nom::Endianness::Big == $e { nom::be_f32($i) } else { nom::le_f32($i) } } ););
macro_rules! f64 ( ($i:expr, $e:expr) => ( {if nom::Endianness::Big == $e { nom::be_f64($i) } else { nom::le_f64($i) } } ););

named_args!(lua_number<'a>(header: &'a LuaHeader) <LuaNumber>,
    switch!(
        value!(header.number_integral),
        true => map!(
            switch!(
                value!(header.number_size),
                1 => map!(be_i8, |v| v as i64) |
                2 => map!(i16!(header.endian()), |v| v as i64) |
                4 => map!(i32!(header.endian()), |v| v as i64) |
                8 => map!(i64!(header.endian()), |v| v as i64)
            ),
            |v| LuaNumber::Integral(v)
        ) |
        false => map!(
            switch!(
                value!(header.number_size),
                4 => map!(f32!(header.endian()), |v| v as f64) |
                8 => map!(f64!(header.endian()), |v| v as f64)
            ),
            |v| LuaNumber::Floating(v)
        )
    )
);

macro_rules! lua_string ( ($i:expr, $header:expr) => ( { map!($i, length_bytes!(apply!(lua_size_t, $header)), |v| if v.is_empty() { v } else { &v[..v.len()-1] }) } ););

named_args!(lua_local<'a>(header: &'a LuaHeader) <LuaLocal>,
    do_parse!(
        name: lua_string!(header) >>
        start_pc: apply!(lua_integer, header) >>
        end_pc: apply!(lua_integer, header) >>
        (LuaLocal {
            name: name.to_vec(),
            start_pc: start_pc,
            end_pc: end_pc,
        })
    )
);

named_args!(pub lua_chunk<'a>(header: &'a LuaHeader) <LuaChunk>,
    do_parse!(
        name: lua_string!(header) >>
        line_defined: apply!(lua_integer, header) >>
        last_line_defined: apply!(lua_integer, header) >>
        num_upvalues: be_u8 >>
        num_params: be_u8 >>
        is_vararg: be_u8 >>
        max_stack: be_u8 >>
        instructions: length_count!(apply!(lua_integer, header), apply!(lua_instruction, header)) >>
        constants: length_count!(apply!(lua_integer, header), switch!(
            be_u8,
            0 => value!(LuaConstant::Null) |
            1 => map!(be_u8, |v| LuaConstant::Bool(v != 0)) |
            3 => map!(apply!(lua_number, header), |v| LuaConstant::Number(v)) |
            4 => map!(lua_string!(header), |v| LuaConstant::String(v.to_vec()))
        )) >>
        prototypes: length_count!(apply!(lua_integer, header), apply!(lua_chunk, header)) >>
        source_lines: length_count!(apply!(lua_integer, header), apply!(lua_integer, header)) >>
        locals: length_count!(apply!(lua_integer, header), apply!(lua_local, header)) >>
        upvalue_names: length_count!(apply!(lua_integer, header), map!(lua_string!(header), |v| v.to_vec())) >>
        (LuaChunk {
            name: name.to_vec(),
            line_defined: line_defined,
            last_line_defined: last_line_defined,
            num_upvalues: num_upvalues,
            num_params: num_params,
            is_vararg: if (is_vararg & 2) != 0 {
                Some(LuaVarArgInfo {
                    has_arg: (is_vararg & 1) != 0,
                    needs_arg: (is_vararg & 4) != 0,
                })
            } else {
                None
            },
            max_stack: max_stack,
            instructions: instructions,
            constants: constants,
            prototypes: prototypes,
            source_lines: source_lines,
            locals: locals,
            upvalue_names: upvalue_names,
        })
    )
);

named!(pub lua_bytecode<LuaBytecode>,
    do_parse!(
        header: lua_header >>
        main_chunk: apply!(lua_chunk, &header) >>
        (LuaBytecode {
            header: header,
            main_chunk: main_chunk,
        })
    )
);
