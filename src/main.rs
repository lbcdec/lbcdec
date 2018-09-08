// lbcdec - Lua Bytecode Decompiler
// Copyright (C) 2018  Dwayne Slater
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#![feature(test)]

#[macro_use]
extern crate nom;

extern crate tempfile;

#[macro_use]
extern crate enum_primitive;

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate test as rust_test;

extern crate obstack;

extern crate glob;
extern crate rayon;
#[macro_use]
extern crate clap;

use std::borrow::Cow;
use std::fs;
use std::io::{Error, ErrorKind};
use std::process::Command;

mod ast;
mod bytecode_reader;
mod cond_logic;
mod dump;
mod instruction_definitions;
mod instruction_decoder;
mod free_mark;
mod reduce;
#[macro_use]
mod view;
mod view_context;

#[cfg(test)]
mod test;

use bytecode_reader::*;
use dump::*;
use instruction_definitions::*;
use view::*;
use view_context::*;

fn compile_lua(script: &str) -> std::io::Result<Vec<u8>> {
    let dir = tempfile::tempdir()?;
    let script_path = dir.path().join("script.lua");
    let bytecode_path = dir.path().join("bc.luac");

    fs::write(&script_path, script)?;

    let command = Command::new("luac5.1.exe")
        .arg("-o")
        .arg(bytecode_path.clone())
        .arg(script_path)
        .status()?;

    if !command.success() {
        return Err(Error::new(ErrorKind::Other, "Failed to compile Lua"));
    }

    let vec = fs::read(bytecode_path)?;

    dir.close()?;

    Ok(vec)
}

fn is_valid_lua_identifier(s: &[u8]) -> bool {
    let mut iter = s.iter().cloned();

    fn test_first_char(first: u8) -> bool {
        (first >= b'A' && first <= b'Z') || (first >= b'a' && first <= b'z') || first == b'_'
    }

    fn test_char(c: u8) -> bool {
        test_first_char(c) || (c >= b'0' && c <= b'9')
    }

    if let Some(first) = iter.next() {
        if test_first_char(first) {
            // First char ok, test the rest of them
            iter.all(test_char)
        } else {
            false
        }
    } else {
        false
    }
}

fn try_get_str_error<'a>(err: &'a Box<std::any::Any + Send + 'static>) -> &'a str {
    if let Some(err) = err.downcast_ref::<&'static str>() {
        return err
    } else if let Some(err) = err.downcast_ref::<String>() {
        return &err
    } else {
        return "unknown error type"
    }
}

// I am a firm believer that three spaced tabs are aesthetically pleasing for Lua code.
const INDENT_WIDTH: usize = 3;

struct DumpContextImpl<'a, 'b> {
    path: &'a [usize],
    context: &'a ViewContext<'a>,
    writer: &'b mut std::io::Write,
    indent_level: Vec<u8>,
    prototypes: &'a [std::thread::Result<DecompiledPrototype<'a>>],
    upvalues: Option<Vec<Cow<'a, str>>>
}

impl<'a, 'b> DumpContextImpl<'a, 'b> {
    fn format_reg_into<W: std::io::Write>(&self, reg: Reg, writer: &mut W) {
        write!(writer, "r{}_", reg.0).unwrap();

        if self.path.is_empty() {
            writer.write(b"rt").unwrap();
        } else {
            writer.write(b"pr").unwrap();
            for (i, idx) in self.path.iter().enumerate() {
                if i != 0 {
                    writer.write(b"_").unwrap();
                }
                write!(writer, "{}", idx).unwrap();
            }
        }
    }
}

impl<'a, 'b> DumpContext for DumpContextImpl<'a, 'b> {
    fn write_root(&mut self) {
        let root_views: Vec<&View> = self.context.iter_root().collect();
        for (i, view) in root_views.iter().rev().enumerate() {
            if i != 0 {
                self.write_newline();
            }
            self.write_view(view.index, DumpType::Statement);
        }
    }
    
    fn write_str(&mut self, s: &str) {
        write!(self.writer, "{}", s).unwrap();
    }

    fn write_view(&mut self, index: ViewRef, typ: DumpType) {
        let view = &self.context.view_at(index);

        // write!(self.writer, "--[[{:?}]]", view).unwrap();

        if let DumpType::Statement = typ {
            let free_mark = &self.context.view_free_mark_at(index);

            if let &ViewType::Expression { dest, .. } = &view.view_type {
                if free_mark.incoming_root != free_mark.outgoing {
                    self.write_str("local ");
                }
                self.write_reg(dest);
                self.write_str(" = ");
            } else if let &ViewType::PinnedExpression { dest } = &view.view_type {
                if free_mark.incoming_root != free_mark.outgoing {
                    self.write_str("local ");
                }
                self.write_reg(dest);
                self.write_str(" = ");
            } else if let &ViewType::MultiExpression { base, count } = &view.view_type {
                self.write_str("local ");
                let mut reg = base;
                let top = (base + count).unwrap();
                while reg != top {
                    if reg != base {
                        self.write_str(", ");
                    }
                    self.write_reg(reg);
                    reg = reg.next();
                }
                self.write_str(" = ");
            }
        }

        view.data.dump(self, typ)
    }

    fn write_newline(&mut self) {
        self.writer.write(b"\n").unwrap();
        self.writer.write(&self.indent_level).unwrap();
    }

    fn write_name(&mut self, kst: Kst) {
        let constant = &self.context.chunk().constants[kst.0 as usize];
        match constant {
            &LuaConstant::String(ref data) => self.writer.write(data),
            _ => self.writer.write(b"`write_name called on non-string constant`")
        }.unwrap();
    }

    fn write_constant(&mut self, kst: Kst) {
        let constant = &self.context.chunk().constants[kst.0 as usize];
        match constant {
            &LuaConstant::Null => { self.writer.write(b"nil").unwrap(); },
            &LuaConstant::Bool(b) => write!(self.writer, "{}", b).unwrap(),
            &LuaConstant::Number(LuaNumber::Floating(f)) => write!(self.writer, "{}", f).unwrap(),
            &LuaConstant::Number(LuaNumber::Integral(i)) => write!(self.writer, "{}", i).unwrap(),
            &LuaConstant::String(ref data) => {
                self.writer.write(b"\"").unwrap();
                self.writer.write(data).unwrap();
                self.writer.write(b"\"").unwrap();
            },
        };
    }

    fn is_valid_name(&self, kst: Kst) -> bool {
        let constant = &self.context.chunk().constants[kst.0 as usize];
        match constant {
            &LuaConstant::String(ref data) => is_valid_lua_identifier(data),
            _ => false
        }
    }

    fn write_reg(&mut self, reg: Reg) {
        let chunk = self.context.chunk();
        if let Some(LuaVarArgInfo { has_arg: true, .. }) = chunk.is_vararg {
            if reg.0 == chunk.num_params {
                // Lua 5.1's compatibility `arg` parameter
                self.write_str("arg");
                return
            }
        }

        write!(self.writer, "r{}_", reg.0).unwrap();

        if self.path.is_empty() {
            self.write_str("rt");
        } else {
            self.write_str("pr");
            for (i, idx) in self.path.iter().enumerate() {
                if i != 0 {
                    self.write_str("_");
                }
                write!(self.writer, "{}", idx).unwrap();
            }
        }
    }

    fn write_proto(&mut self, proto_index: u32, captures: &[ClosureCapture]) {
        let proto_context = &self.prototypes[proto_index as usize];

        match proto_context {
            Ok(proto) => {
                let mut upvalues = Vec::with_capacity(captures.len());
                for capture in captures {
                    match capture {
                        &ClosureCapture::Register(reg) => {
                            let mut s: Vec<u8> = Vec::new();
                            self.format_reg_into(reg, &mut s);
                            upvalues.push(Cow::Owned(String::from_utf8(s).unwrap()))
                        },
                        &ClosureCapture::Upvalue(uv) => {
                            use std::borrow::Borrow;
                            let upval_cow = &self.upvalues.as_ref().unwrap()[uv.0 as usize];
                            upvalues.push(Cow::Borrowed(upval_cow.borrow()))
                        }
                    }
                }

                let mut inner_dumper = DumpContextImpl {
                    path: &proto.path,
                    context: &proto.context,
                    writer: self.writer,
                    indent_level: self.indent_level.clone(),
                    prototypes: &proto.children,
                    upvalues: Some(upvalues),
                };

                inner_dumper.write_str("function(");
                for i in 0u8..proto.context.chunk().num_params {
                    if i != 0 {
                        inner_dumper.write_str(", ");
                    }
                    inner_dumper.write_reg(Reg(i));
                }
                if proto.context.chunk().is_vararg.is_some() {
                    inner_dumper.write_str(", ...");
                }
                inner_dumper.write_str(")");
                write!(inner_dumper.writer, " -- proto {}", proto_index).unwrap();
                inner_dumper.write_newline();
                inner_dumper.indent();

                inner_dumper.write_root();

                inner_dumper.unindent();
                inner_dumper.write_newline();
                inner_dumper.write_str("end");
            },
            Err(err) => write!(self.writer, "!Failed to decompile prototype {}: {}!", proto_index, try_get_str_error(err)).unwrap(),
        }
    }

    fn write_upvalue(&mut self, upvalue: Upvalue) {
        if let Some(ref upvalues) = self.upvalues {
            use std::borrow::Borrow;
            let uv: &str = upvalues[upvalue.0 as usize].borrow();
            self.writer.write(uv.as_bytes()).unwrap();
        } else {
            write!(self.writer, "u{}", upvalue.0).unwrap();
        }
    }

    fn indent(&mut self) {
        let old_len = self.indent_level.len();
        for _ in 0..INDENT_WIDTH {
            self.indent_level.push(b' ');
        }
        self.writer.write(&self.indent_level[old_len..]).unwrap();
    }

    fn unindent(&mut self) {
        let len = self.indent_level.len();
        self.indent_level.truncate(len - INDENT_WIDTH);
    }
}

struct DecompiledPrototype<'a> {
    path: Vec<usize>,
    context: ViewContext<'a>,
    children: Vec<std::thread::Result<DecompiledPrototype<'a>>>
}

impl<'a> DecompiledPrototype<'a> {
    fn dump(&self, writer: &mut std::io::Write) {
        let mut dumper = DumpContextImpl {
            path: &self.path,
            context: &self.context,
            writer,
            indent_level: vec![],
            prototypes: &self.children,
            upvalues: None,
        };

        dumper.write_root();
        dumper.write_newline();
    }
}

fn decompile_chunk<'a>(chunk: &'a LuaChunk, path: &Vec<usize>) -> DecompiledPrototype<'a> {
    // Depth first traversal into prototype chunks
    let mut proto_path = path.clone();

    let protos: Vec<_> = chunk.prototypes.iter()
        .enumerate()
        .map(|(i, proto)| {
            proto_path.push(i);
            let res = std::panic::catch_unwind(|| {
                decompile_chunk(proto, &proto_path)
            });
            proto_path.clone_from(path);
            res
        })
        .collect();
    drop(proto_path);

    info!("{:?}", path);
    let mut context = ViewContext::new(chunk);

    context.decompile();

    DecompiledPrototype {
        path: path.clone(),
        context,
        children: protos
    }
}

fn main() {
    env_logger::Builder::from_default_env()
        .default_format_timestamp(false)
        .init();

    // let compiled = compile_lua("while x do print(123) end").unwrap();
    // let compiled = compile_lua("while x do while y do print(123) end end").unwrap();
    // let compiled = compile_lua("while true do while x do print(123) end end").unwrap();
    // let compiled = compile_lua("while true do while true do if x then print(123) else print(456) end end end").unwrap();
    // let compiled = compile_lua("if x and y then print(123) end").unwrap();
    // let compiled = compile_lua("if x and y then print(123) else print(456) end").unwrap();
    // let compiled = compile_lua("print(123)").unwrap();
    // let compiled = compile_lua("while x do if y then print(123) else print(456) end end").unwrap();
    // let compiled = compile_lua("print(x + y)").unwrap();
    // let compiled = compile_lua("local x = 3 print(x + y)").unwrap();
    // let compiled = compile_lua("x.a, x.b[1], x = 'x.a', 'x.b[1]', 'x'").unwrap();
    // let compiled = compile_lua("x, x.a, x.b[1] = 'x', 'x.a', x.y + 'x.b[1]'").unwrap();
    /*let compiled = compile_lua(r#"
local function proto0(self, show)
    if show then
        local player = Universe:GetPlayerGameObject()
        local region = player:GetClosestBuildableRegion(10.0)
        Classes.DebugDisplayManager:Instance(BuildableRegion.kDebugTextStartX, BuildableRegion.kDebugTextStartY, BuildableRegion.kDebugTextContextName, region) -- , {region})
    else
        local instance = Classes.DebugDisplayManager.GetInstance(BuildableRegion.kDebugTextContextName)
        if instance ~= nil then
            instance:Destroy()
        end
    end
end
    "#).unwrap();*/
    /*let compiled = compile_lua(r#"
  if a and b and c or d then
  end
  
  if b or c and q or e then
  end
  
  if ((b or c) and q) or e then
  end
  
  if a or b then end
  
  if (a or b) and c or d then end
  
  if g then
    print("G")
  else
    print("!G")
  end
  
  while q or b do print("X") end
  
  while true do end
  
  while q or b do
    print("X")
  
    while b do end
  end
  
  while qq or bb do
    print("QQ")
  
    while bb do print("BB") end
  end
  
  while e do
    print("E")
  
    if m then
      print("M AND E")
      if p then
        print("P!!")
      end
    end
  end
  
  while q do
    if p then
      print("P AND Q")
    else
      print("ONLY Q")
    end
  end
  
  while q do
    if p then
      print("P AND Q")
    else
      print("ONLY Q")
    end
    while true do end
  end
"#).unwrap();*/

    let matches = clap_app!(lbcdec =>
        (version: "0.1")
        (author: "Dwayne Slater <ds84182@gmail.com>")
        (about: "Decompiles Lua 5.1 bytecode")
        (@subcommand script =>
            (about: "Loads a Lua 5.1 script from the given file, compiles it with the system's luac5.1, and then decompiles it")
            (@arg INPUT: +required "The input file to use")
        )
        (@subcommand bytecode =>
            (about: "Loads Lua 5.1 bytecode from the given file and decompiles it")
            (@arg INPUT: +required "The input file to use")
            (@subcommand bulk =>
                (about: "Decompiles Lua 5.1 bytecode in bulk")
                // (@arg INPUT: +required "The input file to use, glob patterns are also supported")
                (@arg OUTPUT: "The output directory")
            )
        )
    ).get_matches();

    // TODO: Ability to specify chunk path on command line

    fn decompile_bytecode_to(compiled: &[u8], writer: &mut std::io::Write) {
        let bytecode = lua_bytecode(&compiled);
        if let nom::IResult::Done(_, LuaBytecode { main_chunk: ref chunk, .. }) = bytecode {
            decompile_chunk(&chunk, &vec![]).dump(writer);
        }
    }

    fn decompile_bytecode(compiled: &[u8]) {
        decompile_bytecode_to(compiled, &mut std::io::stdout());
    }

    match matches.subcommand() {
        ("script", Some(matches)) => {
            decompile_bytecode(&{
                if let Some(script_path) = matches.value_of("INPUT") {
                    let script = fs::read_to_string(script_path).unwrap();
                    compile_lua(&script).unwrap()
                } else {
                    error!("Script path not specified");
                    return;
                }
            })
        },
        ("bytecode", Some(matches)) => {
            match matches.subcommand() {
                ("bulk", Some(sub_matches)) => {
                    if let (Some(bytecode_glob), Some(output_path)) = (matches.value_of("INPUT"), sub_matches.value_of("OUTPUT")) {
                        let path_iter = glob::glob(bytecode_glob).unwrap();

                        use rayon::prelude::*;

                        let bytecode_files: Vec<_> = path_iter.filter_map(|file| {
                            match file {
                                Ok(f) => Some(f),
                                Err(e) => {
                                    error!("Globbing failed: {}", e);
                                    None
                                }
                            }
                        }).collect();

                        let output_path = std::path::PathBuf::from(output_path);

                        bytecode_files.par_iter().for_each(move |file| {
                            let bytecode = fs::read(file).unwrap();
                            let mut output = output_path.clone();
                            output.push(file.file_name().unwrap());
                            output.set_extension("lua");

                            let res = std::panic::catch_unwind(|| {
                                let mut output_writer = fs::File::create(&output).unwrap();
                                decompile_bytecode_to(&bytecode, &mut output_writer);
                            });

                            if let Err(err) = res {
                                use std::io::Write;
                                let mut output_writer = fs::File::create(output).unwrap();
                                writeln!(&mut output_writer, "Failed to decompile file at root level: {}", try_get_str_error(&err)).unwrap();
                            }
                        });
                    } else {
                        unimplemented!()
                    }
                },
                _ => {
                    decompile_bytecode(&{
                        if let Some(bytecode_path) = matches.value_of("INPUT") {
                            fs::read(bytecode_path).unwrap()
                        } else {
                            error!("Bytecode path not specified");
                            return;
                        }
                    })
                }
            }
        },
        _ => unreachable!()
    }
}
