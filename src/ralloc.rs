// Generic reverse register allocator
// Attempts to derive meaning from register allocation information
// Takes a list of instructions and returns a list of information for each instruction

use crate::instruction_definitions::{LuaInstruction, InstructionInfo, UsageType, Count, RegOrTop, Reg, RK};

#[derive(Debug, PartialEq, Eq)]
pub enum InstructionClass {
    /// Stack-based instructions "pop" a specified number of arguments, off the stack at their base
    /// Afterwards, they may "push" a specified number of return results, leaving the rest of the register
    /// stack undefined.
    /// Some instructions may choose to output like a free-form instruction (see CONCAT).
    Stack,
    /// Free-form instructions can use any allocated register as input and output to any allocated register (or next free register).
    FreeForm
}

impl Default for InstructionClass {
    fn default() -> Self {
        InstructionClass::FreeForm
    }
}

#[derive(Debug, PartialEq, Eq)]
enum FreeFormRegType {
    Incoming,
    Outgoing
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum FreeFormReg {
    None,
    Initial(Reg),
    Concrete(Reg),
}

impl FreeFormReg {
    fn is_none(&self) -> bool {
        match self {
            FreeFormReg::None => true,
            _ => false,
        }
    }

    fn as_reg(&self) -> Option<Reg> {
        match *self {
            FreeFormReg::None => None,
            FreeFormReg::Initial(reg) | FreeFormReg::Concrete(reg) => Some(reg),
        }
    }
}

#[derive(Debug)]
struct FreeFormData {
    incoming: FreeFormReg,
    outgoing: FreeFormReg,
}

#[derive(Debug)]
struct StackData {
    incoming: RegOrTop,
    outgoing: RegOrTop,
    info: InstructionInfo,
}

#[derive(Debug)]
enum Data {
    FreeForm(FreeFormData),
    Stack(StackData),
}

impl Data {
    fn stack(&self) -> Option<&StackData> {
        if let Data::Stack(data) = self {
            Some(data)
        } else {
            None
        }
    }

    fn free_form(&self) -> Option<&FreeFormData> {
        if let Data::FreeForm(data) = self {
            Some(data)
        } else {
            None
        }
    }

    fn stack_mut(&mut self) -> Option<&mut StackData> {
        if let Data::Stack(data) = self {
            Some(data)
        } else {
            None
        }
    }

    fn free_form_mut(&mut self) -> Option<&mut FreeFormData> {
        if let Data::FreeForm(data) = self {
            Some(data)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct Info {
    data: Data,
    redirect: Option<usize>,
}

impl Info {
    fn class(&self) -> InstructionClass {
        match self.data {
            Data::FreeForm(..) => InstructionClass::FreeForm,
            Data::Stack(..) => InstructionClass::Stack,
        }
    }

    fn pc(&self, pc: usize) -> usize {
        self.redirect.unwrap_or(pc)
    }

    fn incoming(&self) -> Option<Reg> {
        match &self.data {
            Data::FreeForm(data) => match data.incoming {
                FreeFormReg::None => None,
                FreeFormReg::Initial(reg) => Some(reg),
                FreeFormReg::Concrete(reg) => Some(reg),
            },
            Data::Stack(data) => match data.incoming {
                RegOrTop::Reg(reg) => Some(reg),
                RegOrTop::Top => panic!("Use incoming_or_top for varargs support"),
            }
        }
    }

    fn incoming_or_top(&self) -> Option<RegOrTop> {
        match &self.data {
            Data::FreeForm(data) => match data.incoming {
                FreeFormReg::None => None,
                FreeFormReg::Initial(reg) => Some(RegOrTop::Reg(reg)),
                FreeFormReg::Concrete(reg) => Some(RegOrTop::Reg(reg)),
            },
            Data::Stack(data) => Some(data.incoming)
        }
    }

    fn outgoing(&self) -> Option<Reg> {
        match &self.data {
            Data::FreeForm(data) => match data.outgoing {
                FreeFormReg::None => None,
                FreeFormReg::Initial(reg) => Some(reg),
                FreeFormReg::Concrete(reg) => Some(reg),
            },
            Data::Stack(data) => match data.outgoing {
                RegOrTop::Reg(reg) => Some(reg),
                RegOrTop::Top => panic!("Use outgoing_or_top for varargs support"),
            }
        }
    }

    fn outgoing_or_top(&self) -> Option<RegOrTop> {
        match &self.data {
            Data::FreeForm(data) => match data.outgoing {
                FreeFormReg::None => None,
                FreeFormReg::Initial(reg) => Some(RegOrTop::Reg(reg)),
                FreeFormReg::Concrete(reg) => Some(RegOrTop::Reg(reg)),
            },
            Data::Stack(data) => Some(data.outgoing)
        }
    }
}

/// Stack instructions are common, they control the placement of scopes.
/// Without them, we can't determine anything.

#[derive(Default, Debug)]
pub struct InstrInfo {
    class: InstructionClass,
    incoming: Option<RegOrTop>,
    outgoing: Option<RegOrTop>,
    info: Option<InstructionInfo>,
    in_newtable: bool,
    redirect: Option<usize>,
}

struct RAllocCtx<'a> {
    decoded: &'a [LuaInstruction],
    info: Vec<Info>,
}

pub struct RAllocData {
    info: Vec<Info>,
}

impl RAllocData {
    pub fn incoming_free_mark_at(&self, pc: usize) -> Option<Reg> {
        self.info[pc].incoming_or_top().and_then(|rt| match rt {
            RegOrTop::Reg(reg) => Some(reg),
            _ => None
        })
    }
}

impl<'a> RAllocCtx<'a> {
    fn run(mut self) -> RAllocData {
        // First, find all stack based instructions and fill out their instruction info
        for (i, instr) in self.decoded.iter().enumerate() {
            let instr_info = instr.info();
            let data = match instr_info {
                Some(info) => {
                    let (incoming, outgoing) = {
                        let InstructionInfo::Stack { base, inputs, outputs } = info;
                        ((base + inputs).into(), (base + outputs).into())
                    };
                    Data::Stack(StackData {
                        incoming,
                        outgoing,
                        info
                    })
                },
                None => {
                    let last = i == self.decoded.len()-1;

                    let incoming;
                    let outgoing;

                    if !last {
                        let (top_read, base_write) = (instr.top_read().map(|r| r.next()), instr.base_write());
                        incoming = match (top_read, base_write) {
                            (Some(tr), Some(bw)) if bw.is_above(tr) => Some(bw),
                            (None, Some(bw)) => Some(bw),
                            (tr, ..) => tr,
                        };
                        outgoing = match (incoming, base_write.map(|r| r.next())) {
                            (Some(incoming), Some(bw)) if bw.is_above(incoming) => Some(bw),
                            (None, Some(bw)) => Some(bw),
                            (incoming, ..) => incoming,
                        };
                    } else {
                        // Last instruction must have an incoming of 0 and an outgoing of 0
                        incoming = Some(Reg(0));
                        outgoing = Some(Reg(0));
                    }

                    println!("{} {:?} {:?}", i, incoming, outgoing);

                    Data::FreeForm(FreeFormData {
                        incoming: incoming.map(FreeFormReg::Initial).unwrap_or(FreeFormReg::None),
                        outgoing: outgoing.map(FreeFormReg::Initial).unwrap_or(FreeFormReg::None),
                    })
                },
            };
            self.info.push(Info {
                data,
                redirect: None
            });
        }

        // Find forward jumps to TFORLOOP instructions and swap their instruction info
        for (i, instr) in self.decoded.iter().enumerate() {
            if let LuaInstruction::Jump { offset } = *instr {
                let target = offset.apply(i as u32) as usize;
                if let LuaInstruction::TForLoop {..} = self.decoded[target] {
                    if self.info[target].redirect.is_none() {
                        self.info.swap(i, target);
                        self.info[i].redirect = Some(target);
                        self.info[target].redirect = Some(i);
                        self.info[target].data = {
                            let info = self.decoded[target].info().unwrap();
                            let (incoming, outgoing) = {
                                let InstructionInfo::Stack { base, outputs, .. } = info;
                                ((base + outputs).into(), (base + outputs).into())
                            };
                            Data::Stack(StackData {
                                incoming,
                                outgoing,
                                info
                            })
                        };
                    }
                }
            }
        }

        // Fix outgoing for CONCAT instructions
        for (i, instr) in self.decoded.iter().enumerate() {
            if let LuaInstruction::Concat { dest, .. } = *instr {
                let stack_data = self.info[i].data.stack_mut().unwrap();
                if let RegOrTop::Reg(ref mut reg) = stack_data.outgoing {
                    if dest.is_at_or_above(*reg) {
                        *reg = dest.next();
                    }
                }
            }
        }

        // Downwards propagation: instructions going to our current instruction
        // Upwards propagation: instructions our current instruction is going to
        // We generate a graph (!!!) so we can determine the order for both upwards and downwards inference
        // Free mark is determined (for indeterminates) by using upward and downward inference, the one with the lowest concrete base (which is still valid for the instruction, looking at its inputs) wins
        let mut succ = vec![Vec::new(); self.decoded.len()];
        let mut pred = vec![Vec::new(); self.decoded.len()];
        for (i, instr) in self.decoded.iter().enumerate() {
            if i > 0 {
                let absolute_jump = match self.decoded[i - 1] {
                    LuaInstruction::Jump {..} | LuaInstruction::TailCall {..} | LuaInstruction::Return {..} | LuaInstruction::LoadBool { skip_next: true, .. } => {
                        // Always jumps, so it can't be a source of downwards propagation
                        true
                    },
                    _ => false,
                };
                if !absolute_jump {
                    pred[i].push(i - 1);
                    succ[i - 1].push(i);
                }
            }

            match instr {
                LuaInstruction::Jump { offset } | LuaInstruction::ForLoop { target: offset, .. } | LuaInstruction::ForPrep { target: offset, .. } => {
                    let target = offset.apply(i as u32) as usize;
                    pred[target].push(i);
                    succ[i].push(target);
                },
                LuaInstruction::BinCondOp {..} | LuaInstruction::Test {..} | LuaInstruction::TestSet {..} | LuaInstruction::TForLoop {..} | LuaInstruction::LoadBool { skip_next: true, .. } => {
                    // May skip next instruction, may not
                    pred[i + 2].push(i);
                    succ[i].push(i + 2);
                },
                _ => {},
            }
        }

        // Get rid of dead instructions

        let mut worklist = WorkList::new(self.decoded.len(), true);
        let mut dead_list = vec![];
        loop {
            while let Some(pc) = worklist.next() {
                let mut changed = false;
                for &p in &pred[pc] {
                    if p != 0 && pred[p].is_empty() {
                        dead_list.push(p);
                        succ[p].clear();
                        changed = true;
                    }
                }
                for dead in dead_list.drain(..) {
                    let ind = pred[pc].iter().position(|v| *v == dead).unwrap();
                    pred[pc].swap_remove(ind);
                }
                dead_list.clear();
                if changed {
                    // Push our succ
                    for &s in &succ[pc] {
                        worklist.insert(s);
                    }
                }
                worklist.commit(pc);
            }
            if !worklist.flip() {
                break
            }
        }

        use std::collections::BTreeSet;
        struct WorkList {
            worklist: Vec<usize>,
            next_worklist: Vec<usize>,
            worklist_set: BTreeSet<usize>,
        }

        impl WorkList {
            fn new(items: usize, reversed: bool) -> WorkList {
                use std::iter::FromIterator;
                let worklist = if !reversed {
                    Vec::from_iter(0..items)
                } else {
                    Vec::from_iter((0..items).rev())
                };
                let next_worklist = Vec::new();
                let worklist_set = std::collections::BTreeSet::from_iter(0..items);
                WorkList { worklist, next_worklist, worklist_set }
            }

            fn flip(&mut self) -> bool {
                std::mem::swap(&mut self.worklist, &mut self.next_worklist);
                !self.worklist.is_empty()
            }

            fn next(&mut self) -> Option<usize> {
                self.worklist.pop()
            }

            fn commit(&mut self, item: usize) {
                self.worklist_set.remove(&item);
            }

            fn insert(&mut self, item: usize) {
                if self.worklist_set.insert(item) {
                    self.next_worklist.push(item)
                }
            }
        }

        let mut worklist = WorkList::new(self.decoded.len(), true);
        loop {
            while let Some(pc) = worklist.next() {
                let mut changed = false;
                if self.info[pc].class() == InstructionClass::FreeForm {
                    let pred = &pred[pc];
                    let redirect_pc = self.info[pc].redirect.unwrap_or(pc);
                    let our_outgoing = self.info[pc].outgoing();
                    // Derive incoming from predecessors, use lowest value
                    for &p in pred {
                        let outgoing = self.info[p].outgoing();
                        match outgoing {
                            Some(outgoing) => {
                                let info = self.info[pc].data.free_form_mut().unwrap();
                                let compatible = !self.decoded[redirect_pc].does_read_at_or_above(outgoing) && (our_outgoing.is_none() || Some(outgoing.next()) == our_outgoing || Some(outgoing) == our_outgoing);
                                match info.incoming {
                                    FreeFormReg::Initial(ref mut incoming) | FreeFormReg::Concrete(ref mut incoming) => {
                                        if incoming.is_above(outgoing) && compatible {
                                            #[cfg(debug_assertions)]
                                            println!("For {}, using pred {}, incoming = {:?} because old incoming > outgoing ({:?} > {:?})", pc, p, outgoing, incoming, outgoing);
                                            *incoming = outgoing;
                                            changed = true;
                                        }
                                    }
                                    _ => {
                                        if compatible {
                                            #[cfg(debug_assertions)]
                                            println!("For {}, using pred {}, incoming = {:?} because no incoming", p, pc, outgoing);
                                            info.incoming = FreeFormReg::Concrete(outgoing);
                                            changed = true;
                                        }
                                    }
                                }
                            },
                            _ => {}
                        }
                    }
                    // If this instruction writes, derive outgoing to be the write base, otherwise its the same as incoming
                    if let Some(incoming) = self.info[pc].incoming() {
                        let outgoing = if self.does_write(pc, incoming) {
                            incoming.next()
                        } else {
                            incoming
                        };
                        let info = self.info[pc].data.free_form_mut().unwrap();
                        let old = info.outgoing;
                        if old.is_none() {
                            let new = FreeFormReg::Concrete(outgoing);
                            info.outgoing = new;
                            if old != new {
                                changed = true;
                            }
                        }
                    }
                }
                if changed {
                    // Push our succ
                    for &s in &succ[pc] {
                        worklist.insert(s);
                    }
                }
                worklist.commit(pc);
            }
            if !worklist.flip() {
                break
            }
        }

        #[cfg(debug_assertions)]
        for (i, info) in self.info.iter().enumerate() {
            println!("{} {:?}: {:?} (p: {:?}, s: {:?})", i, self.decoded[i], (info.incoming_or_top(), info.outgoing_or_top()), pred[i], succ[i]);
        }

        let mut worklist = WorkList::new(self.decoded.len(), false);
        loop {
            while let Some(item) = worklist.next() {
                let mut changed = false;
                if self.info[item].class() == InstructionClass::FreeForm {
                    // For all succ
                    for &s in &succ[item] {
                        let incoming = self.info[s].incoming().unwrap();
                        let pc = self.info[item].pc(item);
                        let info = self.info[item].data.free_form_mut().unwrap();
                        let outgoing = info.outgoing.as_reg().unwrap();
                        if outgoing.is_above(incoming) && !self.decoded[pc].does_write_at_or_above(incoming) {
                            #[cfg(debug_assertions)]
                            println!("For {}, using succ {}, outgoing = {:?} because old outgoing > incoming ({:?} > {:?})", item, s, incoming, outgoing, incoming);
                            changed = true;
                            info.outgoing = FreeFormReg::Concrete(incoming);
                        } else if outgoing != incoming {
                            info.incoming = FreeFormReg::Concrete(incoming);
                            info.outgoing = FreeFormReg::Concrete(incoming);
                        }
                        // TODO: if our outgoing < succ incoming and its impossible for us to allocate those registers, take on succ incoming for both
                    }
                }
                if changed {
                    // Push our predecessors
                    for &p in &pred[item] {
                        worklist.insert(p);
                    }
                }
                worklist.commit(item);
            }
            if !worklist.flip() {
                break
            }
        }

        // Manually fix loadbool pairs
        for (i, instr) in self.decoded.iter().enumerate() {
            if let LuaInstruction::LoadBool { skip_next: true, .. } = *instr {
                if let LuaInstruction::LoadBool { skip_next: false, .. } = self.decoded[i + 1] {
                    {
                        let skipping = self.info[i].data.free_form_mut().unwrap();
                        skipping.incoming = skipping.outgoing;
                    }
                    {
                        let skipped = self.info[i + 1].data.free_form_mut().unwrap();
                        skipped.incoming = skipped.outgoing;
                    }
                }
            }
        }

        self.info.last_mut().unwrap().data = Data::FreeForm(FreeFormData {
            incoming: FreeFormReg::None,
            outgoing: FreeFormReg::None,
        });

        #[cfg(debug_assertions)]
        for (i, info) in self.info.iter().enumerate() {
            println!("{} {:?}: {:?} (p: {:?}, s: {:?})", i, self.decoded[i], info, pred[i], succ[i]);
        }

        RAllocData {
            info: self.info,
        }
    }

    fn does_read(&self, pc: usize, reg: Reg) -> bool {
        let pc = self.info[pc].pc(pc);
        self.decoded[pc].does_read(reg)
    }

    fn does_write(&self, pc: usize, reg: Reg) -> bool {
        let pc = self.info[pc].pc(pc);
        self.decoded[pc].does_write(reg)
    }
}

pub fn run(decoded: &[LuaInstruction]) -> RAllocData {
    let info = Vec::with_capacity(decoded.len());

    let ctx = RAllocCtx {
        decoded,
        info
    };
    ctx.run()
}
