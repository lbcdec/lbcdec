use std;

#[derive(Debug, Copy, Clone)]
pub struct CondTargetID(pub usize);

#[derive(Debug, Copy, Clone)]
pub struct CondGroupID(usize);

#[derive(Debug, Copy, Clone)]
pub enum CondTargetOrGroupID {
    Target(CondTargetID),
    Group(CondGroupID),
}

#[derive(Debug)]
pub struct CondTarget {
    id: CondTargetID,
    pc: u32,
    on_continue: u32,
    on_exit: u32,
    join: CondJoin,
    ctft: Option<CondTrueFalseTargets>
}

impl CondTarget {
    pub fn new(pc: u32, on_continue: u32, on_exit: u32) -> CondTarget {
        CondTarget {
            id: CondTargetID(usize::max_value()),
            pc: pc,
            on_continue: on_continue,
            on_exit: on_exit,
            join: CondJoin::Unspecified,
            ctft: None
        }
    }

    fn is_exit(&self, pc: u32) -> bool {
        pc == self.on_exit
    }
}

#[derive(Debug, Copy, Clone)]
pub enum CondJoin {
    And, Or, Unknown, Unspecified
}

#[derive(Debug)]
struct CondGroup {
    id: CondGroupID,
    self_target: CondTargetOrGroupID,
    targets: Vec<CondTargetOrGroupID>,
}

#[derive(Debug)]
enum CondTrueFalseTargets {
    Normal { on_true: u32, on_false: u32 },
    Ambiguous { possible_target: u32, prev_on_true: u32, prev_on_false: u32, ambiguous_group: CondGroupID },
    MultiCandidate { on_true: Vec<u32>, on_false: Vec<u32> },
}

enum CondLeftAddResult {
    Simple { ctft: CondTrueFalseTargets, join: CondJoin },
    AmbiguousFixup { ctft: CondTrueFalseTargets, join: CondJoin, ambiguous_fixup_ctft: CondTrueFalseTargets, ambiguous_fixup_join: CondJoin },
}

impl CondTrueFalseTargets {
    fn is_target_compatible(&self, target: &CondTarget) -> bool {
        use self::CondTrueFalseTargets::*;
        match self {
            &Normal { on_true, on_false } => target.is_exit(on_true) || target.is_exit(on_false),
            &Ambiguous { possible_target, prev_on_false, .. } => target.is_exit(possible_target) || target.is_exit(prev_on_false),
            &MultiCandidate { ref on_true, ref on_false } => on_true.iter().any(|&pc| target.is_exit(pc)) || on_false.iter().any(|&pc| target.is_exit(pc)),
        }
    }

    fn compute_join(exit_on_true: bool, exit_on_false: bool) -> CondJoin {
        if exit_on_true && exit_on_false {
            // Ambiguous, we'll find out later
            CondJoin::Unknown
        } else if exit_on_true {
            CondJoin::Or
        } else if exit_on_false {
            CondJoin::And
        } else {
            panic!("Invalid conditional join (doesn't exit on true or false)")
        }
    }

    fn join_to_ctft(group: CondGroupID, target: &CondTarget, join: CondJoin, on_true: u32, on_false: u32) -> CondTrueFalseTargets {
        use self::CondTrueFalseTargets::*;
        match join {
            CondJoin::And => Normal { on_true: target.on_continue, on_false: on_false },
            CondJoin::Or => Normal { on_true: on_true, on_false: target.on_continue },
            CondJoin::Unknown => Ambiguous { possible_target: target.on_continue, prev_on_true: on_true, prev_on_false: on_false, ambiguous_group: group },
            _ => panic!("Unhandled CondJoin")
        }
    }

    fn on_left_added(&self, context: &CondContext, group: &CondGroup, target: &CondTarget) -> CondLeftAddResult {
        use self::CondTrueFalseTargets::*;
        match self {
            &Normal { on_true, on_false } => {
                let join = Self::compute_join(target.is_exit(on_true), target.is_exit(on_false));
                let ctft = Self::join_to_ctft(group.id, target, join, on_true, on_false);
                CondLeftAddResult::Simple { ctft: ctft, join: join }
            },
            &Ambiguous { possible_target, prev_on_true, prev_on_false, ambiguous_group } => {
                let previous_join = if target.is_exit(possible_target) {
                    CondJoin::Or
                } else {
                    CondJoin::And
                };
                let previous_ctft = Self::join_to_ctft(ambiguous_group, context.target_of(context.to_target(group.self_target)), previous_join, prev_on_true, prev_on_false);
                let retry_result = previous_ctft.on_left_added(context, group, target);
                if let CondLeftAddResult::Simple { ctft, join } = retry_result {
                    CondLeftAddResult::AmbiguousFixup {
                        ctft: ctft,
                        join: join,
                        ambiguous_fixup_ctft: previous_ctft,
                        ambiguous_fixup_join: previous_join,
                    }
                } else {
                    panic!("Should not get ambiguous left add result from an ambiguous retry")
                }
            },
            &MultiCandidate { ref on_true, ref on_false } => {
                let exit_on_true = on_true.iter().any(|&pc| target.is_exit(pc));
                let exit_on_false = on_false.iter().any(|&pc| target.is_exit(pc));
                let join = Self::compute_join(exit_on_true, exit_on_false);

                let ctft = if exit_on_true && on_false.len() > 1 {
                    // Multiple onFalse candidates left over, use an invalid target for on_false
                    Self::join_to_ctft(group.id, target, join, target.on_exit, 0xFFFFFFFFu32)
                } else if exit_on_false && on_true.len() > 1 {
                    // Multiple onTrue candidates left over, use an invalid target for on_true
                    Self::join_to_ctft(group.id, target, join, 0xFFFFFFFFu32, target.on_exit)
                } else {
                    // Exact target
                    Self::join_to_ctft(group.id, target, join, if exit_on_true { target.on_exit } else { on_true[0] }, if exit_on_false { target.on_exit } else { on_false[0] })
                };

                CondLeftAddResult::Simple { ctft: ctft, join: join }
            }
        }
    }
}

pub struct CondContext {
    targets: Vec<CondTarget>,
    groups: Vec<CondGroup>,
    root_group: CondGroupID,
}

#[derive(Debug)]
pub enum CondInsertOp {
    AddLeft { group: CondGroupID },
    UpgradeLeftAddLeft { parent_group: CondGroupID },
    AddLeftToVirtualParentGroup { rhs_group: CondGroupID, parent_group: Option<CondGroupID> },
}

pub trait CondVisitor<T = ()> {
    type Item;
    fn on_group(&mut self, group: CondGroupID, target_count: usize, join: CondJoin, last_in_group: bool) -> Self::Item;
    fn on_target(&mut self, target: CondTargetID, join: CondJoin, last_in_group: bool) -> Self::Item;
}

impl CondContext {
    pub fn new(root_target: CondTarget, on_true: u32, on_false: u32) -> CondContext {
        CondContext {
            targets: vec![CondTarget { id: CondTargetID(0), ctft: Some(CondTrueFalseTargets::Normal { on_true, on_false }), .. root_target }],
            groups: vec![CondGroup { id: CondGroupID(0), self_target: CondTargetOrGroupID::Target(CondTargetID(0)), targets: vec![] }],
            root_group: CondGroupID(0),
        }
    }

    pub fn new_multi(root_target: CondTarget, on_true_a: u32, on_true_b: u32, on_false: u32) -> CondContext {
        CondContext {
            targets: vec![CondTarget { id: CondTargetID(0), ctft: Some(CondTrueFalseTargets::MultiCandidate { on_true: vec![on_true_a, on_true_b], on_false: vec![on_false] }), .. root_target }],
            groups: vec![CondGroup { id: CondGroupID(0), self_target: CondTargetOrGroupID::Target(CondTargetID(0)), targets: vec![] }],
            root_group: CondGroupID(0),
        }
    }

    fn target_of(&self, id: CondTargetID) -> &CondTarget {
        &self.targets[id.0]
    }

    fn target_of_mut(&mut self, id: CondTargetID) -> &mut CondTarget {
        &mut self.targets[id.0]
    }

    fn group_of(&self, id: CondGroupID) -> &CondGroup {
        &self.groups[id.0]
    }

    fn group_of_mut(&mut self, id: CondGroupID) -> &mut CondGroup {
        &mut self.groups[id.0]
    }

    fn to_target(&self, id: CondTargetOrGroupID) -> CondTargetID {
        match id {
            CondTargetOrGroupID::Target(target_id) => target_id,
            CondTargetOrGroupID::Group(group_id) => self.to_target(self.group_of(group_id).self_target),
        }
    }

    fn add_target(&mut self, target: CondTarget) -> CondTargetID {
        let id = CondTargetID(self.targets.len());
        self.targets.push(CondTarget { id: id, .. target });
        id
    }

    fn add_group(&mut self, target: CondTargetOrGroupID)  -> CondGroupID {
        let id = CondGroupID(self.groups.len());
        self.groups.push(CondGroup {
            id: id,
            self_target: target,
            targets: vec![]
        });
        id
    }

    fn insert_into(&self, group: &CondGroup, target: &CondTarget, parent_group: Option<CondGroupID>) -> Option<CondInsertOp> {
        trace!("Insert {:?} -> {:?}", target, group);

        if !group.targets.is_empty() {
            trace!("More than one target");

            // Test LHS Exit Compatibility
            if group.targets.iter().all(|&id| self.target_of(self.to_target(id)).on_exit == target.on_exit) {
                trace!("Other targets are compatible, addLeft");
                return Some(CondInsertOp::AddLeft { group: group.id });
            }

            // LHS Propagate
            let lhs_target = group.targets[0];
            let lhs = self.target_of(self.to_target(lhs_target));
            if let &Some(ref lhs_ctft) = &lhs.ctft {
                if lhs_ctft.is_target_compatible(target) {
                    trace!("LHS Propagate");
                    return Some(CondInsertOp::UpgradeLeftAddLeft { parent_group: group.id });
                }
            }

            trace!("LHS Propagate failed");

            // Try LHS Insert
            if let CondTargetOrGroupID::Group(group_id) = lhs_target {
                trace!("Try LHS Insert");
                if let Some(op) = self.insert_into(self.group_of(group_id), target, Some(group.id)) {
                    trace!("LHS Insert ok");
                    return Some(op);
                }
                trace!("LHS Insert failed");
            }

            // RHS VPG
            if let &Some(ref group_ctft) = &self.target_of(self.to_target(group.self_target)).ctft {
                if group_ctft.is_target_compatible(target) {
                    trace!("Target is compatible if we were the RHS of a virtual parent group");
                    return Some(CondInsertOp::AddLeftToVirtualParentGroup { rhs_group: group.id, parent_group: parent_group });
                }
            }
        } else {
            trace!("No targets, trying to add if compatible");

            if let &Some(ref group_ctft) = &self.target_of(self.to_target(group.self_target)).ctft {
                if group_ctft.is_target_compatible(target) {
                    trace!("Target is compatible, adding");
                    return Some(CondInsertOp::AddLeft { group: group.id });
                }
            }
        }

        trace!("Insert failed");
        return None;
    }

    pub fn try_insert(&self, target: &CondTarget) -> Option<CondInsertOp> {
        self.insert_into(self.group_of(self.root_group), target, None)
    }

    fn process_on_left_add(&mut self, group: CondGroupID, target: CondTargetID) {
        let group_as_target = self.to_target(CondTargetOrGroupID::Group(group));
        let result = {
            let group_target_ref = self.target_of(group_as_target);
            let group_target_ctft = group_target_ref.ctft.as_ref().unwrap();
            group_target_ctft.on_left_added(self, self.group_of(group), self.target_of(target))
        };
        match result {
            CondLeftAddResult::Simple { ctft, join } => {
                let mut target_ref_mut = self.target_of_mut(target);
                target_ref_mut.ctft = Some(ctft);
                target_ref_mut.join = join;
            },
            CondLeftAddResult::AmbiguousFixup { ctft, join, ambiguous_fixup_ctft, ambiguous_fixup_join } => {
                {
                    let mut target_ref_mut = self.target_of_mut(target);
                    target_ref_mut.ctft = Some(ctft);
                    target_ref_mut.join = join;
                }

                {
                    let mut group_target_ref_mut = self.target_of_mut(group_as_target);
                    group_target_ref_mut.ctft = Some(ambiguous_fixup_ctft);
                    group_target_ref_mut.join = ambiguous_fixup_join;
                }
            }
        }
    }

    fn apply_internal(&mut self, op: &CondInsertOp, target: CondTargetID) {
        use self::CondInsertOp::*;
        match op {
            &AddLeft { group } => {
                self.group_of_mut(group).targets.insert(0, CondTargetOrGroupID::Target(target));
                self.process_on_left_add(group, target);
            },
            &UpgradeLeftAddLeft { parent_group } => {
                // Upgrade the target to a group
                let new_group = match self.group_of(parent_group).targets[0] {
                    CondTargetOrGroupID::Target(lhs_target) => {
                        let new_group = self.add_group(CondTargetOrGroupID::Target(lhs_target));
                        self.group_of_mut(parent_group).targets[0] = CondTargetOrGroupID::Group(new_group);
                        new_group
                    },
                    CondTargetOrGroupID::Group(group) => group,
                };

                // Insert into the group
                self.group_of_mut(new_group).targets.insert(0, CondTargetOrGroupID::Target(target));

                self.process_on_left_add(new_group, target);
            },
            &AddLeftToVirtualParentGroup { rhs_group, parent_group } => {
                let new_group = self.add_group(CondTargetOrGroupID::Group(rhs_group));
                self.group_of_mut(new_group).targets.insert(0, CondTargetOrGroupID::Target(target));

                if let Some(parent) = parent_group {
                    // Fixup parent
                    self.group_of_mut(parent).targets[0] = CondTargetOrGroupID::Group(new_group);
                } else {
                    self.root_group = new_group;
                }

                self.process_on_left_add(new_group, target);
            }
        }
    }

    pub fn apply(&mut self, op: &CondInsertOp, target: CondTarget) {
        // First, insert the target and create a target ID
        let id = self.add_target(target);
        self.apply_internal(op, id);
    }

    fn dump_target<T: std::io::Write>(&self, out: &mut T, target: CondTargetID) -> std::io::Result<()> {
        let target_ref = self.target_of(target);
        write!(out, "<{}>", target_ref.pc)?;
        Ok(())
    }

    fn dump_target_join<T: std::io::Write>(&self, out: &mut T, target: CondTargetID) -> std::io::Result<()> {
        let target_ref = self.target_of(target);
        Ok(match target_ref.join {
            CondJoin::And => {
                out.write(b" and ")?;
            },
            CondJoin::Or => {
                out.write(b" or ")?;
            },
            CondJoin::Unknown => {
                out.write(b" unknown ")?;
            },
            CondJoin::Unspecified => {
                out.write(b" error ")?;
            }
        })
    }

    fn dump_group<T: std::io::Write>(&self, out: &mut T, group: CondGroupID) -> std::io::Result<()> {
        let group_ref = self.group_of(group);
        for &target in &group_ref.targets {
            match target {
                CondTargetOrGroupID::Target(target) => {
                    self.dump_target(out, target)?;
                    self.dump_target_join(out, target)?;
                },
                CondTargetOrGroupID::Group(group) => {
                    out.write(b"(")?;
                    self.dump_group(out, group)?;
                    out.write(b")")?;
                    self.dump_target_join(out, self.to_target(target))?;
                }
            }
        }

        match group_ref.self_target {
            CondTargetOrGroupID::Target(target) => {
                self.dump_target(out, target)?;
            },
            CondTargetOrGroupID::Group(group) => {
                out.write(b"(")?;
                self.dump_group(out, group)?;
                out.write(b")")?;
            }
        }
        
        Ok(())
    }

    pub fn dump<T: std::io::Write>(&self, out: &mut T) -> std::io::Result<()> {
        self.dump_group(out, self.root_group)
    }

    pub fn visit<T: CondVisitor>(&self, visitor: &mut T) -> T::Item {
        self.visit_group(self.root_group, visitor, true)
    }

    fn visit_group<T: CondVisitor>(&self, group: CondGroupID, visitor: &mut T, last_in_group: bool) -> T::Item {
        let group_ref = self.group_of(group);
        let target = self.target_of(self.to_target(group_ref.self_target));
        visitor.on_group(group, group_ref.targets.len() + 1, target.join, last_in_group)
    }

    pub fn visit_group_target<T: CondVisitor>(&self, group: CondGroupID, index: usize, visitor: &mut T) -> T::Item {
        let group_ref = self.group_of(group);
        let last = index == group_ref.targets.len();
        let target = if last {
            group_ref.self_target
        } else {
            group_ref.targets[index]
        };

        match target {
            CondTargetOrGroupID::Target(id) => {
                visitor.on_target(id, self.target_of(id).join, last)
            },
            CondTargetOrGroupID::Group(id) => self.visit_group(id, visitor, last),
        }
    }
}

impl std::fmt::Debug for CondContext {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        let mut buf = vec![];
        self.dump(&mut buf).unwrap();
        f.write_str(std::str::from_utf8(&buf[..]).unwrap())
    }
}
