use view::{ViewOrRegOrKst, ViewRef};
use dump::*;

#[derive(Debug, Clone)]
pub struct TableInfo {
    pub remaining_array: usize,
    pub remaining_hash: usize,
    pub entries: Vec<TableEntry>,
}

#[derive(Debug, Clone)]
pub enum TableEntry {
    Array(ViewRef), // View Index
    Hash {
        k: ViewOrRegOrKst,
        v: ViewOrRegOrKst
    }
}

impl TableInfo {
    pub fn dump(&self, context: &mut DumpContext) {
        context.write_str("{");
        context.write_newline();
        context.indent();
        for (i, entry) in self.entries.iter().enumerate() {
            if i != 0 {
                context.write_str(",");
                context.write_newline();
            }
            match entry {
                &TableEntry::Array(view) => context.write_view(view, DumpType::Expression),
                &TableEntry::Hash { k, v } => {
                    k.dump_index(context);
                    context.write_str(" = ");
                    v.dump(context);
                },
            }
        }
        context.unindent();
        context.write_newline();
        context.write_str("}");
    }
}
