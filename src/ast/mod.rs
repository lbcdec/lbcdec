// Abstract Syntax Tree for Lua source code

// Design: Each block container and expression container has a list of either statements or expressions, and a list of ids for data that is currently live.

use std;
use obstack;
use obstack::{Obstack};

struct ASTContext {
    statement_stack: Obstack,
    expr_stack: Obstack,
    name_stack: Obstack,
    local_stack: Obstack,
}

struct Scope<'parent, 'ctx: 'parent> {
    parent: &'parent Scope<'parent, 'ctx>,
    locals: std::collections::HashMap<&'ctx str, ()>,
}

// BLOCK = [STATEMENT[;]?]* [LAST_STATEMENT[;]?]?
// CHUNK = BLOCK
struct Block<'ctx> {
    statements: Vec<&'ctx StatementContainer<'ctx>>,
    // last_statement: &'ctx LastStatement
}

// FUNCNAME = NAME [.NAME]* [:NAME]?

struct Node<T: Sized> {
    data: T,
    children: usize,
}

trait StatementContainer<'ctx> {}

struct StatementNode<'ctx, T: Sized> {
    statement: T,
    previous: Option<&'ctx StatementContainer<'ctx>>,
    next: Option<&'ctx StatementContainer<'ctx>>,
}

impl<'ctx, T: Sized> StatementContainer<'ctx> for StatementNode<'ctx, T> {

}

// STATEMENT = {
    // LOCAL_DECL = local NAME[, NAME]* [= EXPR[, EXPR]*]?
pub struct StatLocalDecl { // <'ctx> {
    // names: 
}
    // CALL = PRIMARYEXP (if call)
    // ASSIGN = PRIMARYEXP[, PRIMARYEXP]* = EXPLIST1
    // LOCAL_ASSIGN_FUNC = local function NAME BODY
    // ASSIGN_FUNC = function FUNCNAME BODY
    // DO = do BLOCK end
    // IF = if CONDEXPR then BLOCK [elseif CONDEXPR then BLOCK]* [else BLOCK]? end
    // WHILE = while CONDEXPR do BLOCK end
    // NFOR = for NAME = EXPR, EXPR[, EXPR]? do BLOCK end
    // GFOR = for NAME[, NAME]* in EXPR[, EXPR]* do BLOCK end
    // REPEAT = repeat BLOCK until CONDEXPR
// }
pub enum Statement<'ctx> {
    LocalDecl { },
    Do { block: Block<'ctx> }

    // LAST_STATEMENT = {
        // RETURN = return [EXPR[, EXPR]*]?
        // BREAK = break
    // }

    // FIELD = [.|:]NAME
    // INDEX = [EXPR]
    
    // TABLE_HASH_FIELD = (NAME | INDEX) = EXPR
    // TABLE_ARRAY_FIELD = EXPR
    // TABLE = {(TABLE_HASH_FIELD | TABLE_ARRAY_FIELD)*}

    // PARAM = (NAME | ...)
    // PARLIST = [PARAM[, PARAM]*]? (... always last)
    // BODY = ( PARLIST ) CHUNK end

    // EXPLIST1 = EXPR[, EXPR]*
    // FUNCARGS = ( ([EXPLIST1]?) | TABLE | STRING )

    // PREFIXEXP = NAME | (EXPR)
    // PRIMARYEXP = PREFIXEXP ( .NAME | INDEX | : NAME FUNCARGS | FUNCARGS )
    // SIMPLEEXP = NUMBER | STRING | NIL | true | false | ... | TABLE | function BODY | PRIMARYEXP
    // SUBEXPR(lim) = (SIMPLEEXP | UNOP SUBEXPR) [BINOP(left > lim) SUBEXPR(BINOP(left > lim).right)]
    // EXPR = SUBEXPR(0)
}
