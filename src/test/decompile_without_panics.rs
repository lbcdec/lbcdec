use std::fs;
use nom;

macro_rules! decompile_test {
    ($name:ident ($file:expr): Ok) => {
        #[test]
        fn $name() {
            let script = fs::read_to_string($file).unwrap();
            let compiled = ::compile_lua(&script).unwrap();
            let bytecode = ::lua_bytecode(&compiled);
            if let nom::IResult::Done(_, ::LuaBytecode { main_chunk: ref chunk, .. }) = bytecode {
                ::decompile_chunk(&chunk, &vec![]);
            }
        }
    };
    ($name:ident ($file:expr): Err) => {};
}

macro_rules! decompile_tests {
    ($($name:ident ($file:expr): $status:tt,)*) => {
        $(decompile_test!($name ($file): $status);)*
    };
}

decompile_tests! {
    // Return
    test_return ("cases/return.lua"): Ok,
    test_result_none ("cases/return_none.lua"): Ok,
    test_return_multi ("cases/return_multi.lua"): Ok,

    // Call
    test_call_no_args ("cases/call_no_args.lua"): Ok,
    test_call_with_args ("cases/call_with_args.lua"): Ok,
    test_call_multiret ("cases/call_multiret.lua"): Ok,

    // If Statements
    test_bail_over_if ("cases/bail_over_if.lua"): Ok,
    test_if_cond_use_imm_local ("cases/if_cond_use_imm_local.lua"): Ok,
    test_if_empty_else_body ("cases/if_empty_else_body.lua"): Ok,

    // While Loops
    test_while_break ("cases/while_break.lua"): Ok,
    test_while_if_statement ("cases/while_if_statement.lua"): Ok,
    test_while_if_else_statement ("cases/while_if_else_statement.lua"): Ok,
    test_while_if_break_else ("cases/while_if_break_else.lua"): Ok,
    test_while_if_else_break ("cases/while_if_else_break.lua"): Ok,
    test_while_complex_break ("cases/while_complex_break.lua"): Ok,

    // Assignments
    test_assign_arb_center_expr ("cases/assign_arb_center_expr.lua"): Ok,
    test_complex_move_assign ("cases/complex_move_assign.lua"): Ok,
    test_call_multiret_assign ("cases/call_multiret_assign.lua"): Ok,

    // Tables
    test_table_setlist ("cases/table_setlist.lua"): Ok,
    test_table_settable ("cases/table_settable.lua"): Ok,
    test_table_settable_complex ("cases/table_settable_complex.lua"): Ok,
    test_table_setlist_with_hash ("cases/table_setlist_with_hash.lua"): Ok,
    test_pinned_newtable ("cases/pinned_newtable.lua"): Ok,

    // Conditional Expressions
    test_conditional_expr ("cases/conditional_expr.lua"): Ok,
    test_conditional_expr_no_loadbool ("cases/conditional_expr_no_loadbool.lua"): Ok,
    test_conditional_expr_into_existing ("cases/conditional_expr_into_existing.lua"): Ok,
    test_conditional_expr_loadbool_into_existing ("cases/conditional_expr_loadbool_into_existing.lua"): Ok,

    // Varargs
    test_return_varargs ("cases/return_varargs.lua"): Ok,
    test_call_varargs ("cases/call_varargs.lua"): Ok,

    // Generic For
    test_generic_for ("cases/generic_for.lua"): Ok,
    test_generic_for_break ("cases/generic_for_break.lua"): Ok,
    test_generic_for_multiparam ("cases/generic_for_multiparam.lua"): Ok,
    test_generic_for_partial_multiparam ("cases/generic_for_partial_multiparam.lua"): Ok,

    // Numeric For
    test_numeric_for ("cases/numeric_for.lua"): Ok,
    test_numeric_for_break ("cases/numeric_for_break.lua"): Ok,

    // Prototypes / Closures
    test_proto ("cases/proto.lua"): Ok,
    test_proto_capture ("cases/proto_capture.lua"): Ok,

    // TestSet
    test_testset_into_existing_local ("cases/testset_into_existing_local.lua"): Ok,
    test_testset_into_existing_local_with_complex_assign ("cases/testset_into_existing_local_with_complex_assign.lua"): Ok,

    // Implicit Nils
    test_implicit_nils ("cases/implicit_nil.lua"): Ok,

    // Close
    test_close_explicit ("cases/close_explicit.lua"): Err, // Unimplemented Instruction
    test_close_implicit ("cases/close_implicit.lua"): Err, // Generates Invalid Code

    // Regressions
    test_table_regression_test ("cases/table_regression_test.lua"): Ok,
    regression_get_debug_string ("cases/regression_get_debug_string.lua"): Ok,
    regression_proto_capture_breaks_world ("cases/regression_proto_capture_breaks_world.lua"): Ok,
    regression_return_call_semantics ("cases/regression_return_call_semantics.lua"): Ok,

    // Edge Cases
    test_self_kst_overflow ("cases/self_kst_overflow.lua"): Ok,
}
