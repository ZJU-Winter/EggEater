mod infra;

// Your tests go here!
success_tests! {
    // Number and Boolean Literals
    {
        name: num,
        file: "num.snek",
        expected: "644",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },

    // Input Expression
    {
        name: input_default,
        file: "input0.snek",
        expected: "false",
    },
    {
        name: input_bool,
        file: "input0.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input_num,
        file: "input0.snek",
        input: "123",
        expected: "123",
    },

    // Simple Number Expressions
    {
        name: add1,
        file: "add1.snek",
        expected: "73",
    },
    {
        name: add1_sub1,
        file: "add1_sub1.snek",
        expected: "4",
    },
    {
        name: add_num,
        file: "add.snek",
        input: "10",
        expected: "15",
    },

    // Nested Arithmetic Expressions
    {
        name: nested_arith0,
        file: "nested_arith0.snek",
        expected: "35",
    },
    {
        name: nested_arith1,
        file: "nested_arith1.snek",
        expected: "25",
    },
    {
        name: nested_arith2,
        file: "nested_arith2.snek",
        expected: "0",
    },
    {
        name: nested_arith3,
        file: "nested_arith3.snek",
        input: "8",
        expected: "1117",
    },
    {
        name: nested_arith4,
        file: "nested_arith4.snek",
        expected: "-1",
    },

    // Dynamic Type Checks with isnum/isbool
    {
        name: type_check_succ0,
        file: "isnum.snek",
        expected: "false",
    },
    {
        name: type_check_succ1,
        file: "isnum.snek",
        input: "547",
        expected: "true",
    },
    {
        name: type_check_succ2,
        file: "isnum.snek",
        input: "true",
        expected: "false",
    },
    {
        name: type_check_succ3,
        file: "isbool.snek",
        expected: "true",
    },
    {
        name: type_check_succ4,
        file: "isbool.snek",
        input: "689",
        expected: "false",
    },
    {
        name: type_check_succ5,
        file: "type_check_succ5.snek",
        expected: "true",
    },

    // Comparison Expressions
    {
        name: compare_expr_succ0,
        file: "compare_expr_succ0.snek",
        expected: "true",
    },

    {
        name: compare_expr_succ2,
        file: "compare_expr_succ2.snek",
        expected: "true",
    },

    // Let expressions
    {
        name: binding0,
        file: "binding0.snek",
        expected: "5",
    },
    {
        name: binding1,
        file: "binding1.snek",
        expected: "-5",
    },

    {
        name: binding_expr,
        file: "binding_expr.snek",
        expected: "1225",
    },
    {
        name: binding_nested,
        file: "binding_nested.snek",
        expected: "1",
    },

    {
        name: binding_chain,
        file: "binding_chain.snek",
        expected: "3",
    },
    {
        name: binding_nested_chain,
        file: "binding_nested_chain.snek",
        expected: "12",
    },

    // Let expressions with shadowing
    {
        name: shadowed_binding_succ0,
        file: "shadowed_binding_succ0.snek",
        expected: "100",
    },
    {
        name: shadowed_binding_succ1,
        file: "shadowed_binding_succ1.snek",
        expected: "7",
    },
    {
        name: shadowed_binding_succ2,
        file: "shadowed_binding_succ2.snek",
        expected: "150",
    },
    {
        name: shadowed_binding_succ3,
        file: "shadowed_binding_succ3.snek",
        expected: "5",
    },
    {
        name: shadowed_binding_succ4,
        file: "shadowed_binding_succ4.snek",
        expected: "18",
    },
    {
        name: shadowed_binding_succ5,
        file: "shadowed_binding_succ5.snek",
        expected: "5",
    },
    {
        name: shadowed_binding_succ6,
        file: "shadowed_binding_succ6.snek",
        expected: "3",
    },
    {
        name: shadowed_binding_succ7,
        file: "shadowed_binding_succ7.snek",
        expected: "200",
    },

    // Misc complex expressions with arithmetic and let bindings
    {
        name: complex_expr,
        file: "complex_expr.snek",
        expected: "6",
    },
    {
        name: quick_brown_fox,
        file: "quick_brown_fox.snek",
        expected: "-3776",
    },

    // If expressions
    {
        name: if_expr_succ0,
        file: "if_expr_succ0.snek",
        expected: "10",
    },
    {
        name: if_expr_succ1,
        file: "if_expr_input.snek",
        input: "635",
        expected: "20",
    },
    {
        name: if_expr_succ2,
        file: "if_expr_succ2.snek",
        expected: "8",
    },
    {
        name: if_expr_succ3,
        file: "if_expr_succ3.snek",
        expected: "7",
    },

    // Set expr
    {
        name: set_expr_succ0,
        file: "set_expr1.snek",
        expected: "true",
    },
    {
        name: set_expr_succ1,
        file: "set_expr2.snek",
        expected: "25",
    },
    {
        name: set_expr_succ2,
        file: "set_expr3.snek",
        input: "25",
        expected: "true",
    },
    {
        name: set_expr_succ3,
        file: "set_expr3.snek",
        input: "20",
        expected: "false",
    },

    {
        name: loop_expr_succ0,
        file: "loop_expr0.snek",
        input: "3",
        expected: "6",
    },
    {
        name: loop_expr_succ1,
        file: "loop_expr0.snek",
        input: "7",
        expected: "5040",
    },
    {
        name: loop_expr_succ2,
        file: "loop_expr1.snek",
        expected: "-6",
    },
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: simple_example1,
        file: "simple_test1.snek",
        expected: "(tuple 1 2 3)",
    },
    {
        name: simple_example2,
        file: "simple_test2.snek",
        expected: "3",
    },
    {
        name: out_of_bound_succ1,
        file: "out_bound.snek",
        input: "2",
        expected: "3",
    },
    {
        name: point1,
        file: "point1.snek",
        expected: "(tuple 3 6)",
    },
    {
        name: point2,
        file: "point2.snek",
        expected: "(tuple 0 0)",
    },
    {
        name: bst1,
        file: "bst1.snek",
        expected: "(tuple 3 (tuple 1 nil (tuple 2 nil nil)) (tuple 4 nil (tuple 5 nil nil)))\ntrue\nfalse",
    },
    {
        name: bst2,
        file: "bst2.snek",
        expected: "(tuple -1 nil (tuple 0 nil (tuple 1 nil (tuple 2 nil (tuple 3 nil nil)))))\ntrue\nfalse",
    },
    {
        name: bst3,
        file: "bst3.snek",
        expected: "(tuple 5 (tuple 4 (tuple 3 (tuple 2 (tuple 1 nil nil) nil) nil) nil) nil)\ntrue\nfalse",
    },
    {
        name: set_tuple,
        file: "set_tuple.snek",
        expected: "(tuple (tuple <cyclic>) 2 3)\n(tuple (tuple <cyclic>) 2 3)",
    },
    {
        name: noncyclic_equals,
        file: "equals_non_cyclic.snek",
        expected: "true\ntrue\nfalse\ntrue\ntrue\nfalse\nfalse\ntrue\ntrue",
    },
    {
        name: cyclic_print3,
        file: "print_cyclic3.snek",
        expected: "(tuple 1 (tuple 2 (tuple 3 (tuple 4 (tuple <cyclic>)))))\n(tuple 1 (tuple 2 (tuple 3 (tuple 4 (tuple <cyclic>)))))"
    },
}

runtime_error_tests! {
    // integer overflow
    {
        name: number_overflow_fail0,
        file: "number_overflow_fail0.snek",
        expected: "overflow",
    },
    {
        name: number_overflow_fail1,
        file: "number_overflow_fail1.snek",
        expected: "overflow",
    },
    {
        name: number_overflow_fail2,
        file: "add.snek",
        input: "4611686018427387899",
        expected: "overflow",
    },
    {
        name: number_overflow_fail3,
        file: "nested_arith3.snek",
        input: "4611686018427387890",
        expected: "overflow",
    },

    // type mismatch
    {
        name: invalid_argument_fail0,
        file: "invalid_argument_fail0.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail1,
        file: "invalid_argument_fail1.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail2,
        file: "invalid_argument_fail2.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail3,
        file: "invalid_argument_fail3.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail4,
        file: "invalid_argument_fail4.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail5,
        file: "invalid_argument_fail5.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail6,
        file: "invalid_argument_fail6.snek",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail7,
        file: "nested_arith3.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail8,
        file: "if_expr_input.snek",
        input: "665",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail9,
        file: "set_expr3.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail10,
        file: "loop_expr0.snek",
        input: "5",
        expected: "invalid argument",
    },
    {
        name: invalid_argument_fail11,
        file: "invalid_argument_fail11.snek",
        expected: "invalid argument",
    },
    {
        name: type_error1,
        file: "type_error1.snek",
        expected: "invalid",
    },
    {
        name: type_error2,
        file: "type_error2.snek",
        expected: "invalid",
    },
    {
        name: type_error3,
        file: "type_error3.snek",
        expected: "invalid",
    },
    {
        name: type_error4,
        file: "type_error4.snek",
        expected: "invalid",
    },
    {
        name: out_of_bound_fail1,
        file: "out_bound.snek",
        input: "-1",
        expected: "out of bound",
    },
    {
        name: out_of_bound_fail2,
        file: "out_bound.snek",
        input: "3",
        expected: "out of bound",
    },
    {
        name: invalid_address_fail1,
        file: "invalid_address.snek",
        expected: "invalid",
    }
}

static_error_tests! {

    // Invalid S-expressions
    {
        name: parse_sexp_fail1,
        file: "parse_sexp_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_sexp_fail2,
        file: "parse_sexp_fail2.snek",
        expected: "Invalid",
    },

    // Invalid tokens/operators
    {
        name: parse_token_fail1,
        file: "parse_token_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_token_fail2,
        file: "parse_token_fail2.snek",
        expected: "Invalid",
    },
    {
        name: parse_token_fail3,
        file: "parse_token_fail3.snek",
        expected: "Invalid",
    },
    {
        name: parse_token_fail4,
        file: "parse_token_fail4.snek",
        expected: "Invalid",
    },


    // Invalid/Out of bounds Number Literal
    {
        name: number_bounds_fail0,
        file: "number_bounds_fail0.snek",
        expected: "Invalid",
    },
    {
        name: number_bounds_fail1,
        file: "number_bounds_fail1.snek",
        expected: "Invalid",
    },

    // Invalid operator arguments
    {
        name: parse_op_fail1,
        file: "parse_op_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail2,
        file: "parse_op_fail2.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail3,
        file: "parse_op_fail3.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fai4,
        file: "parse_op_fail4.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail5,
        file: "parse_op_fail5.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail6,
        file: "parse_op_fail6.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail7,
        file: "parse_op_fail7.snek",
        expected: "Invalid",
    },
    {
        name: parse_op_fail8,
        file: "parse_op_fail8.snek",
        expected: "Invalid",
    },

    // Invalid let expressions
    {
        name: parse_let_nobindings_fail,
        file: "parse_let_nobindings_fail.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail1,
        file: "parse_let_improperargs_fail1.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail2,
        file: "parse_let_improperargs_fail2.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail3,
        file: "parse_let_improperargs_fail3.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail4,
        file: "parse_let_improperargs_fail4.snek",
        expected: "Invalid",
    },
    {
        name: parse_let_improperargs_fail5,
        file: "parse_let_improperargs_fail5.snek",
        expected: "keyword",
    },

    {
        name: duplicate_binding_fail0,
        file: "duplicate_binding_fail0.snek",
        expected: "Duplicate binding",
    },
    {
        name: duplicate_binding_fail1,
        file: "duplicate_binding_fail1.snek",
        expected: "Duplicate binding",
    },
    {
        name: duplicate_binding_fail2,
        file: "duplicate_binding_fail2.snek",
        expected: "Duplicate binding",
    },

    // Invalid if expressions
    {
        name: parse_if_fail0,
        file: "parse_if_fail0.snek",
        expected: "Invalid",
    },
    {
        name: parse_if_fail1,
        file: "parse_if_fail1.snek",
        expected: "Invalid",
    },

    // Unbound identifier
    {
        name: unbound_identifier_fail0,
        file: "unbound_identifier_fail0.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_identifier_fail1,
        file: "unbound_identifier_fail1.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: unbound_identifier_fail2,
        file: "unbound_identifier_fail2.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: unbound_identifier_fail3,
        file: "unbound_identifier_fail3.snek",
        expected: "Unbound variable identifier z",
    },
    {
        name: unbound_identifier_fail4,
        file: "unbound_identifier_fail4.snek",
        expected: "Unbound variable identifier t",
    },
    {
        name: unbound_identifier_fail5,
        file: "unbound_identifier_fail5.snek",
        expected: "Unbound variable identifier x",
    },

    // Invalid block
    {
        name: parse_block_fail0,
        file: "parse_block_fail0.snek",
        expected: "Invalid",
    },

    // Invalid break
    {
        name: invalid_break_fail0,
        file: "invalid_break_fail0.snek",
        expected: "break",
    },

    // Invalid loop
    {
        name: invalid_loop_fail0,
        file: "invalid_loop_fail0.snek",
        expected: "Invalid",
    },

    // Function tests
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: duplicate_func,
        file: "duplicate_func.snek",
        expected: "Invalid",
    },
    {
        name: input_in_definition,
        file: "input_in_def.snek",
        expected: "Invalid",
    },
    {
        name: keyword_argument,
        file: "keyword_argument.snek",
        expected: "Invalid",
    },
    {
        name: nonexist_func,
        file: "nonexist_func.snek",
        expected: "Invalid",
    },
    {
        name: wrong_arg_number,
        file: "wrong_arg_number.snek",
        expected: "Invalid",
    },
    {
        name: bad_func,
        file: "bad_func.snek",
        expected: "Invalid",
    }
}
