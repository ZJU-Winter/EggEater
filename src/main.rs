use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::vec;

use sexp::Atom::*;
use sexp::*;

struct Program {
    defs: Vec<Definition>,
    main: Expr,
}

#[derive(Debug)]
enum Definition {
    Func(String, Vec<String>, Expr)
}
use Definition::*;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    Label(String),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RDX,
    RSP,
    RDI,
    RSI,
    R12,
    R13,
    R15,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),
    CMovg(Val, Val),
    CMovl(Val, Val),
    CMove(Val, Val),
    CMovnz(Val, Val),
    CMovne(Val, Val),
    CMovo(Val, Val),
    JNE(Val),
    JE(Val),
    JMP(Val),
    JNZ(Val),
    JO(Val),
    JL(Val),
    Label(Val),
    Call(Val),
    Ret(),
    LeftShift(Val, Val),
    RightShift(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    ReferenceEqual, 
    Greater, 
    GreaterEqual, 
    Less, 
    LessEqual,
    Index,
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Print(Box<Expr>),
    Tuple(Vec<Expr>),
    SetTuple(Box<Expr>, Box<Expr>, Box<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if *n < -4611686018427387904 || *n > 4611686018427387903 {
                panic!("Invalid number, out of range")
            }
            Expr::Number(i64::try_from(*n).unwrap())
        }
        Sexp::Atom(S(bool)) if bool.to_lowercase() == "false" => Expr::Boolean(false),
        Sexp::Atom(S(bool)) if bool.to_lowercase() == "true" => Expr::Boolean(true),
        Sexp::Atom(S(id)) => Expr::Id(id.to_string()),
        Sexp::List(vec) => {
        match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isnum" => {
                Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "isbool" => {
                Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "index" => {
                Expr::BinOp(Op2::Index, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "equal" => {
                Expr::BinOp(Op2::ReferenceEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), condition_expr, then_expr, else_expr] if op == "if" => {
                Expr::If(Box::new(parse_expr(condition_expr)), Box::new(parse_expr(then_expr)), Box::new(parse_expr(else_expr)))
            }
            [Sexp::Atom(S(op)), expr1, expr2, expr3] if op == "set-tuple" => {
                Expr::SetTuple(Box::new(parse_expr(expr1)), Box::new(parse_expr(expr2)), Box::new(parse_expr(expr3)))
            }
            [Sexp::Atom(S(op)), loop_expr] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(loop_expr)))
            }
            [Sexp::Atom(S(op)), break_expr] if op == "break" => {
                Expr::Break(Box::new(parse_expr(break_expr)))
            }
            [Sexp::Atom(S(op)), expr] if op == "print" => {
                Expr::Print(Box::new(parse_expr(expr)))
            }
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), expr] if op == "set!" => {
                Expr::Set(name.to_string(), Box::new(parse_expr(expr)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "tuple" => {
                Expr::Tuple(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "let" => {
                match e1 {
                    Sexp::List(vec) => {
                        Expr::Let(convert_bindings(vec), Box::new(parse_expr(e2)))
                    }
                    _ => panic!("Invalid expression")
                }
            }
            [Sexp::Atom(S(func_name)), args @ ..] => {
                Expr::Call(func_name.to_string(), args.into_iter().map(parse_expr).collect())
            }
            _ => panic!("Invalid expression")
            }
        }
        _ => panic!("Invalid expression")
    }
}

fn compile_to_instrs(e: &Expr, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    match e {
        Expr::Number(n) => {
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1)));
            vec
        }
        Expr::Boolean(n) if *n == true => {
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7)));
            vec
        }
        Expr::Boolean(n) if *n == false => {
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            vec
        }
        Expr::Id(id) => {
            if id == "input" && is_definition {
                panic!("Invalid definition, can't use input in definition")
            }
            if id == "input" {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
                return vec
            }
            if id == "nil" {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                return vec
            }
            if !env.contains_key(id) {
                panic!("Unbound variable identifier {id}")
            }
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(id).unwrap() * 8)));
            vec
        }
        Expr::UnOp(op, subexpr) => {
            compile_op1(op, subexpr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::BinOp(op, subexpr1, subexpr2) => {
            compile_op2(op, subexpr1, subexpr2, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Let(bindings, body) => {
            complie_let(bindings, body, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::If(condition_expr, then_expr, else_expr) => {
            compile_if(condition_expr, then_expr, else_expr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Loop(condition_expr) => {
            compile_loop(condition_expr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Break(condition_expr) => {
            compile_break(condition_expr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Set(name, expr) => {
            compile_set(name, expr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Block(exprs) => {
            compile_block(exprs, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Call(func_name, args) => {
            compile_call(func_name, args, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Print(expr) => {
            compile_print(expr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::Tuple(exprs) => {
            compile_tuple(exprs, si, env, current_break, label_count, all_definitions, is_definition)
        }
        Expr::SetTuple(tuple_expr, index_expr, val_expr) => {
            compile_set_tuple(tuple_expr, index_expr, val_expr, si, env, current_break, label_count, all_definitions, is_definition)
        }
        _ => panic!("Invalid expression")
    }
}

fn compile_op1(op: &Op1, e: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    match op {
        Op1::Add1 => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count, all_definitions, is_definition));
            check_is_number(&mut vec);
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
            check_overflow(&mut vec);
        }
        Op1::Sub1 => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count, all_definitions, is_definition));
            check_is_number(&mut vec);
            vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
            check_overflow(&mut vec);
        }
        Op1::IsBool => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count, all_definitions, is_definition));
            vec.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(3)));
            vec.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(3)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // set to false
            vec.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
        }
        Op1::IsNum => {
            append_instr(&mut vec, compile_to_instrs(e, si, env, current_break, label_count, all_definitions, is_definition));
            vec.push(Instr::And(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::Xor(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::LeftShift(Val::Reg(Reg::RAX), Val::Imm(2)));
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(3)));
        }
    }
    vec
}

fn compile_op2(op: & Op2, e1: &Box<Expr>, e2: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let e1_instr = compile_to_instrs(e1, si, env, current_break, label_count, all_definitions, is_definition);
    let e2_instr = compile_to_instrs(e2, si + 1, env, current_break, label_count, all_definitions, is_definition);
    let mut vec: Vec<Instr> = vec![];
    
    match op {
        Op2::Plus => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
            check_overflow(&mut vec);
        }
        Op2::Minus => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::ISub(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            check_overflow(&mut vec);
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
        }
        Op2::Times => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::RightShift(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
            check_overflow(&mut vec);
        }
        Op2::Greater => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // set to false
            vec.push(Instr::CMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if greater, set to true
        }
        Op2::GreaterEqual => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7))); // set to true
            vec.push(Instr::CMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if less, set to false
        }
        Op2::Less => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // set to false
            vec.push(Instr::CMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if less, set to true
        }
        Op2::LessEqual => {
            append_instr(&mut vec, e1_instr);
            check_is_number(&mut vec);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7))); // set to true
            vec.push(Instr::CMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if greater, set to false
        }
        Op2::Equal => {
            append_instr(&mut vec, e1_instr);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_same_type(&mut vec, Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX), label_count);
            vec.push(Instr::Cmp(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))); // set to false
            vec.push(Instr::CMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX))); // if equal, set to true
        }
        Op2::ReferenceEqual => {
            append_instr(&mut vec, e1_instr);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);

            let index = if si % 2 == 1 { si + 2 } else { si + 1 }; // 16-byte aligment
            vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(index as i64* 8)));
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, 0), Val::Reg(Reg::RDI)));
            vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, (si + index) * 8))); // arg
            vec.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Reg(Reg::RAX))); // arg
            vec.push(Instr::Call(Val::Label("snek_equal".to_string()))); // call
            vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, 0)));
            vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(index as i64* 8)));
        }
        Op2::Index => {
            append_instr(&mut vec, e1_instr);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));
            append_instr(&mut vec, e2_instr);
            check_is_number(&mut vec);
            check_valid_heap(&mut vec, Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX));
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)));
            vec.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Imm(7))); // start address
            vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(4))); // offset
            vec.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBX, 0)));
        }
    }
    vec
}

fn complie_let(bindings: &Vec<(String, Expr)>, body: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let mut set: HashSet<String> = HashSet::new();
    let mut current_env = env.clone();
    let length = bindings.len() as i32;
    for binding in bindings {
        if set.contains(&binding.0) {
            panic!("Duplicate binding")
        }
        if check_is_keyword(&binding.0) {
            panic!("Invalid variable name, can't use the {} keyword", binding.0)
        }
        set.insert(binding.0.clone());
    }
    for (i, binding) in bindings.iter().enumerate() {
        let mut nenv = current_env.clone();
        append_instr(&mut vec, compile_to_instrs(&binding.1, si + i as i32, &mut nenv, current_break, label_count, all_definitions, is_definition));
        current_env.insert(binding.0.clone(), si + i as i32);
        vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + i as i32) * 8), Val::Reg(Reg::RAX)));
    }
    append_instr(&mut vec, compile_to_instrs(body, si + length, &mut current_env, current_break, label_count, all_definitions, is_definition));
    vec
}

fn compile_if(condition_expr: &Box<Expr>, then_expr: &Box<Expr>, else_expr:&Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let else_label = new_label(label_count, "ifelse");
    let end_label = new_label(label_count, "ifend");
    let cond_instrs = compile_to_instrs(condition_expr, si, env, current_break, label_count, all_definitions, is_definition);
    let then_instrs = compile_to_instrs(then_expr, si, env, current_break, label_count, all_definitions, is_definition);
    let else_instrs = compile_to_instrs(else_expr, si, env, current_break, label_count, all_definitions, is_definition);
    append_instr(&mut vec, cond_instrs);
    vec.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(3)));
    vec.push(Instr::JE(Val::Label(else_label.clone())));
    append_instr(&mut vec, then_instrs);
    vec.push(Instr::JMP(Val::Label(end_label.clone())));
    vec.push(Instr::Label(Val::Label(else_label)));
    append_instr(&mut vec, else_instrs);
    vec.push(Instr::Label(Val::Label(end_label)));
    vec
}

#[allow(unused_variables)]
fn compile_loop(expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let loop_label = new_label(label_count, "loop");
    let end_label = new_label(label_count, "loopend");
    let instrs = compile_to_instrs(expr, si, env, &end_label, label_count, all_definitions,is_definition);
    vec.push(Instr::Label(Val::Label(loop_label.clone())));
    append_instr(&mut vec, instrs);
    vec.push(Instr::JMP(Val::Label(loop_label)));
    vec.push(Instr::Label(Val::Label(end_label)));
    vec
}

fn compile_break(expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    if current_break == "" {
        panic!("Invalid break")
    }
    let mut vec: Vec<Instr> = vec![];
    append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count, all_definitions, is_definition));
    vec.push(Instr::JMP(Val::Label(current_break.to_string())));
    vec
}

fn compile_set(name: &String, expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    if !env.contains_key(name) {
        panic!("Unbound variable identifier {}", name)
    }
    let mut vec: Vec<Instr> = vec![];
    append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count, all_definitions, is_definition));
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(name).unwrap() * 8), Val::Reg(Reg::RAX)));
    vec
}

fn compile_block(exprs: &Vec<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    if exprs.is_empty() {
        panic!("Invalid block, no subexpressions")
    }
    for expr in exprs {
        append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count, all_definitions, is_definition))
    }
    vec
}

fn compile_call(func_name: &String, args: &Vec<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    if !check_valid_func(func_name, args, all_definitions) {
        panic!("Invalid function call, wrong argument number or undefined function {func_name}")
    }
    let mut vec: Vec<Instr> = vec![];
    // store temp arguments
    for (i, arg) in args.iter().enumerate() {
        append_instr(&mut vec, compile_to_instrs(arg, si + i as i32, env, current_break, label_count, all_definitions, is_definition));
        vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + i as i32) * 8), Val::Reg(Reg::RAX)))
    }
    let mut index = args.len() as i32 + 1;
    if index % 2 == 0 {
        index += 1
    }
    vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(index as i64 * 8)));
    // construct stack
    for (i, _) in args.iter().enumerate() {
        vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, (si + i as i32 + index) * 8)));
        vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, i as i32 * 8), Val::Reg(Reg::RBX)));
    }
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, args.len() as i32 * 8), Val::Reg(Reg::RDI))); // rdi
    vec.push(Instr::Call(Val::Label(func_name.to_string()))); // call
    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, args.len() as i32 * 8))); // restore rdi
    vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(index as i64 * 8))); // restore rsp
    vec
}

fn compile_print(expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    let index = if si % 2 == 1 { si + 2 } else { si + 1 }; // 16-byte aligment
    append_instr(&mut vec, compile_to_instrs(expr, si, env, current_break, label_count, all_definitions, is_definition));
    vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(index as i64* 8)));
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, 0), Val::Reg(Reg::RDI)));
    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX))); // arg
    vec.push(Instr::Call(Val::Label("snek_print".to_string()))); // call
    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, 0)));
    vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(index as i64* 8)));
    vec
}

fn compile_tuple(exprs: &Vec<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    if exprs.is_empty() {
        vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
        return vec
    }
    // store values on stack
    for (i, expr) in exprs.iter().enumerate() {
        append_instr(&mut vec, compile_to_instrs(expr, si + i as i32, env, current_break, label_count, all_definitions, is_definition));
        vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + i as i32) * 8), Val::Reg(Reg::RAX)));
    }
    // store the tuple length
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(2 * exprs.len() as i64)));
    vec.push(Instr::IMov(Val::RegOffset(Reg::R15, 0), Val::Reg(Reg::RBX)));
    for (i, _) in exprs.iter().enumerate() {
        vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, (si + i as i32) * 8)));
        vec.push(Instr::IMov(Val::RegOffset(Reg::R15, (1 + i as i32) * 8), Val::Reg(Reg::RBX)));
    }
    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R15)));
    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec.push(Instr::IAdd(Val::Reg(Reg::R15), Val::Imm((1 + exprs.len() as i64) * 8)));
    vec
}

fn compile_set_tuple(tuple_expr: &Box<Expr>, index_expr: &Box<Expr>, val_expr: &Box<Expr>, si: i32, env: &mut HashMap<String, i32>, current_break: &String, label_count: &mut i32, all_definitions: &Vec<Definition>, is_definition: bool) -> Vec<Instr> {
    let tuple_instr = compile_to_instrs(tuple_expr, si, env, current_break, label_count, all_definitions, is_definition);
    let index_instr = compile_to_instrs(index_expr, si + 1, env, current_break, label_count, all_definitions, is_definition);
    let val_instr = compile_to_instrs(val_expr, si + 2, env, current_break, label_count, all_definitions, is_definition);
    let mut vec = vec![];

    append_instr(&mut vec, tuple_instr);
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)));

    append_instr(&mut vec, index_instr);
    check_is_number(&mut vec);
    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + 1) * 8), Val::Reg(Reg::RAX)));

    check_valid_heap(&mut vec, Val::RegOffset(Reg::RSP, si * 8), Val::RegOffset(Reg::RSP, (si + 1) * 8));
    append_instr(&mut vec, val_instr);

    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)));
    vec.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Imm(7))); // start address
    vec.push(Instr::IMov(Val::Reg(Reg::R12), Val::RegOffset(Reg::RSP, (si + 1) * 8)));
    vec.push(Instr::IMul(Val::Reg(Reg::R12), Val::Imm(4))); // offset

    vec.push(Instr::IAdd(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::IMov(Val::RegOffset(Reg::RBX, 0), Val::Reg(Reg::RAX)));
    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)));
    vec
}

fn convert_bindings(vec: &Vec<Sexp>) -> Vec<(String, Expr)> {
    if vec.is_empty() {
        panic!("Invalid, no binding")
    }
    let mut rst:Vec<(String, Expr)> = vec![];
    for exp in vec {
        match exp {
            Sexp::List(v) => {
                match &v[..] {
                    [Sexp::Atom(S(s)), subexpr] => {
                        rst.push((s.to_string(), parse_expr(subexpr)))
                    }
                    _ => panic!("Invalid expression")
                }
            }
            _ => panic!("Invalid expression")
        }
    }
    rst   
}

fn new_label(label_count: &mut i32, label: &str) -> String {
    let current = *label_count;
    *label_count += 1;
    format!("{label}_{current}")
}

fn check_is_number(vec : &mut Vec<Instr>) {
    vec.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
    vec.push(Instr::CMovnz(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JNZ(Val::Label("throw_error".to_string())));
}

#[allow(dead_code)]
fn check_is_number_debug(vec : &mut Vec<Instr>) {
    vec.push(Instr::Test(Val::Reg(Reg::RAX), Val::Imm(1)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(1)));
    vec.push(Instr::CMovnz(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JNZ(Val::Label("throw_error_debug".to_string())));
}

fn check_same_type(vec: &mut Vec<Instr>, val1: Val, val2: Val, label_count: &mut i32) {
    let label_1 = new_label(label_count, "check_same_type");
    let label_2 = new_label(label_count, "check_same_type");
    vec.push(Instr::IMov(Val::Reg(Reg::R12), val1));
    vec.push(Instr::IMov(Val::Reg(Reg::R13), val2));

    vec.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::R12)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R13)));
    vec.push(Instr::Xor(Val::Reg(Reg::RDX), Val::Reg(Reg::RBX)));
    vec.push(Instr::And(Val::Reg(Reg::RDX), Val::Imm(1)));
    vec.push(Instr::Test(Val::Reg(Reg::RDX), Val::Reg(Reg::RDX)));
    vec.push(Instr::JE(Val::Label(label_1.clone())));
    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2)));
    vec.push(Instr::Call(Val::Label("throw_error".to_string())));
    vec.push(Instr::Label(Val::Label(label_1.clone())));

    vec.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::R12)));
    vec.push(Instr::And(Val::Reg(Reg::RDX), Val::Imm(1)));
    vec.push(Instr::Test(Val::Reg(Reg::RDX), Val::Reg(Reg::RDX)));
    vec.push(Instr::JE(Val::Label(label_2.clone())));
    vec.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::R12)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R13)));
    vec.push(Instr::Xor(Val::Reg(Reg::RDX), Val::Reg(Reg::RBX)));
    vec.push(Instr::And(Val::Reg(Reg::RDX), Val::Imm(3)));
    vec.push(Instr::JE(Val::Label(label_2.clone())));
    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(2)));
    vec.push(Instr::Call(Val::Label("throw_error".to_string())));
    vec.push(Instr::Label(Val::Label(label_2.clone())));
}

fn check_overflow(vec: &mut Vec<Instr>) {
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(3)));
    vec.push(Instr::CMovo(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JO(Val::Label("throw_error".to_string())));
}

fn check_is_keyword(name: &String) -> bool {
    if name.eq("let") || name.eq("add1") || name.eq("sub1")
    ||
    name.eq("isnum") || name.eq("isbool") || name.eq("print")
    ||
    name.eq("break") || name.eq("set!") || name.eq("loop")
    ||
    name.eq("if") || name.eq("block") || name.eq("input") || name.eq("tuple")
    ||
    name.eq("+") || name.eq("-") || name.eq("*") 
    ||
    name.eq("<") || name.eq(">") || name.eq(">=") || name.eq("<=") || name.eq("=") {
        return true
    }
    return false
}

fn check_valid_func(func_name: &String, args: &Vec<Expr>, all_definitions: &Vec<Definition>) -> bool {
    for definition in all_definitions {
        match definition {
            Func(def_name, parameters, _) => {
                if def_name == func_name && parameters.len() == args.len() {
                    return true
                }
            }
        }
    }
    false
}

fn check_valid_heap(vec : &mut Vec<Instr>, heap_data: Val, number: Val) {
    vec.push(Instr::IMov(Val::Reg(Reg::R12), heap_data));

    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1))); // nil
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(4))); // error code
    vec.push(Instr::CMove(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JE(Val::Label("throw_error".to_string())));

    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(3))); // false
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(4))); // error code
    vec.push(Instr::CMove(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JE(Val::Label("throw_error".to_string())));

    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(7))); // nil
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(4))); // error code
    vec.push(Instr::CMove(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JE(Val::Label("throw_error".to_string())));

    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::And(Val::Reg(Reg::RBX), Val::Imm(1)));
    vec.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(1)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(4))); // error code
    vec.push(Instr::CMovne(Val::Reg(Reg::RDI), Val::Reg(Reg::RBX)));
    vec.push(Instr::JNE(Val::Label("throw_error".to_string())));

    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RBX, -1))); // get length
    vec.push(Instr::IMov(Val::Reg(Reg::R12), number));
    // less than 0
    vec.push(Instr::Cmp(Val::Reg(Reg::R12), Val::Imm(0)));
    vec.push(Instr::IMov(Val::Reg(Reg::R13), Val::Imm(5))); // error code
    vec.push(Instr::CMovl(Val::Reg(Reg::RDI), Val::Reg(Reg::R13)));
    vec.push(Instr::JL(Val::Label("throw_error".to_string())));
    // greater than length
    vec.push(Instr::IAdd(Val::Reg(Reg::R12), Val::Imm(2)));
    vec.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Reg(Reg::R12)));
    vec.push(Instr::CMovl(Val::Reg(Reg::RDI), Val::Reg(Reg::R13)));
    vec.push(Instr::JL(Val::Label("throw_error".to_string())));
}

fn append_instr(vec: &mut Vec<Instr>, other: Vec<Instr>) {
    for instr in other {
        vec.push(instr)
    }
}

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(val1, val2) => {
            format!("mov {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovg(val1, val2) => {
            format!("cmovg {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovl(val1, val2) => {
            format!("cmovl {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMove(val1, val2) => {
            format!("cmove {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovne(val1, val2) => {
            format!("cmovne {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovnz(val1, val2) => {
            format!("cmovnz {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::CMovo(val1, val2) => {
            format!("cmovo {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::IAdd(val1, val2) => {
            format!("add {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::ISub(val1, val2) => {
            format!("sub {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::IMul(val1, val2) => {
            format!("imul {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::And(val1, val2) => {
            format!("and {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::Xor(val1, val2) => {
            format!("xor {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::Cmp(val1, val2) => {
            format!("cmp {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::Test(val1, val2) => {
            format!("test {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::JNE(val) => {
            format!("jne {}", val_to_str(val))
        }
        Instr::JE(val) => {
            format!("je {}", val_to_str(val))
        }
        Instr::JNZ(val) => {
            format!("jnz {}", val_to_str(val))
        }
        Instr::JMP(val) => {
            format!("jmp {}", val_to_str(val))
        }
        Instr::JO(val) => {
            format!("jo {}", val_to_str(val))
        }
        Instr::JL(val) => {
            format!("jl {}", val_to_str(val))
        }
        Instr::Label(label) => {
            format!("{}:", val_to_str(label))
        }
        Instr::Call(label) => {
            format!("call {}", val_to_str(label))
        }
        Instr::Ret() => {
            format!("ret")
        }
        Instr::LeftShift(val1, val2) => {
            format!("sal {}, {}", val_to_str(val1), val_to_str(val2))
        }
        Instr::RightShift(val1, val2) => {
            format!("sar {}, {}", val_to_str(val1), val_to_str(val2))
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => "rax".to_string(),
                Reg::RDX => "rdx".to_string(),
                Reg::RSP => "rsp".to_string(),
                Reg::RDI => "rdi".to_string(),
                Reg::RSI => "rsi".to_string(),
                Reg::RBX => "rbx".to_string(),
                Reg::R12 => "r12".to_string(),
                Reg::R13 => "r13".to_string(),
                Reg::R15 => "r15".to_string(),
            }
        }
        Val::Imm(val) => {
            format!("{val}")
        }
        Val::RegOffset(reg, offset) => {
            match reg {
                Reg::RSP if *offset == 0 => {
                    format!("[rsp]")
                }
                Reg::RSP => {
                    format!("[rsp + {}]", offset)
                }
                Reg::RAX if *offset == 0 => {
                    format!("[rax]")
                }
                Reg::RAX if *offset < 0 => {
                    format!("[rax - {}]", -offset)
                }
                Reg::RAX => {
                    format!("[rax + {}]", offset)
                }
                Reg::R15 if *offset == 0 => {
                    format!("[r15]")
                }
                Reg::R15 if *offset < 0 => {
                    format!("[r15 - {}]", -offset)
                }
                Reg::R15 => {
                    format!("[r15 + {}]", offset)
                }
                Reg::RBX if *offset == 0 => {
                    format!("[rbx]")
                }
                Reg::RBX if *offset < 0 => {
                    format!("[rbx - {}]", -offset)
                }
                Reg::RBX => {
                    format!("[rbx + {}]", offset)
                }
                _ => panic!("Invalid register")
            }
        }
        Val::Label(label) => {
            label.to_string()
        }
    }
}

fn get_binding_depth(bindings: &Vec<(String, Expr)>) -> i32 {
    let mut rst = 0;
    for (i, binging) in bindings.iter().enumerate() {
        rst = rst.max(i as i32 + depth(&binging.1))
    }
    rst
}

fn get_func_depth(args: &Vec<Expr>) -> i32 {
    let mut rst = 0;
    for (i, arg) in args.iter().enumerate() {
        rst = rst.max(i as i32 + depth(arg))
    }
    rst.max(args.len() as i32) // rdi
}

fn get_tuple_depth(elements: &Vec<Expr>) -> i32 {
    let mut rst = 0;
    for (i, element) in elements.iter().enumerate() {
        rst = rst.max(i as i32 + depth(element))
    }
    rst + 1
}

fn depth(expr: &Expr) -> i32 {
    match expr {
        Expr::Number(_) => 0,
        Expr::Boolean(_) => 0,
        Expr::Id(_) => 0,
        Expr::UnOp(_, expr) => depth(expr),
        Expr::BinOp(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::Let(bindings, expr) => get_binding_depth(bindings).max(depth(expr) + bindings.len() as i32),
        Expr::If(expr1, expr2, expr3) => depth(expr1).max(depth(expr2)).max(depth(expr3)),
        Expr::SetTuple(expr1, expr2, expr3) => depth(expr1).max(1 + depth(expr2)).max(2 + depth(expr3)),
        Expr::Loop(expr) => depth(expr),
        Expr::Block(exprs) => exprs.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Break(expr) => depth(expr),
        Expr::Set(_, expr) => depth(expr),
        Expr::Call(_, args) => get_func_depth(args),
        Expr::Print(expr) => depth(expr),
        Expr::Tuple(exprs) => get_tuple_depth(exprs),
    }
}

fn is_definition(s: &Sexp) -> bool {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
            _ => false
        }
        _ => false,
    }
}

fn parse_arguments(args: &[Sexp]) -> Vec<String> {
    let mut vec: Vec<String> = vec![];
    let mut set: HashSet<String> = HashSet::new();
    for arg in args {
        match arg {
            Sexp::Atom(S(val)) => {
                if check_is_keyword(val) {
                    panic!("Invalid definition, can't use {val} keyword as parameter")
                }
                if set.contains(val) {
                    panic!("Invalid definition, duplicate parameter name")
                }
                set.insert(val.to_string());
                vec.push(val.to_string());
            }
            _ => panic!("Invalid argument, should be an atom")
        }
    }
    vec
}

fn parse_definition(s: &Sexp) -> Definition {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => match &name_vec[..] {
                [Sexp::Atom(S(func_name)), args @..] => {
                    if check_is_keyword(func_name) {
                        panic!("Invalid definition, can't use keyword {func_name} as function name")
                    }
                    Func(func_name.to_string(), parse_arguments(args), parse_expr(body))
                }
                _ => panic!("Invalid definition, not function name + args")
            },
            _ => panic!("Invalid definition, no fun keyword"),
        },
        _ => panic!("Invalid definition, wrong syntax, should be a list"),
    }
}

fn parse_program(s: &Sexp) -> Program {
    let mut set: HashSet<String> = HashSet::new();
    match s {
        Sexp::List(vec) => {
            let mut defs: Vec<Definition> = vec![];
            for (i, def_or_exp) in vec.iter().enumerate() {
                if is_definition(def_or_exp) {
                    let definition = parse_definition(def_or_exp);
                    match &definition {
                        Func(def_name, _, _) => {
                            if set.contains(def_name) {
                                panic!("Invalid definition, duplicate function name {def_name}")
                            }
                            set.insert(def_name.to_string());
                        }
                    }
                    defs.push(definition);
                } else {
                    if i != vec.len() - 1 {
                        panic!("Invalid program")
                    }
                    return Program {
                        defs,
                        main: parse_expr(def_or_exp),
                    };
                }
            }
            panic!("Invalid, only found definitions");
        }
        _ => panic!("Invalid, program should be a list")
    }
}

fn compile_definition(definition: &Definition, labels: &mut i32, all_definitions: &Vec<Definition>) -> Vec<Instr> {
    let mut vec: Vec<Instr> = vec![];
    match definition {
        Func(name, args, body) => {
            let depth = depth(body);
            let mut body_env = HashMap::new();
            for (i, arg) in args.iter().enumerate() {
                body_env.insert(arg.to_string(), depth + 1 + i as i32);
            }
            let body_is = compile_to_instrs(body, 0, &mut body_env, &"".to_string(), labels, all_definitions, true);
            vec.push(Instr::Label(Val::Label(name.to_string())));
            vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(depth as i64 * 8)));
            append_instr(&mut vec, body_is);
            vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(depth as i64 * 8)));
            vec.push(Instr::Ret());
        }
    }
    vec
}

fn compile_program(p: &Program) -> (String, String) {
    let mut labels: i32 = 0;
    let mut defs_vec: Vec<Instr> = vec![];
    let mut main_vec: Vec<Instr> = vec![];
    let mut defs_rst: String = String::new();
    let mut main_rst: String = String::new();

    for def in &p.defs[..] {
        append_instr(&mut defs_vec, compile_definition(&def, &mut labels, &p.defs));
    }
    let mut depth = depth(&p.main);
    // depth += 1;
    if depth % 2 == 1 {
        depth += 1;
    }
    let main = compile_to_instrs(&p.main, 0, &mut HashMap::new(), &"".to_string(), &mut labels, &p.defs, false);
    main_vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(depth as i64 * 8)));
    append_instr(&mut main_vec, main);
    main_vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(depth as i64 * 8)));

    for (i, instr) in defs_vec.iter().enumerate() {
        if i != 0 {
            defs_rst.push_str("\n");
            defs_rst.push_str("\t");
        }
        defs_rst.push_str(&instr_to_str(instr));
    }

    for (i, instr) in main_vec.iter().enumerate() {
        if i != 0 {
            main_rst.push_str("\n");
            main_rst.push_str("\t");
        }
        main_rst.push_str(&instr_to_str(instr));
    }
    (defs_rst, main_rst)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    env::set_var("RUST_BACKTRACE", "1");

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;
    
    let prog = "(".to_owned() + &in_contents + ")";
    let prog = parse_program(&parse(&prog).expect("Invalid expression"));
    let (definitions, main) = compile_program(&prog);

    let asm_program = format!("
    section .text
    extern snek_error
    extern snek_print
    extern snek_equal
    throw_error:
    push rsp
    call snek_error
    ret
    {}
    global our_code_starts_here
    our_code_starts_here:
    mov r15, rsi
    {}
    ret
    ", definitions, main);

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
