use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64, memory: *mut i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    match errcode {
        1 => eprintln!("invalid argument, should be number"),
        2 => eprintln!("invalid argument, different types"),
        3 => eprintln!("overflow"),
        4 => eprintln!("invalid address"),
        5 => eprintln!("out of bound"),
        _ => eprintln!("Unknown error code {}", errcode)
    }
    std::process::exit(1);
}

fn snek_str(val : i64, seen : &mut Vec<i64>) -> String {
    if val == 7 { "true".to_string() }
    else if val == 3 { "false".to_string() }
    else if val % 2 == 0 { format!("{}", val >> 1) }
    else if val == 1 { "nil".to_string() }
    else if val & 1 == 1 {
        if seen.contains(&val)  { return "(tuple <cyclic>)".to_string() }
        seen.push(val);
        let addr = (val - 1) as *const i64;
        let length = unsafe { *addr >> 1 };
        let mut rst: String = format!("(tuple").to_string();
        for i in 0..length {
            let element = unsafe { *addr.offset(1 + i as isize) };
            rst.push_str(" ");
            rst.push_str(&snek_str(element, seen));
        }
        rst.push_str(")");
        seen.pop();
        return rst;
    }
    else {
        format!("Unknown value: {}", val)
    }
  }

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val: i64) -> i64 {
    let mut seen = Vec::<i64>::new();
    println!("{}", snek_str(val, &mut seen));
    return val;
}

#[no_mangle]
#[export_name = "\x01snek_equal"]
fn snek_equal(val1: i64, val2: i64) -> i64 {
    // println!("val1: {:#0x}, val2: {:#0x}", val1, val2);
    if val1 == val2 {
        return 7;
    }
    if val1 & 1 == 1 && val2 & 1 == 1 {
        let mut seen = Vec::<i64>::new();
        return equal_helper(val1, val2, &mut seen);
    }
    return 3;
}

fn equal_helper(val1: i64, val2: i64, seen : &mut Vec<i64>) -> i64 {
    if seen.contains(&val1) || seen.contains(&val2) {
        println!("cyclic comparison");
        return 3;
    }
    seen.push(val1);
    seen.push(val2);
    let addr1 = (val1 - 1) as *const i64;
    let addr2 = (val2 - 1) as *const i64;
    let length1 = unsafe { *addr1 >> 1};
    let length2 = unsafe { *addr2 >> 1};
    if length1 != length2 {
        return 3;
    }
    for i in 0..length1 {
        let element1 = unsafe { *addr1.offset(1 + i as isize) };
        let element2 = unsafe { *addr2.offset(1 + i as isize) };
        // println!("element1: {:#0x}, element2: {:#0x}", element1, element2);
        if element1 != element2 {
            if element1 & 1 == 1 && element2 & 1 == 1 {
                let rst = equal_helper(element1, element2, seen);
                if rst == 3 {
                    return 3;
                }
            } else {
                return 3;
            }
        }
    }
    seen.pop();
    seen.pop();
    return 7;
}

fn parse_arg(v : &Vec<String>) -> i64 {
    if v.len() < 2 { return 3 }
    let s = &v[1];
    if s == "true" { 7 }
    else if s == "false" { 3 }
    else { s.parse::<i64>().unwrap() << 1 }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    env::set_var("RUST_BACKTRACE", "1");
    let input = parse_arg(&args);

    let mut memory = Vec::<i64>::with_capacity(1000000);
    let buffer :*mut i64 = memory.as_mut_ptr();

    let i: i64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i);
}
