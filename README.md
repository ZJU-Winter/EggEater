## EggEater
This is a course project repo for CSE231(Compiler Construction) at UCSD.

The compiler is designed to compile a programming language called `snake` and implemented in Rust.

The `snake` language supports basic unary/binary operations, vairable bindings, `if`,`loop`,`block`, `break` expressions, structural and reference comparsion, function calls, heap data allocations and garbage collection.

### Syntax
> snytax for the snake language, see more examples in the tests directory
```
<prog> := <defn>* <expr>
<defn> := (fun (<name> <name>*) <expr>)
<expr> :=
  | <number>
  | true
  | false
  | input
  | <identifier>
  | (let (<binding>+) <expr>)
  | (<op1> <expr>)
  | (<op2> <expr> <expr>)
  | (set! <name> <expr>)
  | (if <expr> <expr> <expr>)
  | (block <expr>+)
  | (loop <expr>)
  | (break <expr>)
  | (<name> <expr>*)
  | (tuple <expr>+)
  | (set-tuple <expr> <expr> <expr>)

<op1> := add1 | sub1 | isnum | isbool | print
<op2> := + | - | * | < | > | >= | <= | = | index | equal

<binding> := (<identifier> <expr>)
```
