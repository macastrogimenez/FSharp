
(*
Exercise 5.6 Suppose that an expression of the form inc(x) is added to the abstract syntax. It adds one to the
value of x in the current state, and the value of the expression is this new value of x. The expression inc(x) should
be added to the type aExp.
How would you refine the interpreter to cope with this construct?
Again we refer to slide 28 in the slide deck from the lecture 5
Hint: Adding inc(x) to aExp, means that evaluating an expression may also update the state. Hence the state
must be returned which has a rippling effect on the evaluation functions.
This task is only to describe how you would solve the task. There is no code to hand–in.
 *)

type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Inc of string
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtraction *)

let rec evalA exp env =
    match exp with
    | N n -> (n,env) 
    | V v -> (Map.find v env,env)
    | Inc d -> 
        let valueD = Map.find d env + 1
        (valueD,Map.add d valueD env)
    | Add(a,b) ->
        match (evalA a env, evalA b env) with
        |((a,b),(c,d)) -> (a+c,env)
    | Mul(a,b)-> 
        match (evalA a env, evalA b env) with
        |((a,b),(c,d)) -> (a-c,env)
    | Sub(a,b)-> 
        match (evalA a env, evalA b env) with
        |((a,b),(c,d)) -> (a*c,env)

// The declaration of the abstract syntax for boolean expressions is defined as follows (slide 25). 
type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *)
| Lt of aExp * aExp (* less than *)
| Neg of bExp (* negation *)
| Con of bExp * bExp (* conjunction *)

let rec evalB exp env =
    match exp with
    | TT -> true
    | FF -> false
    | Eq(a,b) -> if evalA a env = evalA b env then true else false
    | Lt(a,b)-> if evalA a env < evalA b env then true else false
    | Neg a -> if a = TT then false else true
    | Con(a,b) -> 
        match evalB a env, evalB b env with
        |(true,true)-> true
        |(_,_) -> false
        

// The conjunction of two boolean values returns true if both values are true.
// The abstract syntax for the statements are defined as below (slide 26):
type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)
| RU of bExp * stm (*  Repeat Until *)
| IT of bExp * stm (* If then *)

// Define 5 examples and evaluate them.
// For instance, consider the example stmt0 and initial state state0 below.

let stmt0 = Ass("res",(Add(N 10, N 30)))
let state0 = Map.empty

// You can then run the example as follows
// > I stmt0 state0;;
// val it : Map<string,int> = map [("res", 40)]
// and get the result state with variable res assigned the value 40 (as expected).

let rec I stm env =
    match stm with
    | Ass(x,a) -> 
        let valueA = evalA a env
        match valueA with
        |(a,b)-> Map.add x a env
    | Skip -> env 
    | Seq(stm1, stm2) ->
            I stm1 env |> I stm2
    | ITE(b,stm1,stm2) -> if (evalB b env) = true then I stm1 env else I stm2 env 
    | While(b, stm3) -> 
        match evalB b env with
        | true -> I (Seq(stm3,(While(b,stm3)))) env
        | _ ->  I Skip env 
    | RU(b, stm4) -> 
        match evalB b env with
        | false -> I (Seq(stm4,(RU(b,stm4)))) env
        | _ ->  I Skip env (* Repeat Until *)
    | IT (b,stm5) -> if (evalB b env) = true then I stm5 env else I Skip env


(*
SKIP stm1 -> if given an environment such as state0 with the SKIP keyword it will return state0 untouched -> PASSED
SEQ stm2 -> given an environment, it will perform two statements in sequential order on the env and returned the modified env
    example: 
        assign a = 10
        assing b = 12
        return map with a = 10  and b = 12
ITE -> given env perform x on env if b is true 
    if Lt(10,12) = true then do stm2
WHILE
*)

let stm1: Map<string,int> = I Skip state0
// Skip test, running the above line --> PASSED
let stm2 = Seq(Ass("a",N(10)),Ass("b",N(12)))
// SEQ, Ass, N test: I stm2 state0;; --> PASSED

let stm3 = ITE(Lt(N(10),N(12)),stm2,Skip)
let stm4 = ITE(Lt(N(13),N(12)),stm2,Skip)
// ITE, Lt, N, Skip test: I stm3 state0;;
    // should return the map with 10 and 12 --> PASSED
// ITE, Lt, N, Skip test: I stm4 state0;;
    // should return the empty map --> PASSED
let stm5 = 
    Seq(Seq(Ass("a",N(1)),Ass("b",N(20))),
        While(
            (Lt((V "a"),(V "b"))),
            (Ass("a",(Add(V "a",N 1))))
        ))

// WHILE, Ass, Add, Lt test: I stm5 state0;; --> PASSED

let stm6 = Seq(Ass("a",N 3),Ass("a",Inc "a"))
//test: I stm6 state0;; --> PASSED

let stm7 = 
    Seq(Seq(Ass("a",N(1)),Ass("b",N(20))),
        While(
            (Lt((V "a"),(V "b"))),
            (Ass("a",Inc "a"))
        ))
//Test: I stm7 state0;;