(*For this hand–in you also need to consider scenarios where your solutions should return an error, i.e., an
exception. The requirement is, that no matter what input you pass to your function that fulfils the function type,
then the function should return the intended answer or an exception. It is up to you to define the exceptions and
whether they should carry extra information, like error messages.*)

// Exercise 6.1 - HR exercise 6.2
(*
Postfix form is a particular representation of arithmetic expressions where each operator is
preceded by its operand(s), for example:
(x + 7.0) has postfix form x 7.0 +
(x + 7.0) ∗ (x − 5.0) has postfix form x 7.0 + x 5.0 − ∗
Declare an F# function with type Fexpr -> string computing the textual, postfix form of
expression trees from Section 6.2.
*)
type Fexpr =
    Leaf
    |Node of Fexpr * string * Fexpr

let rec Fexpr exp =
    match exp with
    | Leaf -> ""
    | Node(leftTree,s,rightTree) -> 
        if (String.forall (fun x -> x = ' ' ) s) then failwith "No terms in the expression can be empty or a space"
        else (Fexpr leftTree + " " + Fexpr rightTree + " " + s).Trim()

let firstCalculation = Node(Node(Leaf,"x",Leaf),"+",Node(Leaf,"7.0",Leaf))
let secondCalculation = Node(Node(Node(Leaf,"x",Leaf),"+",Node(Leaf,"7.0",Leaf)),"*",Node(Node(Leaf,"x",Leaf),"-",Node(Leaf,"5.0",Leaf)))
let thirdTest = Node(Node(Leaf,"",Leaf),"+",Node(Leaf,"7.0",Leaf))
//test: Fexpr firstCalculation;; -> PASSED
//test: Fexpr secondCalculation;; -> PASSED
//test: Fexpr thirdTest;; 

let trans (fe, x) = 
    (Fexpr fe).Split()
    |> List.ofArray
    |> List.map (fun y -> if y = "x" then x.ToString() else y )
    
//test: trans(secondCalculation, 1.0);;

// ------------------------------------------------------------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------------------------------------

// Exercise 6.2 - HR exercise 6.8
(*
We consider a simple calculator with instructions for addition, subtraction, multiplication and
division of floats, and the functions: sin, cos, log and exp.
The instruction set of the calculator is modelled by the following F# type:
type Instruction = | ADD | SUB | MULT | DIV | SIN
| COS | LOG | EXP | PUSH of float
The calculator is a stack machine, where a stack is a list of floats.
The execution of an instruction maps a stack to a new stack:
The execution of ADD with stack abc ··· yields a new stack: (b + a) c ··· , where the top
two elements a and b on the stack have been replaced by the single element (b + a). Similarly
with regard to the instructions, SUB, MULT and DIV, which all work on the top two elements
of the stack.
The execution of one of the instructions SIN, COS, LOG and EXP applies the corresponding function to the top element of the stack. For example, the execution of LOG with stack
abc ··· yields the new stack: log(a) b c ··· .
The execution of PUSH r with the stack abc ··· pushes r on top of the stack, that is, the
new stack is: rabc ···

1. Declare a type Stack for representing the stack, and declare an F# function to interpret the
execution of a single instruction:
intpInstr: Stack -> Instruction -> Stack

2. A program for the calculator is a list of instructions [i1, i2,...,in]. A program is executed
by executing the instructions i1, i2,...,in one after the other, in that order, starting with an
empty stack. The result of the execution is the top value of the stack when all instructions
have been executed.
Declare an F# function to interpret the execution of a program:
intpProg: Instruction list -> float

3. Declare an F# function
trans: Fexpr * float -> Instruction list
where Fexpr is the type for expression trees declared in Section 6.2. The value of the expression 
trans(fe, x) is a program prg such that intpProg(prg) gives the float value of
fe when X has the value x. Hint: The instruction list can be obtained from the postfix form of
the expression. (See Exercise 6.2.)
*)


type Instruction = 
    | ADD 
    | SUB 
    | MULT 
    | DIV 
    | SIN
    | COS 
    | LOG 
    | EXP 
    | PUSH of float

type Stack = list<float>
module Stack =
    let pushOnTop a (s:Stack) = 
        let b:Stack = s |> List.append [a]
        b
    let push a (s:Stack) = 
        let b:Stack = s.Tail |> List.append [a]
        b

    let doublePush a (s:Stack) = 
        let b:Stack = s.Tail.Tail |> List.append [a]
        b
    let head (s:Stack) = s.Head

    let tailHead (s:Stack) = s.Tail.Head
let stackForTestingLog:Stack = [(System.Math.Exp 2.0) .. +2.0 .. 20.0]

let stackForBasicArithmetic:Stack = [2.0 .. +1.0 .. 9.0]
let intpInstr (s:Stack) i=
    match i with
    | ADD -> Stack.doublePush (Stack.head s + Stack.tailHead s) s
    | SUB -> Stack.doublePush (Stack.tailHead s - Stack.head s) s
    | MULT -> Stack.doublePush (Stack.head s * Stack.tailHead s) s
    | DIV -> Stack.doublePush (Stack.tailHead s / Stack.head s) s
    | SIN -> Stack.push (System.Math.Sin (Stack.head s)) s
    | COS -> Stack.push (System.Math.Cos (Stack.head s)) s
    | LOG -> Stack.push (System.Math.Log (Stack.head s)) s
    | EXP -> Stack.push (System.Math.Exp (Stack.head s)) s
    | PUSH f -> Stack.pushOnTop f s

// LOG test: intpInstr LOG stackForTestingLog;;
// ADD test: intpInstr ADD stackForBasicArithmetic;;
// SUB test: intpInstr SUB stackForBasicArithmetic;;
// MULT test: intpInstr MULT stackForBasicArithmetic;;
// DIV test: intpInstr DIV stackForBasicArithmetic;;
// SIN test: intpInstr SIN stackForBasicArithmetic;;
// COS test: intpInstr COS stackForBasicArithmetic;;
// EXP test: intpInstr EXP stackForBasicArithmetic;;
// PUSH test: intpInstr (PUSH 1.0) stackForBasicArithmetic;;

//intpProg2 : version of the calculator that requires a stack to be passed to it.
let rec intpProg il s  = 
    match il with 
    | [] -> Stack.head s
    | x::xs -> 
        intpInstr s x
        |> intpProg xs

//intpProg2 : version of the calculator that starts with an empty stack without having to pass the argument to it.
let intpProg2 il  = 
    let s:Stack = []
    let finalStack = List.fold (fun acc instruction -> intpInstr acc instruction) s il
    Stack.head finalStack     

let iL = [ADD; SUB; MULT; PUSH 1; LOG;]

let oL = [ADD; SUB; MULT; PUSH 1; LOG; ADD; ADD; ADD]
//test: intpProg iL stackForBasicArithmetic;; -> result should be 0 -> PASSED
    // intpProg oL stackForBasicArithmetic;; -> result should be 18 -> PASSED

// Exercise 6.3 - HR exercise 7.2

type Fexpr1 =
    Leaf
    |Node of Fexpr1 * string * Fexpr1

let rec Fexpr1 exp =
    match exp with
    | Leaf -> ""
    | Node(leftTree,s,rightTree) -> 
        if (String.forall (fun x -> x = ' ' ) s) then failwith "No terms in the expression can be empty or a space"
        else (Fexpr1 leftTree + " " + Fexpr1 rightTree + " " + s).Trim()

let testExpTree = Node(Node(Leaf,"x",Leaf),"+",Node(Leaf,"7.0",Leaf))
let testExpTree2 = Node(Node(Node(Leaf,"x",Leaf),"+",Node(Leaf,"7.0",Leaf)),"*",Node(Node(Leaf,"x",Leaf),"-",Node(Leaf,"5.0",Leaf)))
let testExpTree3 = Node(Node(Leaf,"",Leaf),"+",Node(Leaf,"7.0",Leaf))

let testExpTree4 = Node(Node(Leaf,"x",Leaf),"/",Node(Leaf,"7.0",Leaf))
//test: Fexpr testExpTree;; -> PASSED
//test: Fexpr testExpTree2;; -> PASSED
//test: Fexpr testExpTree3;; -> PASSED

let fromStringToExpression (s:string) x=
    let lowerS = s.ToLower() // to avoid problems with Upper, lowercase or combination of both
    match lowerS with 
    |"+"->ADD
    |"-"->SUB
    |"*"->MULT
    |"/"->DIV
    |"sin"->SIN
    |"cos"->COS
    |"log"->LOG
    |"exp"->EXP
    |"x" -> PUSH x
    |_ -> lowerS |> float |> PUSH
let translate (fe:Fexpr1, x) = 
    (Fexpr1 fe).Split()
    |> List.ofArray
    |> List.map (fun y-> fromStringToExpression y x )

let instructionsTestTree2 = translate (testExpTree2, 10);;
let ES: Stack = []
// testing my version -> PASSED
let test = intpProg instructionsTestTree2 ES;;

// testing AI improved version -> PASSED
let test2 = intpProg2 instructionsTestTree2;;

// testing division after debugging: intpProg2 (translate (testExpTree4, 35.0));; -> PASSED
