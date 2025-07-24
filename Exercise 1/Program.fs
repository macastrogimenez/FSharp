// For more information see https://aka.ms/fsharp-console-apps

printfn "Hello World from F#"

// Exercise 1
let sqr x = x * x

let testSqr = sqr 9

// Exercise 2
let pow x n = System.Math.Pow (x, n)

let testPow = pow 2 5

// Declare a function g: int -> int, where g(n) = n+ 4.
let g n = n + 4

let testG = g 4 

// Declare a function h: float * float -> float, where h(x,y) = x2 + y2. Hint: Use
// the function System.Math.Sqrt.

let h (x:float) (y:float)  = System.Math.Sqrt (x*x + y*y)

let testH = h 3.0 4.0 

// Declare a recursive function f: int -> int, where f(n) = 1 + 2 +···+ (n−1) + n
// for n ≥ 0. (Hint: use two clauses with 0 and n as patterns.)
// State the recursion formula corresponding to the declaration.
// Give an evaluation for f(4).
let rec f n =
    match n with
    | 0 -> 0
    | n -> n + f (n-1)
    
let testF = f 4
    
// The sequence F0,F1,F2,... of Fibonacci numbers is defined by:
// F0 = 0
// F1 = 1
// Fn = Fn−1 + Fn−2
// Thus, the first members of the sequence are 0,1,1,2,3,5,8,13,....
// Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns
// correspond to the three cases of the above definition.
// Give an evaluations for F4.

let rec fb n = 
    match n with 
    | 0 -> 0
    | 1 -> 1
    | n -> fb (n-1) + fb (n-2)

// Declare a recursive function sum: int * int -> int, where
// sum(m,n) = m+ (m+ 1) + (m+ 2) +···+ (m+ (n−1)) + (m+ n)
// for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
// Give the recursion formula corresponding to the declaration.

let rec sum (m,n) = 
    match (m, n) with
    |(m,0) -> m
    |(m,n) -> m + n + sum (m, n-1)

// Determine a type for each of the expressions:
// (System.Math.PI, fact -1) -> float * int
// fact(fact 4) int 
// power(System.Math.PI, fact 2) float
// (power, fact)  float * int

// Consider the declarations:
// let a = 5;;
// let f a = a + 1;;
// let g b = (f b) + a;;
// Find the environment obtained from these declarations and write the evaluations of the expres-
// sions f 3 and g 3.

(*
env1 =
    a -> 5
    f -> function a + 1
    g -> function (f b) + a

Evaluations:
    f 3 = 3 + 1
    f 3 = 4

    g 3 = f 3 + a
    g 3 = 3 + 1 + a
    g 3 = 4 + a
    g 3 = 4 + 5
    g 3 = 9
*)

// Exercise 1.10 Write a function dup:string->string that concatenates a string with itself.
// You can either use + or ˆ. For example:
// val dup : string -> string
// > dup "Hi ";;
// val it : string = "Hi Hi "

let dup (s:string) = s + s

// Exercise 1.11 Write a function dupn:string->int->string so that dupn s n creates the concatenation
// of n copies of s. For example:
// val dupn : string -> int -> string
// > dupn "Hi " 3;;
// val it : string = "Hi Hi Hi "

let rec dupn (s:string) (n:int) : string = 
    match n with 
    | 0 -> ""
    | n -> s + dupn s (n-1)

