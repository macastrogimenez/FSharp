// Exercise 2.1 Assume the time of day is represented as a pair (hh, mm):int*int.
// Write a function timediff:int*int->int*int->int so that timediff t1 t2 computes the difference in minutes between t1 and t2, i.e., t2-t1. A few examples:
// val timediff : int * int -> int * int -> int
// > timediff (12,34) (11,35);;
// val it : int = -59
// > timediff (12,34) (13,35);;
// val it : int = 61

let timediff (a:int,b:int) (c:int,d: int) = 
    (c*60+d) - (a*60+b) // F# is so cool

// Exercise 2.2 Write a function minutes:int*int->int to compute the number of minutes since midnight.
// Easily done using the function timediff. A few examples:
// val minutes : int * int -> int
// > minutes (14,24);;
// val it : int = 864
// > minutes (23,1);;
// val it : int = 1381

let minutesSinceMidnight (a:int,b:int) = 
    timediff (0,0) (a,b)

// Exercise 2.3 Solve HR, exercise 2.2
// Declare the F# function isIthChar: string * int * char -> bool where the value of
// isIthChar(str,i,ch) is true if and only if chis the i’th character in the string str (numbering
// starting at zero).

let isIthChar (str: string) (i:int) (ch:char) = 
    if str.[i] = ch then true else false

// // test: isIthChar "migul" 0 'm';;  
// Declare an F# function pow: string * int -> string, where:
// pow(s,n) = s·s···· ·s
// n
// where we use· to denote string concatenation. (The F# representation is +.)

let rec pow (s:string ,n:int) = 
    match (s,n) with
    | (s,0) -> ""
    | (s, n) -> s + pow (s,n-1) 

// Exercise 2.4 Solve HR, exercise 2.8

let rec bin (n:int,k:int ) = 
    match (n,k) with
    | (n,0) -> 1
    | (n,k) -> 
        if k = n then 1
        else bin (n-1, k-1) + bin (n-1, k)

// Exercise 2.5 Solve HR, exercise 2.9

// 2.9 Consider the declaration:
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y)

// 1. Determine the type of f. -> int * int -> int
// 2. For which arguments does the evaluation of f terminate? for x = 0
// 3. Write the evaluation steps for f(2,3). 
    (*
    f (2,3)
    -> f(1,6)
    -> f(0,6)
    -> 6
    *)
// 4. What is the mathematical meaning of f(x,y)?
// y*!x

//Exercise 2.6 Solve HR, exercise 2.10

// Consider the following declaration:
let test(c,e) = if c then e else 0;;

// 1. What is the type of test? 

//bool * int -> int

// 2. What is the result of evaluating test(false, fact(-1))?

(*Due to the "Eager evaluation" concept of F# all arguments of the function are evaluated simultaneously
once they are given to a function, and since one of them would throw the function into 
infinite recursion, the function never manages to actually reach the if statement evaluation
*)
let rec fact = function
| 0 -> 1
| n -> n * fact(n-1);;

// 3. Compare this with the result of evaluating if false then fact -1 else 0

(*In the case of the if statement, its evaluation is "lazy", therefore,
its arguments are only evaluated once they are needed but never before
*)

//  Exercise 2.7 Solve HR, exercise 2.13

// The functions curry and uncurry of types
// curry : (’a * ’b -> ’c) -> ’a -> ’b -> ’c
// uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c
// are defined in the following way:
// curry f is the function g where g x is the function h where h y= f(x,y).
// uncurry g is the function f where f(x,y) is the value h y for the function h= g x.
// Write declarations of curry and uncurry.

let curry f = 
    let g x =
        let h y= f(x,y)
        h 
    g

let sumUncurried (x, y) = x + y 

let uncurry g =
    let f (x,y) = 
        let h = g x 
        h y 
    f 

let sumCurried x y = x + y


//hehehe 
// heheh 2s