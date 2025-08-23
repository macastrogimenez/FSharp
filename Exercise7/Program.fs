// Exercise 7.1 HR exercise 9.1
(*
Consider the function g declared on Page 202 and the stack and heap after the evaluation of g 2
shown in Figure 9.2. Reproduce this resulting stack and heap by a systematic application of push
and pop operations on the stack, and heap allocations that follow the step by step evaluation of
g 2.
*)
let xs = [1;2]

let rec g = function
| 0 -> xs
| n -> 
    let ys = n::g(n-1)
    List.rev ys

g 2

// stack push xs -> with pointer to -> HEAP: 1 -> 2
// stack push g -> with pointer to -> HEAP: g closure
// stack push g 2 -> with pointer to -> HEAP: 2 :: g 1
// stack push g 1 -> with pointer to -> HEAP: 1 :: g 0 
// stack pop g 0 -> with pointer to -> stack xs - on the HEAP: 1 -> 2
// stack pop g 1 -> with pointer to -> HEAP: 2 -> 1 -> 1 (because it was reversed)
// stack pop g 2 -> with pointer to -> HEAP: 1 -> 1 -> 2 -> 2 (because it was reversed)

//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------
// Iterations as loops -> factW with a while loop instead
let factW n =
    let mutable mutableN = n
    let mutable acc = 1
    while mutableN>0 do
        acc <- acc * mutableN 
        mutableN <- mutableN-1
    acc;;

//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------

// Exercise 7.2 HR exercise 9.3.
(*
Declare an iterative solution to exercise 1.6. (BELOW)

Declare a recursive function sum: int * int -> int, where
sum(m, n) = m + (m + 1) + (m + 2) + ··· + (m + (n − 1)) + (m + n)
for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
Give the recursion formula corresponding to the declaration.
*)

// #time;;
let rec sum (m,n) = 
    match (m,n) with
    |(m,0)-> m
    |(m,n)-> m + n + sum (m,n-1)

// test: sum (4,1);; -> should be 9 -> PASSED
// test: sum (5,1);; -> should be 11  -> PASSED
// sum (5,200000);;
// test: sum (5,2000000);;
// test: sum (5,900000);;  -> crashes
// test: sum (5,2000000);; -> crashes, memory exceeded
    
let sumIt (m,n)=
    if not (n = 0) then 
        List.fold (fun acc n -> acc + m+n) m [1.. +1 .. n]
    elif n < 0 then
        raise (System.ArgumentException("n cannot be a negative number"))
    else m 

// test: sumIt (4,1);; -> should be 9  -> PASSED
// test: sumIt (5,1);; -> should be 11  -> PASSED
// test: sumIt (5,200000);;
// test: sumIt (5,2000000);; -> integer overflow but worked-> PASSED
// sumIt (5,200000000);;


let rec sumIt2 (m,n,acc) = 
    if not (n = 0) then 
        sumIt2 (m, n-1, acc+m+n)
    elif n < 0 then
        raise (System.ArgumentException("n cannot be a negative number"))
    else acc + m 
// test: sumIt2 (5,200000,0);;
// test: sumIt2 (5,2000000,0);; -> integer overflow but worked-> PASSED
// test: sumIt2 (5,200000000,0);; -> integer overflow but worked -> PASSED
// test: sumIt2 (5,1,0);; -> should be 11 -> PASSED


//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------
// Exercise 7.3 HR exercise 9.4.
// One iterative declaration is enough.
(*Give iterative declarations of the list function List.length*)

// #time;;

let rec listLength xs = 
    let acc n f= n + f  
    match xs with
    |[]-> 0
    |x::xs -> acc 1 (listLength xs)

// test: listLength [1.. +1 .. 10000000];; -> failed, not tail recursive

let rec listLength2 (accumulator, xs) = 
    if not (xs = []) then listLength2(accumulator+1, xs.Tail) else accumulator

// test: listLength2 (0,[1.. +1 .. 10000000]);; -> failed, not tail recursive

let listLength3 xs = 
    if not (xs = []) 
        then List.fold (fun acc x -> acc + 1) 0 xs
    else 0

// test: listLength3 [1.. +1 .. 10000000];; -> passed, tail recursive


//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------
// Exercise 7.4 HR exercise 9.6.
(* Declare a continuation-based version of the factorial function and compare the run time with
the results in Section 9.4.

The use of factA gives a clear improvement to the use of fact. Consider the following
example measuring the time of 1000000 computations of 16! using these two function:
let xs16 = List.init 1000000 (fun i -> 16);;
val xs16 : int list = [16; 16; 16; 16; 16; 16; 16; 16; ...]
#time;;
for i in xs16 do let _ = fact i in ();;
Real: 00:00:00.051, CPU: 00:00:00.046,
GC gen0: 0, gen1: 0, gen2: 0
val it : unit = ()
for i in xs16 do let _ = factA(i,1) in ();;
Real: 00:00:00.024, CPU: 00:00:00.031,
GC gen0: 0, gen1: 0, gen2: 0
val it : unit = ()
208 Efficiency
The performance gain of using factA is actually much better than the factor 2 indicated by
the above examples becomes the run time of the for construct alone is about 12 ms:
for i in xs16 do let _ = () in ();;
Real: 00:00:00.012, CPU: 00:00:00.015,
GC gen0: 0, gen1: 0,
*)

let rec fact n = 
    match n with
    | 0 -> 1
    | n-> n*fact(n-1)

// #time;;
// fact 35;;

let rec fact2 a acc = 
    if a > 0 then fact2 (a-1) (a*acc) else acc*1

// fact2 35 1;;

let rec fact3 a c = 
    match a with
    | 0 -> c 1
    | n-> fact3 (n-1) (fun acc -> c (n*acc))

let id a = a

fact3 5 id;;

//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------
// Exercise 7.5 HR exercise 8.6.
// This to be used in the next task.
(*
Declare a function for computing Fibonacci numbers Fn (see Exercise 1.5) using a while
loop. Hint: introduce variables to contain the two previously computed Fibonacci numbers.

1.5 The sequence F0, F1, F2,... of Fibonacci numbers is defined by:
F0 = 0
F1 = 1
Fn = Fn−1 + Fn−2
Thus, the first members of the sequence are 0, 1, 1, 2, 3, 5, 8, 13,....
Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns
correspond to the three cases of the above definition.
Give an evaluations for F4.
*)

let rec fibW a = 
    let mutable counter = 0
    let mutable currentFibVal = 0
    let mutable fibMinusOne = 1
    let mutable fibMinusTwo= 0
    while counter < a do
        fibMinusTwo <- fibMinusOne 
        fibMinusOne <- currentFibVal
        if counter = 0 then 
            currentFibVal <- 0
            counter <- counter + 1
            //printfn "when counter was %d,the current fib was %A" counter currentFibVal
        elif counter = 1 then 
            currentFibVal <- 1
            counter <- counter + 1
            //printfn "when counter was %d,the current fib was %A" counter currentFibVal
        elif counter > 1 then 
            currentFibVal <- fibMinusOne + fibMinusTwo
            counter <- counter + 1
            //printfn "when counter was %d,the current fib was %A" counter currentFibVal
    currentFibVal
// #time;;     
// fibW 1;;
// fibW 34;;


//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------
// Exercise 7.6 HR exercise 9.7.
(*
Develop the following three versions of functions computing Fibonacci numbers Fn (see Exercise 1.5):
1. A version fibA: int -> int -> int -> int with two accumulating parameters n1 and
n2, where fibA n n1 n2 = Fn, when n1 = Fn−1 and n2 = Fn−2. Hint: consider suitable
definitions of F−1 and F−2.
2. A continuation-based version fibC: int -> (int -> int) -> int that is based on the
definition of Fn given in Exercise 1.5.
Compare these two functions using the directive #time, and compare this with the while-loop
based solution of Exercise 8.6
*)

let rec fibA2 n n1 n2 =
    if n = 0 then n2
    elif n = 1 then n1
    else fibA2 (n - 1) (n1 + n2) n1
// fibA2 6 1 0;; -> PASSED

let rec fibC n C =
    if n = 0 then C 0
    elif n = 1 then C 1
    else fibC (n-1) ( fun res1 ->
            (fibC (n-2) (fun res2 ->
                C (res1 + res2))))
