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
// Exercise 7.2 HR exercise 9.3.
(*
Declare an iterative solution to exercise 1.6. (BELOW)

Declare a recursive function sum: int * int -> int, where
sum(m, n) = m + (m + 1) + (m + 2) + ··· + (m + (n − 1)) + (m + n)
for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
Give the recursion formula corresponding to the declaration.
*)

//------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------
// Exercise 7.3 HR exercise 9.4.
// One iterative declaration is enough.
(*Give iterative declarations of the list function List.length*)

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