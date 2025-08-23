// Exercise 8.1 HR exercise 9.8.
(*
Develop a version of the counting function for binary trees
countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. Observe that this function is not tail recursive.
*)

// let rec g z = if p z then g(f z) else h z;;

type 'a BinTree =
    Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
    Node(562, Leaf, Node(78, Leaf, Leaf)))

let rec countA acc (bt: 'a BinTree) = 
    match bt with
    |Leaf -> acc
    |Node(a,b,c) -> countA (countA (acc + 1) b) c

// test: countA 0 intBinTree;; -> PASSED
// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.2 HR exercise 9.9.
(*
Declare a tail-recursive functions with the type
countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b
such that count t= countAC t 0 id. The intuition with countAC t a c is that a is the
number of nodes being counted so far and c is the continuation.
*)
let id a = a
let rec countAC (bt: 'a BinTree) (acc:int) cont = 
    match bt with
    |Leaf -> cont acc
    |Node(a,b,c) -> 
        countAC c 
            (countAC b 
                (acc + 1) cont) cont

//test countAC intBinTree 0 id;; --> PASSED

// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.3 HR exercise 9.10.
(*
Consider the following list-generating function:
let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res));;
The call bigListK 130000 id causes a stack overflow. Analyze this problem.
*)
// You probably have to call with a larger number before you see the stack overflow, .e.g., try with 
// bigListK 300000 id;;

let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res));;

// bigListK 130000 id;; -> no problem
// bigListK 300000 id;; -> effectively fails due to stack overflow

// it happens because we call the recursion before the continuation, therefore, the stack
// keeps growing until it overflows. If we had called the continuation first, then the stack
// would not grow as much because the continuation would be called first and then the recursion
// would be called on the result of the continuation, which would be a much smaller stack.
// Therefore, the stack would not grow as much and would not overflow.

let rec bigListKFixed n k =
    if n = 0 then k []
    else bigListKFixed (n-1) (fun acc -> k (1 :: acc))
    
// bigListKFixed 300000 id;; -> no problem

(*
bigListK 300000 id
    bigListK 299999 (fun res -> 1 :: k res)
        bigListK 299998 (fun res -> 1 :: k res)
*)




// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.4 HR exercise 9.11.
// The functions count and countC are found on page 214 in HR.

// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.5 HR exercise 11.1.

// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.6 HR exercise 11.2.