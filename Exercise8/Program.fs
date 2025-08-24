// Exercise 8.1 HR exercise 9.8.
(*
Develop a version of the counting function for binary trees
countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. Observe that this function is not tail recursive.
*)



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
// example below:
(*
bigListK 300000 id
    bigListK 299999 (fun res -> 1 :: k res)
        bigListK 299998 (fun res -> 1 :: k res)
*)

let rec bigListKFixed n k =
    if n = 0 then k []
    else bigListKFixed (n-1) (fun acc -> k (1 :: acc))
    
// bigListKFixed 300000 id;; -> no problem

// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.4 HR exercise 9.11.
// The functions count and countC are found on page 214 in HR.
(*
Declare tail-recursive functions leftTree and rightTree. By use of leftTree it should
be possible to generate a big unbalanced tree to the left containing n+ 1 values in the nodes so
that n is the value in the root, n− 1 is the value in the root of the left subtree, and so on. All
subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
big unbalanced tree to the right.
1. Use these functions to show the stack limit when using count and countA from Exer-
cise 9.8.
2. Use these functions to test the performance of countC and countAC from Exercise 9.9.
*)

// let rec g z = if p z then g(f z) else h z;;
let rec leftTree n cont : 'a BinTree = 
    match n with
    | 0 -> cont Leaf
    |n -> leftTree (n-1) (fun res -> cont (Node(n,res,Leaf)))

let rec rightTree n cont : 'a BinTree = 
    match n with
    | 0 -> cont Leaf
    |n -> rightTree (n-1) (fun res -> cont (Node(n,Leaf,res)))

//#time;;
// leftTree 1000000 id;;

// rightTree 1000000 id;;

// countAC (leftTree 20000 id) 0 id;; -> crashes

// countA 0 (rightTree 20000 id);; -> crashes
        
// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.5 HR exercise 11.1.
// Make a declaration for the sequence of odd numbers.

let oddNumbers n = 
    if n%2 = 0 then raise (System.Exception("n cannot be an even number"))
    else seq { 1 .. + 2 .. n}

// testing: oddNumbers 1101;; -> PASSED

// testing: s3 1;; -> PASSED
// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// Exercise 8.6 HR exercise 11.2.
// Make a declaration for the sequence of numbers 1,1,2,6,...,n!,....

// yield: emits one item | yield!: emits all items from another sequence
let rec go n = 
    seq {
        yield! seq {(Seq.foldBack (fun x acc -> x * acc) (seq {1 .. + 1 .. n}) 1)}  
        if n > 1 then yield! go (n-1)
        elif n=0 then yield 1
        elif n=1 then yield 1
        elif n < 0 then raise (System.ArgumentException "n cannot be a negative number")
    }

let rec factSeq n =   
    Seq.rev (go n)

let factorial n = Seq.item n (factSeq n)

// test: factorial 4;; -> PASSED
// test: factorial -2;; -> PASSED
