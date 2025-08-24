// Do assignment 1 and 3 in exam set June 2018. You get 1 point for each assignment.
(*
We define a heap as a binary tree whose nodes fulfil the heap property. The heap property means that
the value stored in a node must be less than or equal to the values stored in the child nodes. 

The heap property is fulfilled in example 1 below, but not in example 2 where 5 
is greater than 2 and 3. In this assignment you will implement a heap based 
on a binary tree structure. Example 3 below shows the tree from Example 1 with 
empty nodes E added. The empty nodes are grey.
We represent the heap with the below polymorphic datatype where empty nodes are represented by
EmptyHP.
*)

type Heap<'a when 'a: equality> =
| EmptyHP
| HP of 'a * Heap<'a> * Heap<'a>


(* Question 1.1
- Declare a value ex3 representing the binary tree shown in example 3 above. You may use the
template:

let ex3 = HP(1,HP(2,HP(...
HP(4,...  -> DONE 

- Write the type of the value ex3. Explain why the type is either monomorphic or polymorphic. -> DONE 

- Declare a value empty representing an empty heap, i.e. a binary tree with only one empty root -> DONE 
node. The type of the empty value is empty : Heap<’a> when ’a : equality.

- Declare an F# exception named HeapError that can be used to signal an error condition from a -> DONE 
function on heaps. The exception should carry a string to be used to describe the error.
*)

let ex3 = 
    HP(1,
    HP(2,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),
    HP(4,EmptyHP,EmptyHP))

let ex4 = 
    HP(10,
    HP(4,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),
    HP(1,EmptyHP,EmptyHP))

let ex5 = 
    HP(10,
    HP(4,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)),
    HP(2,EmptyHP,EmptyHP))

// The type is monomorphic (int) because we fed the Heap integers as Node values

let empty = EmptyHP

exception HeapError of string

let testException n = if n = 0 then raise (HeapError "broken heap") else "cheese"

// ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------

(*
Question 1.2
• Declare a function
isEmpty : Heap<’a> -> bool when ’a : equality
that returns true if a heap is the empty heap. For instance isEmpty empty returns true. The
value empty is defined above. -> DONE 

• The size h of a heap h is the number of non–empty nodes in the binary tree. Declare a function
size : Heap<’a> -> int when ’a : equality
that returns the size of a heap. For instance, size ex3 returns 5. -> DONE 

• Declare a function find h of type
find : Heap<’a> -> ’a when ’a : equality
that returns the minimum value in a non–empty heap, i.e. the root value. For instance find ex3 -> DONE 
returns 1.
*)

let isEmpty hp =
    match hp with
    |EmptyHP -> true
    |_ -> false

// test isEmpty empty;;
// #time;;
let sizeC hp =
    let id a= a
    let rec sizeGo hp acc cont = 
        match hp with 
        |EmptyHP -> cont acc
        |HP(a,b,c) -> sizeGo c (sizeGo b (cont (acc+ 1)) cont) cont 
    sizeGo hp 0  id

let sizeA hp =
    let rec sizeGo hp acc = 
        match hp with 
        |EmptyHP ->  acc
        |HP(a,b,c) -> sizeGo c (sizeGo b (acc+ 1)) 
    sizeGo hp 0

// sizeC empty;; -> PASSED
// sizeA empty;;  -> PASSED
// sizeC ex3;;  -> PASSED
// sizeA ex3;;  -> PASSED

let find hp =
    let rec goFind hp min = 
        let mutable mMin = min
        match hp with 
        |EmptyHP -> min
        |HP(a,b,c) -> 
            if min = -9999999 then goFind c (goFind b a)
            elif a < min then goFind c (goFind b a)
            else min
    goFind hp -9999999

// find ex3;; -> PASSED
// find ex4;; -> PASSED
// find ex5;; -> PASSED

(*
• Declare a function chkHeapProperty h of type
chkHeapProperty : Heap<’a> -> bool when ’a : comparison
that returns true if the heap h fulfils the heap property and otherwise false. The empty heap by
definition fulfils the heap property. For instance chkHeapProperty ex3 returns true.
*)

let chkHeapProperty hp =
    match hp with 
    |EmptyHP -> true
    |HP(a,b,c) -> 
        if find b < a || find c < a then false 
        else true 

// chkHeapProperty empty;; -> should be true -> PASSED
// chkHeapProperty ex3;; -> should be true -> PASSED
// chkHeapProperty ex4;; -> should be false -> PASSED
// chkHeapProperty ex4;; -> should be false -> PASSED

// ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------

(*
Question 1.3
• Declare a function map f h of type
map : (’a -> ’b) -> Heap<’a> -> Heap<’b>
when ’a : equality and ’b : equality
where map f h returns the heap where the function f has been applied on all values in the heap h.
You decide, but must explain, what order the function f is applied to the values in the heap. For
instance map ((+)1) ex3 returns the heap with all values in ex3 increased by one.
*)




