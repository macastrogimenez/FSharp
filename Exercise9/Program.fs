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

let rec map f hp =
    match hp with
    |EmptyHP -> EmptyHP
    |HP(A,B,C) -> 
        let mapA = f A
        let mapLeftTree = map f B
        let mapRightTree = map f C
        HP(mapA,mapLeftTree,mapRightTree)

// test: map (fun x -> x+1) ex3; -> PASSED
// test: map ((+)1) ex3 -> PASSED

// I decided to develop the function and map the heap in pre-order fashion since it seems 
// the most natural due to the structure of the tree with (root, leftTree, rightTree)

(*
The heap ex3 fulfils the heap property. Give an example of a function f such that mapping f on
all values in ex3 gives a new heap that does not fulfil the heap property. Given your definition of
f, show that chkHeapProperty (map f ex3) returns false.
*)
// map (fun x -> x-(x*2)) ex3 -> returns an inverted tree, where the biggest values have turned 
// into the smallest and vice versa

// chkHeapProperty (map (fun x -> x-(x*2)) ex3);; -> should return false -> PASSED

// ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------
(*
In this question we work with sequences as covered in Chapter 11 in HR.
Question 3.1
• Declare the infinite sequence triNum of triangular numbers. The type of triNum is seq<int>.
The triangular numbers are defined as xn =
n(n+1)
2 where xn is the nth element in the sequence.
The first element has index n = 0.
Hint: You may use Seq.initInfinite. The sequence is seq [0;1;3;6;...].

• Declare a cached version triNumC of triNum such that already computed elements are cached.
The type of triNumC is seq<int>.
 *)

let triNum = 
    Seq.initInfinite (fun n -> (n*n + n)/2) 

// Seq.item 0 triNum;; should be 0   -> PASSED
// Seq.item 1 triNum;; should be 1   -> PASSED
// Seq.item 2 triNum;; should be 3   -> PASSED
// Seq.item 3 triNum;; should be 6   -> PASSED

let triNumC = Seq.cache triNum

// Seq.item 0 triNumC;; should be 0   -> PASSED
// Seq.item 1 triNumC;; should be 1   -> PASSED
// Seq.item 2 triNumC;; should be 3   -> PASSED
// Seq.item 3 triNumC;; should be 6   -> PASSED

// ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------

(*
Question 3.2
The function filterOddIndex s filters out all elements ei of the sequence s where i is odd. The
function declaration is based on the assumption that the input sequence s is infinite but unfortunately
goes into an infinite loop. For instance filterOddIndex triNum never terminates.
let rec filterOddIndex s =
Seq.append (Seq.singleton (Seq.item 0 s))
(filterOddIndex (Seq.skip 2 s))

• Declare your own version myFilterOddIndex similar to filterOddIndex except that it
does not enter an infinite loop but returns the intended sequence.
Hint: You may be inspired by Section 11.3 in HR. The sequence for myFilterOddIndex
triNum is seq [0;3;10;21;...].
*)

let rec filterOddIndex s =
    Seq.append (Seq.singleton (Seq.item 0 s))
        (filterOddIndex (Seq.skip 2 s))

let rec myFilterOddIndex (s:seq<'a>) = s |> Seq.cache |> Seq.skipWhile (fun x -> )

myFilterOddIndex triNum;;

// ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------
(*
Question 3.3
The sequence library Seq contains a number of functions to manipulate sequences, see Table 11.1 in
HR. One such function is Seq.zip s1 s2 of type
(seq<’a> -> seq<’b> -> seq<’a * ’b>)
For instance executing Seq.zip triNum triNum returns the value
seq [(0, 0); (1, 1); (3, 3); (6, 6); ...]
• Declare a function seqZip of type
(seq<’a> -> seq<’b> -> seq<’a * ’b>)
that works the same as Seq.zip. You are not allowed to use Seq.zip but should implement
seqZip using sequence expressions as explained in Section 11.6 in HR. You may use the tem-
plate below.
let rec zipSeq s1 s2 =
seq {let e1 = Seq.item 0 s1
let e2 = Seq.item 0 s2
... }
*)
