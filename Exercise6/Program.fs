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
    | Node(leftTree,s,rightTree) -> (Fexpr leftTree + " " + Fexpr rightTree + " " + s).Trim()

let firstCalculation = Node(Node(Leaf,"x",Leaf),"+",Node(Leaf,"7.0",Leaf))
let secondCalculation = Node(Node(Node(Leaf,"x",Leaf),"+",Node(Leaf,"7.0",Leaf)),"*",Node(Node(Leaf,"x",Leaf),"-",Node(Leaf,"5.0",Leaf)))
//test: Fexpr firstCalculation;; -> PASSED
//test: Fexpr secondCalculation;; -> PASSED

// ------------------------------------------------------------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------------------------------------

    

// Exercise 6.2 - HR exercise 6.8
// Exercise 6.3 - HR exercise 7.2