(*
Exercise 5.1 Consider the definition of type ’a BinTree on slide 30. Write a function
inOrder : ’a BinTree -> ’a list
that makes an in–order traversal of the tree and collect the elements in a result list. In–order traversal is defined
on slide 32.
With the value intBinTree defined on slide 30
let intBinTree =
Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
Node(562, Leaf, Node(78, Leaf, Leaf)))
we get the following:
> inOrder intBinTree;;
val it : int list = [56; 25; 43; 562; 78]
*)

type 'a BinTree =
    Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
    Node(562, Leaf, Node(78, Leaf, Leaf)))

let rec inOrder = function
    | Leaf -> []
    | Node(j,t1,t2) -> inOrder t1 @ [j] @ inOrder t2
// this is basically the function in slide 6, with only one thing modified - the order of the nodes

let rec inOrderWrong a = 
    match a with
    | Leaf -> []
    | Node(j,t1,t2) -> inOrder t1 @ j :: inOrder t2
// test: inOrderWrong intBinTree;; -> This is the corrected version of what I would have done 