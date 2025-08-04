// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------

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

// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------
(*Exercise 5.2 Write a function
mapInOrder : (’a -> ’b) -> ’a BinTree -> ’b BinTree
that makes an in-order traversal of the binary tree and apply the function on all nodes in the tree.
With the value intBinTree
let intBinTree =
Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
Node(562, Leaf, Node(78, Leaf, Leaf)))

we can do the following example:
> mapInOrder (fun x -> x+1) intBinTree;;
val it : int BinTree =
Node
(44,Node (26,Node (57,Leaf,Leaf),Leaf),
Node (563,Leaf,Node (79,Leaf,Leaf)))
Can you give an example of why mapInOrder might give a result different from mapPostOrder, but the
result tree returned in both cases is still the same.
*)

let rec mapInOrder f a= 
    match a with
    | Leaf -> Leaf  // ✅ Return Leaf (polymorphic, will be 'b BinTree)
    | Node(j,t1,t2) ->
        // In-order processing: left subtree, root, right subtree
        let mapt1 = mapInOrder f t1
        let mapmid = f j
        let mapt2 = mapInOrder f t2
        Node(mapmid, mapt1, mapt2)

// test: mapInOrder (fun x acc-> x+acc ; acc+1) intBinTree;;

(*
    well, mapInOrder cannot have a different result from mapPostOrder since we are applying the 
    mapping operation over every single element individually - there are no accumulators from one 
    node to the next one, also since this is not a regular list as the one used in inOrder
    we cannot return a result in different order than the predetermined order for BinTree which is
    (node: int x BinTree x BinTree). Therefore even if we traverse through the binary tree in a different way
    the result will be the same and the binary tree will also be the same.
*)
    

(*
Let's define mapPostOrder to compare:
*)
let rec mapPostOrder f = function
    | Leaf -> Leaf
    | Node(j,t1,t2) ->
        let mapt1 = mapPostOrder f t1    // Process left subtree FIRST
        let mapt2 = mapPostOrder f t2    // Process right subtree SECOND  
        let mapmid = f j                 // Process root LAST
        Node(mapmid, mapt1, mapt2)

(*
Example 1: Function with side effects (logging)
*)
let loggedIncrement x =
    printfn "Processing node: %d" x
    x + 1

let testTree = Node(1, Node(2, Leaf, Leaf), Node(3, Leaf, Leaf))

// mapInOrder will print: "Processing node: 1", "Processing node: 2", "Processing node: 3"
// mapPostOrder will print: "Processing node: 2", "Processing node: 3", "Processing node: 1"
// But both return: Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf))

(*
Example 2: Stateful function
*)
let mutable counter = 0
let statefulIncrement x =
    counter <- counter + 1
    counter = counter + 1
    printfn "Step %d: processing %d" counter x
    x * 10

// Reset counter and run mapInOrder
counter <- 0
let result1 = mapInOrder statefulIncrement testTree
// Prints: Step 1: processing 1, Step 2: processing 2, Step 3: processing 3

// Reset counter and run mapPostOrder  
counter <- 0
let result2 = mapPostOrder statefulIncrement testTree
// Prints: Step 1: processing 2, Step 2: processing 3, Step 3: processing 1

// Both result1 and result2 have identical tree structure!

(*
inOrder(Node(Node(Leaf,1,Leaf), 3, Node(Node(Leaf,4,Leaf), 5, Leaf)));;
Result should be: [1; 3; 4; 5]
*)


// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------