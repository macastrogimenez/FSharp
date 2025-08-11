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

// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------

(*
Exercise 5.3 Write a function
foldInOrder : (’a -> ’b -> ’b) -> ’b -> ’a BinTree -> ’b
that makes an in-order traversal of the tree and folds over the elements.
For instance, given the tree 
let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))
the application
foldInOrder (fun n a -> a + n) 0.0 floatBinTree
returns 764.0.
*)
let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
    Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

let foldInOrder fn acc bt = bt |> inOrder |> List.fold fn acc
// test: foldInOrder (fun n a -> a + n) 0.0 floatBinTree;;
    // - foldInOrder (fun n a -> a + n) 0.0 floatBinTree;;;;
    // val it: float = 764.0
let rec foldInOrder2 fn acc bt = 
    match bt with
    | Leaf -> acc
    | Node(value, left, right) ->
        let leftResult = foldInOrder2 fn acc left
        let midResult = fn value leftResult
        foldInOrder2 fn midResult right

let rec foldInOrder3 f acc bT = 
    match bT with
    |Leaf -> acc
    |Node(root,leftTree,rightTree) -> 
        let processRoot= f acc root
        let processLeft = foldInOrder3 f processRoot leftTree
        foldInOrder3 f processLeft rightTree
        
let sumAndPrint acc a= 
    let b = acc + a
    printfn "%f" a 
    b
    
let rec foldPostOrder f acc bT = 
    match bT with 
    |Leaf -> acc
    |Node(root,leftTree,rightTree) -> f (foldPostOrder f (foldPostOrder f acc leftTree) rightTree) root
        
let rec foldPreOrder f acc bT = 
    match bT with
    |Leaf -> acc
    |Node(root,leftTree,rightTree) -> foldPreOrder f (foldPreOrder f (f acc root) leftTree) rightTree


(*
Great question! Your current `foldInOrder` function uses a composition approach, but to make it have the exact type signature `('a -> 'b -> 'b) -> 'b -> 'a BinTree -> 'b`, you need to implement it as a **direct recursive function** that folds over the tree structure itself, rather than converting to a list first.

Here's how to fix it: 
```
Made changes.


```
## **Why This Works:**

### **Type Signature Breakdown:**
```fsharp
foldInOrder : ('a -> 'b -> 'b) -> 'b -> 'a BinTree -> 'b
              ↑                  ↑    ↑           ↑
              function           acc  tree        result
```

### **In-Order Traversal Pattern:**
1. **Process left subtree** with accumulator
2. **Apply function to current node** with result from left
3. **Process right subtree** with updated accumulator

### **How it executes with your example:**
```fsharp
foldInOrder (fun n a -> a + n) 0.0 floatBinTree

// Tree structure:
//       43.0
//      /    \
//   25.0     562.0
//   /           \
// 56.0         78.0

// Execution order (in-order):
// 1. foldInOrder on left subtree of 43.0
//    1a. foldInOrder on left subtree of 25.0 (56.0 node)
//        - Left: Leaf → 0.0
//        - Node: 56.0 + 0.0 = 56.0
//        - Right: Leaf → 56.0
//    1b. Node 25.0: 25.0 + 56.0 = 81.0
//    1c. Right of 25.0: Leaf → 81.0
// 2. Node 43.0: 43.0 + 81.0 = 124.0
// 3. foldInOrder on right subtree of 43.0
//    3a. Left of 562.0: Leaf → 124.0
//    3b. Node 562.0: 562.0 + 124.0 = 686.0
//    3c. Right subtree (78.0): 78.0 + 686.0 = 764.0
```

## **Comparison with Your Original Approach:**

### **Your original (composition):**
```fsharp
let foldInOrder fn acc bt = bt |> inOrder |> List.fold fn acc
```
- **Pros:** Simple, reuses existing functions
- **Cons:** Creates intermediate list, less efficient

### **New direct implementation:**
```fsharp
let rec foldInOrder fn acc bt = 
    match bt with
    | Leaf -> acc
    | Node(value, left, right) ->
        let leftResult = foldInOrder fn acc left
        let midResult = fn value leftResult
        foldInOrder fn midResult right
```
- **Pros:** More efficient (no intermediate list), direct tree traversal
- **Cons:** Slightly more complex

## **Alternative Implementations:**

### **Using continuation-passing style:**
```fsharp
let rec foldInOrder fn acc = function
    | Leaf -> acc
    | Node(v, l, r) -> 
        foldInOrder fn (fn v (foldInOrder fn acc l)) r
```

### **More explicit with let bindings:**
```fsharp
let rec foldInOrder fn acc tree =
    match tree with
    | Leaf -> acc
    | Node(value, leftTree, rightTree) ->
        let accAfterLeft = foldInOrder fn acc leftTree
        let accAfterNode = fn value accAfterLeft  
        let accAfterRight = foldInOrder fn accAfterNode rightTree
        accAfterRight
```

Your new `foldInOrder` function now has the exact type signature requested and implements true in-order folding directly on the tree structure!
*)
let rec insert i = function
    | Leaf -> Node (i, Leaf, Leaf)
    | Node (root, left, right) as bT-> 
        match compare i root with 
        | 0 -> bT
        | n when n < 0 -> Node(root, insert i left, right)
        | _ -> Node(root, left,insert i right)
            

let mutable newTree = Node(6,Leaf,Leaf);;

newTree <- insert 5 newTree;;
newTree <- insert 10 newTree;;
newTree <- insert 41 newTree;;

let rec memberOf i = function // type int -> binTree -> bool
    |Leaf -> false
    |Node (root, l, r) as bT-> 
        match compare i root with
        | 0 -> true
        | n when n < 0 -> memberOf i l
        | _ -> memberOf i r

memberOf 5 newTree;;
memberOf 41 newTree;;

// ------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------

(*
Exercise 5.4 Complete the program skeleton for the interpreter presented on slide 28 in the slide deck from the
lecture 5 about finite trees.
The declaration for the abstract syntax for arithmetic expressions follows the grammar (slide 23): *)

type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtraction *)

let rec evalA exp env =
    match exp with
    | N n -> n 
    | V v -> Map.find v env
    | Add(a,b) -> evalA a env + evalA b env
    | Mul(a,b)-> evalA a env * evalA b env
    | Sub(a,b)-> evalA a env - evalA b env

// The declaration of the abstract syntax for boolean expressions is defined as follows (slide 25). 
type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *)
| Lt of aExp * aExp (* less than *)
| Neg of bExp (* negation *)
| Con of bExp * bExp (* conjunction *)

let rec evalB exp  env =
    match exp with
    | TT -> true
    | FF -> false
    | Eq(a,b) -> if evalA a env = evalA b env then true else false
    | Lt(a,b)-> if evalA a env < evalA b env then true else false
    | Neg a -> if a = TT then false else true
    | Con(a,b) -> 
        match evalB a env, evalB b env with
        |(true,true)-> true
        |(_,_) -> false
        

// The conjunction of two boolean values returns true if both values are true.
// The abstract syntax for the statements are defined as below (slide 26):
type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)

// Define 5 examples and evaluate them.
// For instance, consider the example stmt0 and initial state state0 below.

let stmt0 = Ass("res",(Add(N 10, N 30)))
let state0 = Map.empty

// You can then run the example as follows
// > I stmt0 state0;;
// val it : Map<string,int> = map [("res", 40)]
// and get the result state with variable res assigned the value 40 (as expected).

//TODO: complete the skeleton below using the types above:
let rec I stm env =
    match stm with
    | Ass(x,a) -> Map.add x (evalA a env) env
    | Skip -> env 
    | Seq(stm1, stm2) ->
            I stm1 env |> I stm2
    | ITE(b,stm1,stm2) -> if (evalB b env) = true then I stm1 env else I stm2 env 
    | While(b, stm3) -> 
        match evalB b env with
        | true -> I stm3 env
        | _ ->  I Skip env 

// need to test:
(*
SKIP stm1 -> if given an environment such as state0 with the SKIP keyword it will return state0 untouched -> PASSED
SEQ stm2 -> given an environment, it will perform two statements in sequential order on the env and returned the modified env
    example: 
        assign a = 10
        assing b = 12
        return map with a = 10  and b = 12
ITE -> given env perform x on env if b is true 
    if Lt(10,12) = true then 
WHILE
*)

let stm1 = I Skip state0
let stm2 = Seq(Ass(),Ass())