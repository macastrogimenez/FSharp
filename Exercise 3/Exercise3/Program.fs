(*
Exercise 3.1 Write a function
downTo:int->int list
so that downTo n returns the n-element list [n; n-1; . . .; 1]. You must use if-then-else expressions to
define the function.
Secondly define the function downTo2 having same semantics as downTo. This time you must use pattern
matching.
*)
let downTo n = 
    if n >= 1 then 
        [n .. -1 .. 1]
    else 
        []

let downTo2 n =
    match n with
    | n when n <= 0 -> []
    | _ -> [n .. -1 .. 1]

(*Exercise 3.2 Write a function
removeOddIdx:int list->int list
so that removeOddIdx xs removes the odd-indexed elements from the list xs:
removeOddIdx [x0; x1; x2; x3; x4; ...] = [x0; x2; x4; ...]
removeOddIdx [] = []
removeOddIdx [x0] = [x0]*)
let rec removeOddIdx xs = 
    match xs with 
    |[] -> []
    |x::xs -> if x % 2 = 0 then x :: removeOddIdx xs else removeOddIdx xs
    
let zeroToTen = [0.. +1 .. 10]

(*
Exercise 3.3 Write a function
combinePair:int list->(int*int) list
so that combinePair xs returns the list with elements from xs combined into pairs. If xs contains an odd
number of elements, then the last element is thrown away:
combinePair [x1; x2; x3; x4] = [(x1,x2);(x3,x4)]
combinePair [x1; x2; x3] = [(x1,x2)]
combinePair [] = []
combinePair [x1] = []
*)

let rec combinePair xs =
    match xs with
    | []-> []
    | x::xy -> 
        if List.isEmpty xy then [] else 
        (x, List.head xy)::combinePair (List.tail xy)

let oddElementsList = [0..+1..10]
let evenElementsList = [0..+1..11]
let emptyList : list<int> = []
let oneElementList = [1]
