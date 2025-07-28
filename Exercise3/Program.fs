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

let removeOddIdx xs = 
    match xs with 
    |[] -> []
    |x::ys -> List.map (fun x -> if x%2 = 0 then Some(x) else None) xs
// TODO: the function returns a map with the NONEs and Somes and we only want the even values, how to completely
// skip the odd values? Fix and test again.
let oneToTen = [1.. +1 .. 10]