let testList = [-10..+2..14]

let tuplelist = [(2,3);(3,4);(0,2);(7,1)]

let posList xs = List.map (fun x -> x > 0) xs

let addTuples xs = List.map (fun (x,y) -> x+y) xs
// test addTuples tuplelist;; 
let isMember x ys = List.exists x ys

// example test : isMember (fun x-> x=0) testList;;

let disjoint (xs:list<int>) (ys:list<int>) = List.forall (fun x -> not (isMember x ys)) xs

let oddList = [1..+2..9]
let evenList = [0..+2..10]