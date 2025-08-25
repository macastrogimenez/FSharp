let testList = [1.0 .. 10.0] 

let f m fl = 
    match fl with
    |[] -> 0.00
    |_ -> List.fold (fun acc x -> acc + x) 0 (List.map (fun x -> x*m) fl)
//test: f 2 testList;; -> PASSED

let f2 m fl = 
    match fl with
    |[] -> 0.00
    |_ -> (List.fold (fun acc x -> acc + x) 0.00 fl) * m 
// test: f2 2 testList;;

let g fl = 
    f 15.00 fl

//test: g testList;; -> PASSED

type concat<'a> = 
    E
    |C of 'a * concat<'a>

let v = C('A',C('B',C('C',E)))

let count c= 
    let rec go c acc =
        match c with 
        |E -> acc
        |C (a,b)-> go b (acc+1)
    go c 0

// test: count v;;

let rec map f v = 
    match v with 
    |E -> E
    |C (a,b)->  C (f a, map f b)
    

//test: map char.toLower v;;

// map (fun x -> x.) v;; works

let rec append v1 v2 = 
    match v1 with
    | E -> v2
    | C (a,b) -> append b v2

