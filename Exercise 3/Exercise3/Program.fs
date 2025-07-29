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

// Exercise 3.4 Solve HR, exercise 3.2.
(*
The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
functions to add and subtract two amounts, represented by triples (pounds,shillings,pence) of
integers, and declare the functions when a representation by records is used. Declare the func-
tions in infix notation with proper precedences, and use patterns to obtain readable declarations.
*)

// first with triples
let MigulTreasure = (10, 15, 11)
let UlitaTreasure = (10, 5, 11)

//added should be (8, 1, 23)

// in pence: 2830 + 2710 = 5540
let (+.) (penceA:int, shillingsA:int,poundsA: int) (penceB:int, shillingsB:int,poundsB: int) =
    let sumPounds =  ((poundsB*20*12+shillingsB*12+penceB) + (poundsA*20*12+shillingsA*12+penceA))/(20*12)
    let sumShillings = (((poundsB*20*12+shillingsB*12+penceB) + (poundsA*20*12+shillingsA*12+penceA))%(20*12))/12
    let sumPence = (((poundsB*20*12+shillingsB*12+penceB) + (poundsA*20*12+shillingsA*12+penceA))%(20*12))%12
    (sumPence, sumShillings, sumPounds)

let (-.) (penceA:int, shillingsA:int,poundsA: int) (penceB:int, shillingsB:int,poundsB: int) =
    let finalPounds =  ((poundsA*20*12+shillingsA*12+penceA) - (poundsB*20*12+shillingsB*12+penceB))/(20*12)
    let finalShillings = (((poundsA*20*12+shillingsA*12+penceA) - (poundsB*20*12+shillingsB*12+penceB))%(20*12))/12
    let finalPence = (((poundsA*20*12+shillingsA*12+penceA) - (poundsB*20*12+shillingsB*12+penceB))%(20*12))%12
    (finalPence, finalShillings, finalPounds)

// now with record types

type BritishCurrency = {
    pence: int
    shillings: int
    pounds: int
}
let (+/) (a:BritishCurrency) (b: BritishCurrency) = 
        let finalPounds =  ((a.pounds*20*12+a.shillings*12+a.pence) + (b.pounds*20*12+b.shillings*12+b.pence))/(20*12)
        let finalShillings = (((a.pounds*20*12+a.shillings*12+a.pence) + (b.pounds*20*12+b.shillings*12+b.pence))%(20*12))/12
        let finalPence = (((a.pounds*20*12+a.shillings*12+a.pence) + (b.pounds*20*12+b.shillings*12+b.pence))%(20*12))%12
        let addedBritishCcy: BritishCurrency = 
            {
                pence = finalPence
                shillings = finalShillings
                pounds = finalPounds
            }
        addedBritishCcy

let (-/) (a:BritishCurrency) (b: BritishCurrency) = 
        let finalPounds =  ((a.pounds*20*12+a.shillings*12+a.pence) - (b.pounds*20*12+b.shillings*12+b.pence))/(20*12)
        let finalShillings = (((a.pounds*20*12+a.shillings*12+a.pence) - (b.pounds*20*12+b.shillings*12+b.pence))%(20*12))/12
        let finalPence = (((a.pounds*20*12+a.shillings*12+a.pence) - (b.pounds*20*12+b.shillings*12+b.pence))%(20*12))%12
        let substractedBritishCcy: BritishCurrency = 
            {
                pence = finalPence
                shillings = finalShillings
                pounds = finalPounds
            }
        substractedBritishCcy

let migTresure = 
    {
        pence = 10
        shillings = 15
        pounds = 11
    }

let uliTreasure = 
    {
        pence = 10
        shillings = 5
        pounds = 11
    }


