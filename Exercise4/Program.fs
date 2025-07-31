(*
Exercise 4.1 Write a function
explode:string->char list
so that explode s returns the list of characters in s:
explode "star" = [’s’;’t’;’a’;’r’]
Hint: if s is a string then s.ToCharArray() returns an array of characters. You can then use List.ofArray
to turn it into a list of characters.
Now write a function
explode2:string->char list
similar to explode except that you now have to use the string function s.Chars (or .[]), where s is a string.
You can also make use of s.Remove(0,1). The definition of explode2 will be recursive
*)
let testS = "Miguel is the Best"
let explode (s:string) = s.ToCharArray() |> List.ofArray

let rec explode2 (s:string) = 
    match s with
    |"" -> []
    |_ -> s.[0]:: explode2 (s.Remove(0,1))

(*
Exercise 4.2 Write a function
implode:char list->string
so that implode s returns the characters concatenated into a string:
implode [’a’;’b’;’c’] = "abc"
Hint: Use List.foldBack.
Now write a function
implodeRev:char list->string
so that implodeRev s returns the characters concatenated in reverse order into a string:
implodeRev [’a’;’b’;’c’] = "cba"
Hint: Use List.fold.
*)
let abc: char list = ['a';'b';'c']

let implode xs = List.foldBack (fun c s -> string c + s) xs ""

// This doesn't work because foldBack expects a function with TWO parameters
// let implode2 xs = List.foldBack (fun x -> string x) xs ""

// test: implode abc;; val it: string = "abc"

let implodeRev xs = List.fold (fun s (c:char) -> string c + s) "" xs 
// test: implodeRev abc;; val it: string = "cba"

(*
Exercise 4.3 Write a function
toUpper:string->string
so that toUpper s returns the string s with all characters in upper case:
toUpper "Hej" = "HEJ"
Hint: Use System.Char.ToUpper to convert characters to upper case. You can do it in one line using
implode, List.map and explode.
Write the same function toUpper1 using forward function composition
((f >> g) x = g(f x)).
Write the same function toUpper2 using the pipe-forward operator (|>) and backward function composition
(<<).
Hint: << is defined as (f << g) x = (f o g) x = f(g(x)).
Hint: |> is defined as x |> f = f x.
The two operators are by default supported by F#. You can have F# interactive print the types:
1
KSFUPRO1KU, Functional Programming ITU, Spring 2025
> (<<);;
val it : ((’a -> ’b) -> (’c -> ’a) -> ’c -> ’b) = <fun:it@3-4>
> (|>);;
val it : (’a -> (’a -> ’b) -> ’b) = <fun:it@4-5>
> (>>);;
val it : ((’a -> ’b) -> (’b -> ’c) -> ’a -> ’c) = <fun:it@5-6>
*)


let toUpper s = s |> explode |> List.map System.Char.ToUpper |> implode
// this uses the pipeline technique, starting from the input and simply adding the functions to it
// notation: x |> f = f x

// test: toUpper "migul";; val it: string = "MIGUL"

let toUpper2 s = 
    implode (List.map System.Char.ToUpper (explode s))
// this wraps the functions once inside another, being the inner most the first one to take place and the outer most the last one
// notation: f(g(x))

// test: toUpper2 "migul";; val it: string = "MIGUL"

let toUpper3 s =
    (implode << List.map System.Char.ToUpper << explode) s
// this makes a pipeline of functions from last one to first one - left to right - inside a parenthesis
// and passes the parameter right outside the parenthesis
// notation: (f << g) x = f(g(x)).

// test: toUpper3 "heyooo ulita";; val it: string = "HEYOOO ULITA"    

(*
Exercise 4.4 Write a function
palindrome:string->bool,
so that palindrome s returns true if the string s is a palindrome; otherwise false.
A string is called a palindrome if it is identical to the reversed string, eg, “Anna” is a palindrome but “Ann” is not.
The function is not case sensitive.
*)

let palindrome s =
    let palidromOfS = s |> explode |> List.map System.Char.ToLower |> implodeRev
    if s.ToLower() = palidromOfS then true else false

(*
Exercise 4.5 The Ackermann function is a recursive function where both value and number of mutually recursive
calls grow rapidly.
Write the function
ack:int*int->int
that implements the Ackermann function using pattern matching on the cases of (m,n) as given below.
A(m, n) =



n + 1 if m = 0
A(m − 1, 1) if m > 0 and n = 0
A(m − 1, A(m, n − 1)) if m > 0 and n > 0
What is the result of ack(3,11).
Notice: The Ackermann function is defined for non negative numbers only
*)

let rec ack (m, n) = 
    match (m,n) with
    | (0,n) -> n+1
    | (m,0) -> ack (m-1,1)
    | (m,n) when (m > 0 && n > 0) -> ack(m-1,ack(m,n-1))
    | (m,n) when (m < 0 || n < 0) -> failwith "invalid value, none of the functions parameters can be negative"


let rec ackSafe (m,n) = 
    match (m,n) with
    | (m,n) when (m < 0 || n < 0) -> None
    | (0,n) -> Some(n+1)
    | (m,0) -> ackSafe (m-1,1)
    | (m,n) -> ackSafe (m-1,ack(m,n-1))        

(*
Exercise 4.6 The function
time f:(unit->’a)->’a*TimeSpan
below times the computation of f x and returns the result and the real time used for the computation.
let time f =
let start = System.DateTime.Now in
let res = f () in
let finish = System.DateTime.Now in
(res, finish - start);
Try compute time (fun () -> ack (3,11)).
Write a new function
timeArg1 f a : (’a -> ’b) -> ’a -> ’b * TimeSpan
that times the computation of evaluating the function f with argument a. Try timeArg1 ack (3,11).
Hint: You can use the function time above if you hide f a in a lambda (function).
*)
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);

(*
>  time (fun () -> ack (3,11));; 
val it: int * System.TimeSpan =
  (16381, 00:00:00.5269360 {Days = 0;
                            Hours = 0;
                            Microseconds = 936;
                            Milliseconds = 526;
                            Minutes = 0;
                            Nanoseconds = 0;
                            Seconds = 0;
                            Ticks = 5269360L;
                            TotalDays = 6.098796296e-06;
                            TotalHours = 0.0001463711111;
                            TotalMicroseconds = 526936.0;
                            TotalMilliseconds = 526.936;
                            TotalMinutes = 0.008782266667;
                            TotalNanoseconds = 526936000.0;
                            TotalSeconds = 0.526936;})
*)

let timeArg1 f a = time (fun () -> f a )
// test: timeArg1 ack (3,11);;

(*
Exercise 4.7 HR exercise 5.4
Declare a function downto1 such that:
downto1 f n e= f(1,f(2,...,f(n−1,f(n,e))...)) for n > 0
downto1 f n e= e for n ≤ 0
Declare the factorial function by use of downto1.
Use downto1 to declare a function that builds the list [g(1),g(2),...,g(n)] for a function g
and an integer n.

In Code Judge, we use the faculty function as the function g:
let rec fact = function
| 0 -> 1
| n when n > 0 -> n * fact(n-1)
| _ -> failwith "fact only works on positive numbers"
We can then call buildList as
buildList fact n
where n is a positive integer.
*)

let downto1 f n e = 
    match n with
    | n when n <= 0 -> e 
    | n when n > 0 -> 
        let listN = [1.. +1 ..n]
        List.foldBack f listN e

let fact n = downto1 (fun x e -> x * e) n 1;; 

let rec fact2 = function
| 0 -> 1
| n when n > 0 -> n * fact(n-1)
| _ -> failwith "fact only works on positive numbers"

let buildList n = downto1 (fun n e-> fact2 n::e) n []

