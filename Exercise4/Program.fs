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