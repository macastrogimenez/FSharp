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

let explode (s:string) = s.ToCharArray() |> List.ofArray

let rec explode2 (s:string) = 
    match s with
    |"" -> []
    |_ -> s.[0]:: explode2 (s.Remove(0,1))