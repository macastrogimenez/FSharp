# FSharp
Repo with all the solved exercises for the course Functional Programming from ITU.

# 24/07/2025 - Exercise battery 1: 
Regarding basic functions, recursion, environments, types and concatenation of strings.

# 25/07/2025 - Exercise battery 2:
Regarding tuples vs curried functions, if statements, recursion and pattern matching, types and evaluation + eager evaluation.

# 28/07/2025 - Exercise battery 3:
Regarding lists, recursion and pattern matching on lists.

# 31/07/2025 - Exercise battery 4:
Regarding lists, library functions for lists (such as fold, foldBack, map), strings and string manipulation, pipe-forward operator, 
and composition of lambdas for higher order functions.

# 13/08/2025 - Exercise battery 5:
Regarding Binary trees, Binary tree traversals, types - defining types, and function evaluation (if the output changes 
for one branch of the pattern matching process, it changes for all).

I used types to create a "new" programming language, defining its own statements such as 'If-then-else', 'sequential composition',  
'while loops' (using recursion), etc and then used functions to define an interpreter for statements made using these new types.

# 14/08/2025 - Exercise battery 6:
Regarding types, expression trees, pattern matching, modules, and lists to create a calculator program.

I had to create the stack data type using lists create a module with functions particular to this data type.
I then used this module to implement arithmetic expression evaluation (as per Dijkstra's double stack 
method here: https://writings.sh/post/en/arithmetic-expression). 
However, instead of using two stacks I used one stack for the floating point numbers that took part of the calculation and a list where the instructions would be added and scanned from an Expression Tree.

# 14/08/2025 - Exercise battery 7:
Regarding efficiency - stack and heap (garbage collection) -> different ways of writing the same functions for more efficiency: tail-recursion with iterative(cumulative) functions, continuations, etc.
Big focus on CPS - continuation passing style.

# 24/08/2025 - Exercise battery 8:
Regarding efficiency - developing tail-recursive functions with accumulators and continuations.
Sequences - creating basic sequences and more complex ones (factorial function sequence).
