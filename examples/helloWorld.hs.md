## Hello World with Lit

Here is an overview of a hello world Haskell program.  We define * as the macro from which all other code or macros exist.
```Haskell
&lt&lt * &gt&gt;=
&lt&lt a trivial comment &gt&gt
&lt&lt print a string &gt&gt
```
Now we can define each of the above macros,
beginning with an inconsequential comment!
```Haskell
&lt&lt a trivial comment &gt&gt;=
-- this is a hello world haskell program
```
Lastly our program needs to print hello world
```Haskell
&lt&lt print a string &gt&gt;=
main = putStr "Hello, World!"
```
