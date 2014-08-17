## Hello World with Lit

Here is an overview of a hello world Haskell program.  We define * as the macro from which all other code or macros exist.
```Haskell
&lt;&lt;%20*%20&gt;&gt;=
&lt;&lt;%20a trivial comment%20&gt;&gt;
&lt;&lt;%20print a string%20&gt;&gt;
```
Now we can define each of the above macros,
beginning with an inconsequential comment!
```Haskell
&lt;&lt;%20a trivial comment%20&gt;&gt;=
-- this is a hello world haskell program
```
Lastly our program needs to print hello world
```Haskell
&lt;&lt;%20print a string%20&gt;&gt;=
main = putStr "Hello, World!"
```
