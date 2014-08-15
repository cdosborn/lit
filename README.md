#lit - a *modern* literate programming tool

Lit has several notable features:

- All prose can expressed in Markdown or plaintext
- Supports all programming languages in theory
- Literate constructs defined by indentation
- Watch literate files for changes

## Install 
In order to build, fetch cabal and run: 
```
$ git clone https://github.com/cdosborn/lit.git 
$ cd lit/
$ cabal configure
$ cabal build
$ cabal install
```
##Usage
```
Usage: lit OPTIONS... FILES...
  -h  --html          Generate html
  -m  --markdown      Generate markdown
  -c  --code          Generate code by file extension
      --css=FILE      Specify a css file for html generation
      --docs-dir=DIR  Directory for generated docs
      --code-dir=DIR  Directory for generated code
  -w  --watch         Watch for file changes, automatically run lit
  -v  --version       Print version
      --help          Display help
```

## What is literate programming?

Literate programming is a way to write programs prioritized for understanding. All literate programs boil
down to prose and literate constructs. This paragraph is a valid literate program.

Literate tools take a literate file and essentially generate two types of files. Primarily,
they generate the source code that computers understand. Because literate programs are inherently legible, 
these tools often generate nice documentation.

*Programs must be written for people to read, and only incidentally for machines to execute. <br>-H. Abelson and G. Sussman (SICP)*
## Example

Here is an example literate file `helloWorld.hs.lit`
```
Here is an overview of a hello world Haskell program. 
We define * as the macro from which all other code or macros exist.
    << * >>=
    << a trivial comment >>
    << print a string >>

Now we can define each of the above macros,
beginning with an inconsequential comment!
    << a trivial comment >>=
    -- this is a hello world haskell program

Lastly our program needs to print hello world
    << print a string >>=
    main = putStr "Hello, World!"
```

Lit can process the literate file in several ways. To simply produce the code file run:<br>
`$ lit -c helloWorld.hs.lit`

Which generates the file `helloWorld.hs`
```haskell
-- this is a hello world haskell program
main = putStr "Hello, World!"
```

## Syntax
Lit only has two valid constructs:

A macro definition: `<< ... >>=` and a macro reference: `<< ... >>`

Any lines sharing or to the right of the initial indent, are included in the definition.
A macro definition is where the actual source code is placed. It can also contain macro references.

It is also possible to extend a macro definition by creating a
macro definition with the same name later in the literate file.

When lit attempts to generate the source code from the literate file, it expands each
macro reference with the corresponding macro definition. By convention, lit starts at the root macro
definition `<< * >>=`, which must be included to generate source code.
