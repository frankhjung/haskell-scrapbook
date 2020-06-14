% Bird Style Literate Haskell
% Frank H Jung
% 23 May 2020

* * * * * * * * * *

This is the famous "Hello world" example, written in literate Haskell.

The following embedded code will be compiled and nicely display:

> {-|
>
> Module      : Bird
> Description : Example literate Haskell using Bird style.
> Copyright   : © Frank Jung, 2020
> License     : GPL-3
>
> Example literate Haskell using Bird style.
>
> -}

> module Main(main) where

> main :: IO ()
> main = putStrLn "Hello, world!"

== Notes

This is after a 2nd level heading.

=== Compile using GHC

This is after a 3rd level heading.

```bash
ghc --make bird.lhs
```
    $ ghc --make bird.lhs
    [1 of 1] Compiling Main             ( bird.lhs, bird.o )
    Linking bird ...

=== Documented using Haddock

```bash
haddock --title="Bird Style Literate Haskell" \
        --html \
        --hyperlinked-source \
        --odir public \
        bird.lhs
```

=== Render to HTML using pandoc

```bash
pandoc  -r markdown+lhs \
        -s bird.lhs \
        -w html \
        -css haskell.css \
        -o bird.html
```

=== Render to PDF using pandoc

```bash
pandoc  -r markdown+lhs \
        -s bird.lhs \
        --css haskell.css \
        -o bird.pdf
```

=== Run using runhaskell

```bash
runhaskell bird.lhs
```
    $ runhaskell bird.lhs
    Hello, world!
