% Bird Style Literate Haskell
% Frank H Jung
% 23 May 2020

* * * * * * * * * *

This is the famous "Hello world" example, written in literate Haskell.

This embedded code will be compiled.

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

Notes
=====

Compile using GHC
--------------------

```bash
ghc --make bird.lhs
```
    $ ghc --make bird.lhs
    [1 of 1] Compiling Main             ( bird.lhs, bird.o )
    Linking bird ...

Documented using Haddock
---------------------------

```bash
haddock --title="Bird Style Literate Haskell" --html --hyperlinked-source --odir public bird.lhs
```

Render to HTML using pandoc
---------------------------

```bash
pandoc -r markdown+lhs -s bird.lhs -w html -o bird.html
```

Run using runhaskell
--------------------

```bash
runhaskell bird.lhs
```
    $ runhaskell bird.lhs
    Hello, world!

