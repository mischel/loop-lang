# Loop-Lang

This package provides an interpreter for the *Loop* programming language.


# Example

```haskell
#!/usr/bin/env stack
-- stack runhaskell --package loop-lang -- -threaded
{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

import           Prelude hiding (fromInteger)
import qualified Prelude (fromInteger)

import           Loop

-- use rebindable syntax for Int literals for some extra syntactic sugar
fromInteger :: Integer -> Int
fromInteger = Prelude.fromInteger

x1 = "x1"
x2 = "x2"
y = "y"
z = "z"

half = do
  x1 .= z; x2 .= z 
  nloop y $ do
    z .= x1; x1 .= x2 .+ 1; x2 .= z
  where (>>) = sequ

half' z0 y0 = do
  putStrLn $ ">>> half " ++ show z0 ++ " " ++ show y0
  putStr   $ pretty [z] $ eval half [z <~ z0, y <~ y0]
  putStrLn "<<<\n"

main = do
  half' 0 4
  half' 6 3
```


```bash
$ ./examples/half.hs
>>> half 0 4
  z = 0	
  z = 0	
  z = 1	
  z = 1	
  z = 2	
<<<

>>> half 6 3
  z = 6	
  z = 6	
  z = 7	
  z = 7	
<<<
```


# Literature

  - A. R. Meyer and D. M. Ritchie, _The Complexity of Loop Programs_, 1967
  - A. M. Ben-Amram, N. D. Jones, and L. Kristiansen, _Linear, Polynomial or Exponential? Complexity Inference in Polynomial Time_, 2008

