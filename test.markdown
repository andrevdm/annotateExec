# Main
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Protolude

import qualified AnnotateExec as E

main :: IO ()
main = E.exec [E.printTerminal, E.saveMarkdown "./test.markdown"] $ do
  -- Normal comment
  let a = 1
  let b = 2
  E.prn $ a + b
```

```
>  3
```

```haskell
  -- Space above is retained for console output but not markdown
  let b = [1,2,3]
  E.pprn $ b <> [4,5]
```

```
>  [ 1
>  , 2
>  , 3
>  , 4
>  , 5
>  ] 
```
