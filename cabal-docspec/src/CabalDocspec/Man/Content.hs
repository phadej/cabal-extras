{-# LANGUAGE TemplateHaskell #-}
module CabalDocspec.Man.Content (
    manContent,
) where

import Language.Haskell.TH        (litE, stringL)
import Language.Haskell.TH.Syntax (qAddDependentFile, qRunIO)
import Prelude                    (String, readFile, ($))

manContent :: String
manContent = $(do
    let fp = "cabal-docspec.1"
    qAddDependentFile fp
    contents <- qRunIO (readFile fp)
    litE $ stringL contents
   )
