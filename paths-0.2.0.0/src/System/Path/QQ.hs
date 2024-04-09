{-# LANGUAGE CPP                   #-}

#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE Safe                  #-}
#else
{-# LANGUAGE Trustworthy           #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell       #-}
#endif

-- | QuasiQuoters for 'Path's
--
-- @since 0.2.0.0
module System.Path.QQ
    ( fspath
    , unrooted
    ) where

import           Language.Haskell.TH
import qualified Language.Haskell.TH.Quote as QQ
import qualified System.FilePath.Posix     as FP.Posix

import           System.Path.Internal

-- | Quasiquoter that materialises a value with a type of one of
--
--  * 'Path' 'Absolute'
--  * 'Path' 'HomeDir'
--  * 'Path' 'CWD'
--
-- depending on the POSIX-style path literal given.
--
-- @since 0.2.0.0
fspath :: QQ.QuasiQuoter
fspath = quoter qfspath

qfspath :: FilePath -> Q Exp
qfspath fp
  | FP.Posix.isAbsolute fp  = qPath fp  [t|Absolute|]
  | Just fp' <- atHome fp   = qPath fp' [t|HomeDir|]
  | otherwise               = qPath fp  [t|CWD|]
  where
    atHome :: FilePath -> Maybe FilePath
    atHome "~"           = Just ""
    atHome ('~':sep:fp') | FP.Posix.isPathSeparator sep = Just fp'
    atHome _otherwise    = Nothing


-- | Quasiquoter for constructing 'Path' 'Unrooted' from POSIX-style path literals.
--
-- @since 0.2.0.0
unrooted :: QQ.QuasiQuoter
unrooted = quoter qunrooted

qunrooted :: FilePath -> Q Exp
qunrooted fp
  | FP.Posix.isAbsolute fp = fail "Unrooted path must be relative"
  | otherwise              = qPath fp [t|Unrooted|]

-- | Helper for constructing 'Path x :: Path t' as TH expression
qPath :: FilePath -> Q Type -> Q Exp
qPath fp qtagTy = do
  pathCon    <- [|Path|]
  pathTy     <- [t|Path|]
  tagTy      <- qtagTy
  return (SigE (AppE pathCon (LitE (StringL fp))) (AppT pathTy tagTy))

-- | Helper
quoter :: (String -> Q Exp) -> QQ.QuasiQuoter
quoter x = QQ.QuasiQuoter { QQ.quoteExp  = x
                          , QQ.quotePat  = \_ -> fail "pattern position not supported"
                          , QQ.quoteType = \_ -> fail "using as type not supported"
                          , QQ.quoteDec  = \_ -> fail "using as declaration not supported"
                          }
