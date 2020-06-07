module CabalIfaceQuery.GHC.NameCacheUpdater (
    NameCacheUpdater,
    makeNameCacheUpdater,
) where

import Data.IORef (atomicModifyIORef', newIORef)
import Prelude    (IO, return, ($))

import IfaceEnv   (NameCacheUpdater (..))
import NameCache  (initNameCache)
import UniqSupply (mkSplitUniqSupply)

makeNameCacheUpdater :: IO NameCacheUpdater
makeNameCacheUpdater = do
    usQ <- mkSplitUniqSupply 'q'
    ncRef <- newIORef $ initNameCache usQ []
    return (NCU (atomicModifyIORef' ncRef))
