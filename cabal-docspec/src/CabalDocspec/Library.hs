module CabalDocspec.Library where

import Peura

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Parsec                    as C
import qualified Distribution.Pretty                    as C
import qualified Distribution.Types.PackageName         as C
import qualified Distribution.Types.UnqualComponentName as C
import qualified Text.PrettyPrint                       as PP

data Library = Library !PackageName !LibraryName
  deriving (Eq, Ord, Show)

instance C.Parsec Library where
    parsec = do
        pn <- C.parsec
        ln  <- fmap (fromMaybe LMainLibName) $ optional $ do
            _ <- P.char ':'
            qn <- C.parsec
            return $
                if C.unPackageName pn == C.unUnqualComponentName qn
                then LMainLibName
                else LSubLibName qn

        return (Library pn ln)

instance C.Pretty Library where
    pretty (Library pn LMainLibName)     = C.pretty pn
    pretty (Library pn (LSubLibName ln)) = C.pretty pn <> PP.colon <> C.pretty ln
