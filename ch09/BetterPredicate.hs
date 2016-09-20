-- file: ch09/BetterPredicate.hs
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -- path
               -> Permissions -- permission
               -> Maybe Integer -- file size
               -> ClockTime -- last modified
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize = undefined

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
    perms <- getPermissions name
    size <- getFileSize name
    modified <- getModificationTime name
    return (p name perms size modified)