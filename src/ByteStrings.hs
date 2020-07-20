module ByteStrings
  ( packTest,
    unpackTest,
    toChunkTest,
    fromChunkTest,
    readFileTest,
    ByteStrings.copyFile,
  )
where

import Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import GHC.Word
import System.Directory
import System.Environment
import System.IO

packTest :: B.ByteString

unpackTest :: [Word8]

toChunkTest :: [S.ByteString]

fromChunkTest :: B.ByteString

readFileTest :: IO B.ByteString

copyFile :: IO ()

packTest = B.pack [97 .. 122]

unpackTest =
  let bs = packTest
   in B.unpack bs

toChunkTest = B.toChunks packTest

fromChunkTest = B.fromChunks toChunkTest

readFileTest = B.readFile "LICENSE"

copyFile = do
  (src : dst : _) <- getArgs
  copy src dst
  where
    copy ::FilePath->FilePath->IO()
    copy src dst = do
      content <- B.readFile src
      bracketOnError
        (openTempFile "." "temp")
        ( \(tmpName, tmpHandle) -> do
            hClose tmpHandle
            removeFile tmpName
        )
        ( \(tmpName, tmpHandle) -> do
            B.hPutStr tmpHandle content
            hClose tmpHandle
            renameFile tmpName dst
        )
