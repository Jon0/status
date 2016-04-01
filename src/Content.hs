module Content where

import Control.Exception
import System.IO
import System.Posix

-- length of files maybe unknown
data DataStream = DataStream {
    dataLength :: Maybe FileOffset,
    dataHandle :: Handle
}

-- classes which rely on files for data
class DataDerived d where
    getStream :: d -> DataStream


nothingError :: IOException -> IO (Maybe t)
nothingError e = do
    print e
    return Nothing


contentSize :: FilePath -> IO (Maybe FileOffset)
contentSize filename =
    handle (nothingError) $ do
    stat <- getFileStatus filename
    return $ Just (fileSize stat)


contentOpen :: FilePath -> IO (Maybe DataStream)
contentOpen filename =
    handle (nothingError) $ do
    hdl <- openFile filename ReadMode
    hSetBinaryMode hdl True
    hSetBuffering hdl NoBuffering
    len <- contentSize filename
    return $ Just (DataStream len hdl)


contentClose :: DataStream -> IO ()
contentClose ct = do
    hClose (dataHandle ct)
