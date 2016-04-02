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
    getStrFormat :: d -> IO String
    getStream :: d -> DataStream


printError :: IOException -> IO ()
printError e = do
    print e


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


-- try outputing a stream to a handle
tryOutput :: (DataDerived d) => Handle -> d -> IO ()
tryOutput hdl dat =
    handle (printError) $ do
    ct <- getStrFormat dat
    hPutStrLn hdl ct
