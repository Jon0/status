module Content where

import Control.Exception
import System.IO
import System.Posix

-- length of files maybe unknown
data DataStream = DataStream {
    dataLength :: Maybe FileOffset,
    dataHandle :: Handle
}


class DataObject t where
    showObject :: t -> String
    readObject :: String -> t

-- classes which rely on files for data
class (DataObject t) => DataFormat t where
    readObjects :: Handle -> IO [t]
    putObjects :: Handle -> [t] -> IO ()


class (DataFormat t) => DataDependency d t | d -> t where
    getStream :: d -> DataStream
    getFormat :: d -> t


printError :: IOException -> IO ()
printError e = do
    print e


emptyError :: IOException -> IO [t]
emptyError e = do
    print e
    return []


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
tryOutput :: (DataFormat t) => Handle -> [t] -> IO ()
tryOutput hdl dat =
    handle (printError) $ do
    putObjects hdl dat
