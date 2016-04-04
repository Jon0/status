module Content where

import Control.Exception
import System.IO
import System.Posix


data StreamSet = StreamSet {
    openStreams :: [DataStream],
    nextId :: Integer
}


emptyStreamSet :: StreamSet
emptyStreamSet = StreamSet [] 0



-- length of files maybe unknown
data DataStream = DataStream {
    dataId :: Integer,
    dataPath :: FilePath,
    dataLength :: Maybe FileOffset,
    dataHandle :: Handle
}


data StreamTransfer = StreamTransfer {
    transferLength :: Maybe FileOffset,
    transferFn :: FileOffset -> IO (String, Bool)
}

-- serialisable types
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


contentCreateHandle :: Integer -> FilePath -> IO (Maybe DataStream)
contentCreateHandle uid path =
    handle (nothingError) $ do
    hdl <- openFile path ReadMode
    hSetBinaryMode hdl True
    hSetBuffering hdl NoBuffering
    len <- contentSize path
    return $ Just (DataStream uid path len hdl)


streamSetAppend :: StreamSet -> DataStream -> StreamSet
streamSetAppend set item =
    StreamSet (item : (openStreams set)) ((nextId set) + 1)


notEqualIds :: Integer -> DataStream -> Bool
notEqualIds uid item = uid /= (dataId item)


streamSetRemove :: StreamSet -> DataStream -> StreamSet
streamSetRemove set item =
    StreamSet (filter (notEqualIds (dataId item)) (openStreams set)) (nextId set)


contentOpen :: StreamSet -> FilePath -> IO (StreamSet, Maybe DataStream)
contentOpen set filename = do
    mHdl <- contentCreateHandle (nextId set) filename
    case mHdl of
        Nothing -> do
            return (set, Nothing)
        Just stream -> do
            return ((streamSetAppend set stream), Just stream)


contentClose :: StreamSet -> DataStream -> IO StreamSet
contentClose set ct = do
    hClose (dataHandle ct)
    return (streamSetRemove set ct)


contentCloseAll :: StreamSet -> IO ()
contentCloseAll set = do
    putStrLn ("Closing " ++ (show (map dataPath (openStreams set))))
    mapM hClose (map dataHandle (openStreams set))
    return ()



readSomeError :: IOException -> IO (String, Bool)
readSomeError e = do
    print e
    return ("", True)


-- read some bytes from handle
readSomeStream :: DataStream -> FileOffset -> IO (String, Bool)
readSomeStream _ 0 = do
    return ("", False)
readSomeStream stream chars =
    handle (readSomeError) $ do
    putStrLn ("get " ++ (show chars))
    char <- hGetChar (dataHandle stream)
    (rest, end) <- readSomeStream stream (chars - 1)
    return ((char : rest), end)


createStreamTransfer :: DataStream -> StreamTransfer
createStreamTransfer s = StreamTransfer (dataLength s) (readSomeStream s)



readSomeString :: String -> FileOffset -> IO (String, Bool)
readSomeString str chars = do
    putStrLn "return string"
    return (str, True)


createStringTransfer :: String -> StreamTransfer
createStringTransfer s = StreamTransfer (Just (fromIntegral (length s))) (readSomeString s)




-- send content in fragments
sendAllContent :: Handle -> StreamTransfer -> FileOffset -> IO ()
sendAllContent hdl str size = do
    putStrLn ("requesting " ++ (show size) ++ "bytes")
    (ct, end) <- (transferFn str) $ size
    putStrLn ("writing " ++ (show (length ct)) ++ "bytes")
    hPutStrLn hdl ct
    case end of
        True -> do
            return ()
        False -> do
            sendAllContent hdl str size


-- try outputing a stream to a handle
tryOutput :: (DataFormat t) => Handle -> [t] -> IO ()
tryOutput hdl dat =
    handle (printError) $ do
    putObjects hdl dat
