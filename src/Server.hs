module Server where

import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.ByteString as BStr
import System.IO
import Network.Socket
import Config


data ProcStream = ProcStream {
    procThread :: ThreadId,
    procResult :: Chan BStr.ByteString
}


forkStream :: (Chan BStr.ByteString -> IO ()) -> IO ProcStream
forkStream handler = do
    chan <- newChan
    pid <- forkIO (handler chan)
    return $ ProcStream pid chan


-- a set of open connections
data SocketSet = SocketSet { socketHandles :: [ProcStream] }


-- socket to listen on a port
openSocket :: PortNumber -> IO Socket
openSocket port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    return sock


mainLoop :: (Handle -> IO ()) -> Socket -> IO ()
mainLoop handler sock = do
    putStrLn "Wait for connection..."
    connection <- accept sock
    forkIO (respond handler connection)
    mainLoop handler sock


respond :: (Handle -> IO ()) -> (Socket, SockAddr) -> IO ()
respond handler (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    handler hdl
    hClose hdl


respond2 :: (Socket, SockAddr) -> IO ()
respond2 (sock, _) = do
    send sock "Hello!\n"
    close sock


-- opens port 8080 and applies handler to recieved connections
acceptLoop :: (Handle -> IO ()) -> PortNumber -> IO ()
acceptLoop handler port = do
    sock <- openSocket port
    mainLoop handler sock
