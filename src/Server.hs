module Server where

import Control.Concurrent
import System.IO
import Network.Socket

-- socket to listen on a port
open_socket :: PortNumber -> IO Socket
open_socket port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    return sock


main_loop :: (Handle -> IO ()) -> Socket -> IO ()
main_loop handler sock = do
    putStrLn "Wait for connection..."
    connection <- accept sock
    forkIO (respond handler connection)
    main_loop handler sock


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
accept_loop :: (Handle -> IO ()) -> IO ()
accept_loop handler = do
    sock <- open_socket 8080
    main_loop handler sock
