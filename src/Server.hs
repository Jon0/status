module Server where

import Control.Concurrent
import System.IO
import Network.Socket
import Route

open_socket1 = do
    return socket (AF_INET, Stream, 8080)


-- socket to listen on port 8080
open_socket :: PortNumber -> IO Socket
open_socket port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    return sock


main_loop :: Socket -> IO ()
main_loop sock = do
    putStrLn "Wait for connection..."
    connection <- accept sock
    forkIO (respond connection)
    main_loop sock


respond :: (Socket, SockAddr) -> IO ()
respond (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "HTTP/1.1 200 OK";
    hPutStrLn hdl "Content-Length: 6";
    hPutStrLn hdl ""
    hPutStrLn hdl "Hello!"
    hClose hdl


respond2 :: (Socket, SockAddr) -> IO ()
respond2 (sock, _) = do
    send sock "Hello!\n"
    close sock


accept_loop = do
    sock <- open_socket 8080
    main_loop sock
