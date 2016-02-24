module Server where
import Network.Socket

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
