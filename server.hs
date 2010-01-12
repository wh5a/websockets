import Network (listenOn, PortID(..), accept, withSocketsDo, sClose, Socket)
import System.IO
import Control.Concurrent (forkIO)
import Data.Char (chr, toUpper)
import Control.Monad (liftM, forever)

-- WebSocket-Origin specifies the origin of the client web page, similar to the same-origin principle.
-- It must match the settings of the web server.  
-- WebSocket-Location specifies the settings of this web sockets server.
-- Using JavaScript, a web page hosted on WebSocket-Origin talks to a web sockets server listening on WebSocket-Location.
serverHandshake = 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
    \Upgrade: WebSocket\r\n\
    \Connection: Upgrade\r\n\
    \WebSocket-Origin: http://localhost:8000\r\n\
    \WebSocket-Location: ws://localhost:9876/\r\n\
    \WebSocket-Protocol: sample\r\n\r\n\0"

acceptLoop socket = forever $ do
                      (h,_,_) <- accept socket
                      hPutStr h serverHandshake
                      hSetBuffering h NoBuffering
                      forkIO (sendFrame h "hi you're connected!" >> sendFrame h "ain't that grand?" >> listenLoop h)  

main = withSocketsDo $ do
         socket <- listenOn (PortNumber 9876)
         acceptLoop socket
         sClose socket

listenLoop :: Handle  -> IO ()
listenLoop h = do
  sendFrame h . (map toUpper) =<< readFrame h ""
  listenLoop h

sendFrame :: Handle -> String -> IO ()
sendFrame h s = do
  hPutChar h (chr 0)
  hPutStr h s
  hPutChar h (chr 255)

readFrame :: Handle -> String -> IO String
readFrame h str  = do
  new <- hGetChar h
  if new == chr 0
     then readFrame h ""
     else if new == chr 255
          then return $ reverse str
          else readFrame h (new:str)
