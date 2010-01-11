module Web (serverListen, sendFrame, readFrame) where

import Network
import System.IO
import Char
import Control.Concurrent
import Control.Monad

-- restarting an apache server (apache2ctl restart)
-- magic configuration file /etc/apache2/sites-available/default

-- http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

-- tcpdump -i lo (run as sudo)

-- Should really parse this out to headers!
serverHandshake :: String
serverHandshake = 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
    \Upgrade: WebSocket\r\n\
    \Connection: Upgrade\r\n\
    \WebSocket-Origin: http://localhost\r\n\
    \WebSocket-Location: ws://localhost:9876/\r\n\
    \WebSocket-Protocol: sample\r\n\r\n"

acceptLoop :: Socket -> (Handle -> IO ()) -> IO a
acceptLoop socket f = forever $ do
                        (h,_,_) <- accept socket
                        hPutStr h serverHandshake
                        hSetBuffering h NoBuffering
                        forkIO (f h)  

serverListen :: PortNumber -> (Handle -> IO()) -> IO()
serverListen port f = withSocketsDo $ do
                        socket <- listenOn (PortNumber port)
                        acceptLoop socket f
                        sClose socket
                        return ()

sendFrame :: Handle -> String -> IO ()
sendFrame h s = do
  hPutChar h (chr 0)
  hPutStr h s
  hPutChar h (chr 255)

readFrame :: Handle -> IO String
readFrame h = readUntil h ""
    where
      readUntil hl str = do
        new <- hGetChar hl
        if new == chr 0
          then readUntil hl ""
          else if new == chr 255
            then return str
            else readUntil hl (str ++ [new])



