import Network
import System.IO
import Char

-- restarting an apache server (apache2ctl restart)
-- magic configuration file /etc/apache2/sites-available/default

-- http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

-- tcpdump -i lo (run as sudo)

-- Should really parse this out to headers!
serverHandshake = 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
    \Upgrade: WebSocket\r\n\
    \Connection: Upgrade\r\n\
    \WebSocket-Origin: http://localhost\r\n\
    \WebSocket-Location: ws://localhost:9876/\r\n\
    \WebSocket-Protocol: sample\r\n\r\n"

main = withSocketsDo $ do
         socket <- listenOn (PortNumber 9876)
         (h,_,_) <- accept socket
         x <- listenLoop h
         y <- getLine
         sClose socket
         return ()
         
listenLoop :: Handle  -> IO (Bool)
listenLoop h = do
  hPutStr h serverHandshake
  sendFrame h "hello"
  sendFrame h "world"
  x <- hGetLine h
  --  sendFrame h "hi"
  return (True)

sendFrame :: Handle -> String -> IO ()
sendFrame h s = do
  hPutChar h (chr 0)
  hPutStr h s
  hPutChar h (chr 255)
