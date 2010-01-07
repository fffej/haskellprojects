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
serverHandshake = 
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
    \Upgrade: WebSocket\r\n\
    \Connection: Upgrade\r\n\
    \WebSocket-Origin: http://localhost\r\n\
    \WebSocket-Location: ws://localhost:9876/\r\n\
    \WebSocket-Protocol: sample\r\n\r\n\0"

acceptLoop socket = forever $ do
                      (h,_,_) <- accept socket
                      hPutStr h serverHandshake
                      hSetBuffering h NoBuffering
                      forkIO (listenLoop h)  
    where
      forever a = do a; forever a

main = withSocketsDo $ do
         socket <- listenOn (PortNumber 9876)
         acceptLoop socket
         sClose socket
         return ()

listenLoop :: Handle  -> IO ()
listenLoop h = do
  sendFrame h "hi, remember you can stop this at anytime by pressing quit!"
  msg <- readFrame h
  putStrLn msg
  when (msg /= "quit") (listenLoop h)

sendFrame :: Handle -> String -> IO ()
sendFrame h s = do
  hPutChar h (chr 0)
  hPutStr h s
  hPutChar h (chr 255)

readFrame :: Handle -> IO String
readFrame h = readUntil h ""
    where
      readUntil h str = do
        new <- hGetChar h
        if (new == chr 0) 
          then readUntil h ""
          else if new == chr 255
            then return str
            else readUntil h (str ++ [new])



