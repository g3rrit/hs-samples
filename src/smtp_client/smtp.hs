import System.IO
import Network.Simple.TCP
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class

data Mail = Mail {
  host    :: String,
  port    :: String,
  from    :: String,
  to      :: String,
  subject :: String,
  txt     :: [String]}
  deriving (Show)

main = do
  putStr "SMTP Server: "
  host <- getLine
  putStr "Port: "
  port <- getLine
  putStr "From: "
  from <- getLine
  putStr "To: "
  to <- getLine
  putStr "Subject: "
  subject <- getLine
  putStrLn "Data: "
  txt <- get_lines
  let mail = Mail host port from to subject txt
  print mail
  connect host port (send_mail mail)

{--
send_mail :: Mail -> IO ()
send_mail m = connect (host m) (port m) (\(s, a) -> do
                                            send_str s "HELO user"
                                            recv_str s
                                            return ())
--}
send_mail :: Mail -> (Socket, SockAddr) -> IO ()
send_mail m (s, a) = do
  send_str s "HELO user"
  recv_str s
  send_str s ("MAIL FROM:" ++ (from m))
  recv_str s
  send_str s ("RCPT TO:" ++ (to m))
  recv_str s
  send_txt s ((("From: " ++ (from m))
              :(("To: " ++ (to m))
              :(("Subject: " ++ (subject m))
              :((" ")
              :(txt m))))))
  send_str s "."
  recv_str s
  send_str s "QUIT"
  recv_str s

-- UTIL

recv_str :: Socket -> IO ()
recv_str s = do
  res <- recv s 1000
  maybe_print res

maybe_print :: Maybe B.ByteString -> IO ()
maybe_print b = case b of
                  Just n -> print n
                  Nothing -> putStrLn "<no response>"

get_lines :: IO [String]
get_lines = get_lines' 1

get_lines' :: Int -> IO [String]
get_lines' i = do x <- getLine
                  if x ==  ""
                    then do
                      if i == 0
                      then return []
                      else get_lines' 0
                    else do xs <- get_lines' 1
                            return (x:xs)

pack_str :: String -> B.ByteString
pack_str = encodeUtf8 . T.pack

send_str :: MonadIO m => Socket -> String -> m ()
send_str so s = send so $ pack_str s

send_str_ln :: MonadIO m => Socket -> String -> m ()
send_str_ln so s = send so $ pack_str (s ++ "\n")

send_txt :: MonadIO m => Socket -> [String] -> m ()
send_txt so sa
  | (length sa) > 1 = do send_str_ln so (head sa)
                         send_txt so (tail sa)
  | otherwise = send_str_ln so (head sa)
