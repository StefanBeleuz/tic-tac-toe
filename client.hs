import           Network.Socket
import           System.Environment
import           System.IO
import           System.Timeout
import           Data.List
import           Data.List.Extra
import           Control.Monad

main :: IO ()
main = do
  [host, port] <- getArgs
  runClient host (fromIntegral (read port :: Int))

runClient :: HostName -> PortNumber -> IO () -- create socket and connect to server
runClient host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just (show port)) -- host:port
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol -- TCP socket
  connect sock (addrAddress serverAddr) -- connect to server's socket
  handle <- socketToHandle sock ReadWriteMode
  prepareGame handle
  close sock

prepareGame :: Handle -> IO () -- create / join a game
prepareGame handle = do
  putStrLn "Enter request..."
  request <- getLine
  hPutStrLn handle request
  if request == "exit"
    then return ()
    else do
      response <- hGetLine handle
      putStrLn response
      if "Join" `isPrefixOf` response
        then beginGame handle
        else prepareGame handle

beginGame :: Handle -> IO ()
beginGame handle = do
  response <- hGetLine handle -- get player id
  if "Waiting" `isPrefixOf` response
    then do
      putStrLn response
      beginGame handle
    else do
      let player = read response :: Int
      putStrLn ("Welcome you are player " ++ show player ++ "!")
      play handle player

play :: Handle -> Int -> IO ()
play handle player = do
  response <- hGetLine handle
  putStrLn (formatResponse response)
  if "Your opponent has left"
     `isPrefixOf` response
     ||           "Disconnected"
     `isPrefixOf` response
  then
    prepareGame handle
  else
    if "You won"
       `isPrefixOf` response
       ||           "You lost"
       `isPrefixOf` response
       ||           "Draw"
       `isPrefixOf` response
    then
      do
        response <- hGetLine handle
        putStrLn (formatResponse response)
        checkRequest <- timeout 5000000 getLine -- 10s wait time to answer
        let request = getString checkRequest
        hPutStrLn handle request
        response <- hGetLine handle
        putStrLn (formatResponse response)
        if response == "Restarting..."
          then beginGame handle
          else prepareGame handle
    else
      if "Opponent"
           `isPrefixOf` response
           ||           "Unknown command"
           `isPrefixOf` response
           ||           "Impossible move"
           `isPrefixOf` response
           ||           "Showing board"
           `isPrefixOf` response
           ||           "You start"
           `isPrefixOf` response
        then do
          putStrLn "Now is your turn..."
          request <- getLine
          hPutStrLn handle request
          play handle player
        else play handle player

getString :: Maybe String -> String
getString Nothing        = "no"
getString (Just request) = request

formatResponse :: String -> String
formatResponse = replace "~" "\n"
