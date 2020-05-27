{-
  https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
  http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#t:Handle
  https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
  https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
  https://catonmat.net/simple-haskell-tcp-server
  https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch07.html
-}

import           System.Environment
import           Network.Socket
import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.List                     as List
import           Data.Map                      as Map


type RoomId = String
type Room = (Maybe Handle, Maybe Handle)
type RoomList = Map RoomId Room
newtype RoomListState = RoomListState (MVar RoomList)

type Board = [String]
type Move = (Int, Int)
type Token = Char

emptyBoard = [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']] -- 3x3 initial board

main :: IO ()
main = do
  args <- getArgs
  let port = fromIntegral (read (head args) :: Int) -- cast arg to integer
  runServer port 0 -- run server on localhost, on given port

runServer :: PortNumber -> HostAddress -> IO () -- create socket and listen for clients
runServer port addr = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol -- TCP socket, using IPv4
  bind sock (SockAddrInet port addr) -- bind socket to given port and addr
  listen sock 5 -- listen for connections, max 5 queued connections
  putStrLn "Waiting for clients..."
  roomList <- newRoomList
  sockHandler sock roomList
  close sock

sockHandler :: Socket -> RoomListState -> IO () -- accept client
sockHandler sock roomList = do
  (sockh, _) <- accept sock
  playerh    <- socketToHandle sockh ReadWriteMode
  putStrLn "Client connected!"
  forkIO (prepareGame playerh roomList) -- new thread for each client
  sockHandler sock roomList

prepareGame :: Handle -> RoomListState -> IO () -- create / join a room
prepareGame playerh roomList = do
  request <- hGetLine playerh
  let cmd    = head (words request)
  let roomId = last (words request)
  case cmd of
    "create" -> do
      checkRoom <- lookupRoom roomList roomId -- check if room already exists
      if isNothing checkRoom
        then do
          insertRoom roomList roomId (Nothing, Nothing)
          hPutStrLn playerh ("Create room with id " ++ show roomId ++ ".")
          prepareGame playerh roomList
        else do
          hPutStrLn playerh "Room already exists!"
          prepareGame playerh roomList
    "join" -> do
      room <- lookupRoom roomList roomId -- check if room exists
      if isNothing room
        then do
          hPutStrLn playerh "Room not found!"
          prepareGame playerh roomList
        else do
          let checkRoom = setRoomPlayer room playerh -- check if room is full
          if isNothing checkRoom
            then do
              hPutStrLn playerh "Room is full!"
              prepareGame playerh roomList
            else do
              let newRoom = getRoom checkRoom
              hPutStrLn playerh ("Join room with id " ++ show roomId ++ ".")
              updateRoom roomList roomId newRoom
              beginGame roomId newRoom roomList
    "show" -> do
      rooms <- showRoomList roomList
      hPutStrLn playerh ("Show rooms: " ++ show rooms)
      prepareGame playerh roomList
    "help" -> do
      hPutStrLn playerh "Commands: create <room-id>, join <room-id>, show"
      prepareGame playerh roomList
    "exit" -> return ()
    _      -> do
      hPutStrLn playerh "Unknown command!"
      prepareGame playerh roomList

beginGame :: RoomId -> Room -> RoomListState -> IO ()
beginGame roomId (Just player1h, Nothing) roomList =
  hPutStrLn player1h "Waiting for opponent..." -- wait for second player to connect
beginGame roomId (Just player1h, Just player2h) roomList = do
  -- notify players (welcome message)
  hPrint player1h 1
  hPrint player2h 2
  -- print initial (empty) board
  hPutStrLn player1h ("You start!~" ++ printBoard emptyBoard)
  hPutStrLn player2h (printBoard emptyBoard)
  forkIO
    (commandProcessor roomId
                      (Just player1h, Just player2h)
                      player1h
                      emptyBoard
                      roomList
    ) -- new thread for current room
  return ()

commandProcessor :: RoomId -> Room -> Handle -> Board -> RoomListState -> IO ()
commandProcessor roomId (Just player1h, Just player2h) currPlayerh board roomList
  = do
    let opponenth = getOpponent player1h player2h currPlayerh
    request <- hGetLine currPlayerh -- get message from client (ex: "move 1 1")
    let cmd = words request -- create an array of string (ex: ["move", "1", "1"])
    case head cmd of
      "exit" -> do
        deleteRoom roomList roomId
        hPutStrLn currPlayerh "Disconnected!"
        hPutStrLn opponenth   "Your opponent has left!"
        prepareGame player1h roomList
        prepareGame player2h roomList
      "show" -> do
        hPutStrLn currPlayerh ("Showing board...~" ++ printBoard board)
        commandProcessor roomId
                         (Just player1h, Just player2h)
                         currPlayerh
                         board
                         roomList
      "move" -> do
        let move     = parseMove (tail cmd)
        let token    = if currPlayerh == player1h then 'X' else '0'
        let newBoard = executeMove board move token
        if newBoard /= board
          then if checkWin newBoard
            then do
              hPutStrLn currPlayerh "You won!"
              hPutStrLn opponenth   "You lost!"
              restartGame roomId (Just currPlayerh, Just opponenth) roomList -- winner will be first player
            else if checkDraw newBoard
              then do
                hPutStrLn currPlayerh "Draw!"
                hPutStrLn opponenth   "Draw!"
                restartGame roomId (Just currPlayerh, Just opponenth) roomList
              else do
                hPutStrLn currPlayerh (printBoard newBoard)
                hPutStrLn opponenth ("Opponent moved!~" ++ printBoard newBoard)
                commandProcessor roomId
                                 (Just player1h, Just player2h)
                                 opponenth
                                 newBoard
                                 roomList
          else do
            hPutStrLn currPlayerh "Impossible move!"
            commandProcessor roomId
                             (Just player1h, Just player2h)
                             currPlayerh
                             board
                             roomList
      _ -> do
        hPutStrLn currPlayerh "Unknown command!"
        commandProcessor roomId
                         (Just player1h, Just player2h)
                         currPlayerh
                         board
                         roomList

restartGame :: RoomId -> Room -> RoomListState -> IO ()
restartGame roomId (Just player1h, Just player2h) roomList = do
  hPutStrLn player1h "Play again?"
  hPutStrLn player2h "Play again?"
  response1 <- hGetLine player1h
  response2 <- hGetLine player2h
  if response1 == "yes" && response2 == "yes"
    then do
      hPutStrLn player1h "Restarting..."
      hPutStrLn player2h "Restarting..."
      beginGame roomId (Just player1h, Just player2h) roomList
    else do
      hPutStrLn player1h "Players not connected!"
      hPutStrLn player2h "Players not connected!"
      deleteRoom roomList roomId
      forkIO (prepareGame player1h roomList)
      forkIO (prepareGame player2h roomList)
      return ()

getOpponent :: Handle -> Handle -> Handle -> Handle -- get opponent of current player
getOpponent player1h player2h currPlayerh | currPlayerh == player1h = player2h
                                          | otherwise               = player1h

newRoomList :: IO RoomListState -- create an empty map (roomId -> room)
newRoomList = do
  m <- newMVar Map.empty
  return (RoomListState m)

insertRoom :: RoomListState -> RoomId -> Room -> IO () -- create a new game room
insertRoom (RoomListState m) roomId room = do
  roomList <- takeMVar m
  putMVar m (Map.insert roomId room roomList)

lookupRoom :: RoomListState -> RoomId -> IO (Maybe Room) -- search for a room by id
lookupRoom (RoomListState m) roomId = do
  roomList <- takeMVar m
  putMVar m roomList
  return (Map.lookup roomId roomList)

updateRoom :: RoomListState -> RoomId -> Room -> IO () -- modify a room's components (players)
updateRoom (RoomListState m) roomId newRoom = do
  roomList <- takeMVar m
  putMVar m (Map.insert roomId newRoom roomList)

deleteRoom :: RoomListState -> RoomId -> IO () -- remove a room from list
deleteRoom (RoomListState m) roomId = do
  roomList <- takeMVar m
  putMVar m (Map.delete roomId roomList)

showRoomList :: RoomListState -> IO [RoomId] -- print available rooms
showRoomList (RoomListState m) = do
  roomList <- takeMVar m
  putMVar m roomList
  return (Map.keys roomList)

setRoomPlayer :: Maybe Room -> Handle -> Maybe Room -- add player into room
setRoomPlayer (Just (Nothing, Nothing)) playerh = Just (Just playerh, Nothing)
setRoomPlayer (Just (Just handle, Nothing)) playerh =
  Just (Just handle, Just playerh)
setRoomPlayer (Just (Just handle1, Just handle2)) playerh = Nothing

getRoom :: Maybe Room -> Room
getRoom (Just room) = room

-------------- game logic --------------

parseMove :: [String] -> Move -- get a valid move (ex: ["0", "0"] -> (0,0))
parseMove movesList
  | length movesList == 2 && isDigit x && isDigit y
  = (digitToInt x, digitToInt y)
  | otherwise
  = (-1, -1)
 where
  x = head (head movesList)
  y = head (last movesList)

executeMove :: Board -> Move -> Token -> Board -- get new board after executing a move (if valid)
executeMove board move token | not (checkMove board move) = board
                             | otherwise = insertToken board move token

insertToken :: Board -> Move -> Token -> Board
insertToken board move token = case x of
  0 ->
    [insertTokenIntoRow (head board) y token] ++ [board !! 1] ++ [board !! 2]
  1 ->
    [head board] ++ [insertTokenIntoRow (board !! 1) y token] ++ [board !! 2]
  2 ->
    [head board] ++ [board !! 1] ++ [insertTokenIntoRow (board !! 2) y token]
 where
  x = fst move
  y = snd move

insertTokenIntoRow :: String -> Int -> Token -> String
insertTokenIntoRow row index token =
  List.take index row ++ [token] ++ List.drop (index + 1) row

checkMove :: Board -> Move -> Bool -- check if given move is valid
checkMove board move
  | x < 0 || x > 2 || y < 0 || y > 2 || (getCellContent board move /= ' ')
  = False
  | otherwise
  = True
 where
  x = fst move
  y = snd move

getCellContent :: Board -> Move -> Char
getCellContent board move = board !! fst move !! snd move

checkDraw :: Board -> Bool -- check if the board is full (draw)
checkDraw board = not (any (elem ' ') board)

checkWin :: Board -> Bool -- check if there is a winner (3 tokens in a row)
checkWin board =
  checkHorizontal board || checkVertical board || checkDiagonal board

checkHorizontal :: Board -> Bool
checkHorizontal = List.foldr ((||) . sameToken) False

checkVertical :: Board -> Bool
checkVertical board =
  sameToken ([head (head board)] ++ [head (board !! 1)] ++ [head (board !! 2)])
    || sameToken ([head board !! 1] ++ [board !! 1 !! 1] ++ [board !! 2 !! 1])
    || sameToken ([head board !! 2] ++ [board !! 1 !! 2] ++ [board !! 2 !! 2])

checkDiagonal :: Board -> Bool
checkDiagonal board =
  sameToken ([head (head board)] ++ [board !! 1 !! 1] ++ [board !! 2 !! 2])
    || sameToken ([head board !! 2] ++ [board !! 1 !! 1] ++ [head (board !! 2)])

sameToken :: String -> Bool -- check if a row/column/diagonal has the same token
sameToken (x : xs) = all (== x) xs && notElem ' ' xs

printBoard :: Board -> String -- get string representation of the board
printBoard []  = ""
printBoard [x] = intercalate " | " (List.map (: []) x)
printBoard (x : xs) =
  intercalate " | " (List.map (: []) x)
    ++ "~"
    ++ "--+---+--"
    ++ "~"
    ++ printBoard xs
