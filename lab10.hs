data MatchResult = Win | Loss | Draw -- the result of a match could be a Win, Loss or Draw
data Player = Player {
    name :: String,
    elo :: Float,
    matchHistory :: [MatchResult]
} deriving Show -- we care about 3 things: the player's name, elo (floating point which measures the player's strength), and a list of results of past games (call it matchHistory)
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show -- A binary tree we will use to represent the elimination stage of the tournament

instance Show MatchResult where
    show Win = "Win"
    show Loss = "Loss"
    show Draw = "Draw"
 
instance Show Player where
    show (Player name elo matchHistory) = name ++ " " ++ show elo ++ " " ++ show matchHistory

addResult :: MatchResult -> Player -> Player
addResult = \result (Player name elo matchHistory) -> Player name elo (result : matchHistory)

points :: MatchResult -> Float
points = \result -> case result of
    Win -> 1.0
    Loss -> 0.0
    Draw -> 0.5

score :: Player -> Float
score = \player -> foldr (+) 0.0 (map points (matchHistory player))

instance Eq Player where
p1 == p2 = score p1 == score p2
 
instance Ord Player where
p1 <= p2 = score p1 <= score p2
  -- Haskell can infer implementations for all other operations if the implementation for (<=) is given. 

-- return the players, updated (used for group stages) 
playGame :: Player -> Player -> (Player, Player)
playGame p1 p2 
    | elo p1 > elo p2 = (addResult Win p1, addResult Loss p2)
    | elo p1 < elo p2 = (addResult Loss p1, addResult Win p2)
    | otherwise = (addResult Draw p1, addResult Draw p2)
-- ex : playGame (Player "A" 1 []) (Player "B" 2 []) == (Player "A" 1 [Loss], Player "B" 2 [Win])
 
 
-- return the winner, and the players, all updated (used for elimination stage). In case of a draw, return any of the players instead of the winner. 
playGameWithWinner :: Player -> Player -> (Player, (Player, Player))
playGameWithWinner p1 p2
    | elo p1 >= elo p2 = (p1, playGame p1 p2)
    | otherwise = (p2, playGame p1 p2)
-- ex : playGameWithWinner (Player "A" 1 []) (Player "B" 2 []) == ((Player "A" 1 [Loss], Player "B" 2 [Win]))

playAll :: Player -> [Player] -> (Player, [Player])
playAll player [] = (player, [])
playAll player (x:xs) = let (winner, (p1, p2)) = playGameWithWinner player x in
    let (winner', rest) = playAll winner xs in
    (winner', p1 : p2 : rest)

{-
Simulate a single group. Each player will play against all other players exactly once.
Return a list of all players, updated to reflect the results of the matches. Hint: use playAll
-}
playGroup :: [Player] -> [Player]
playGroup players 
 
{-
Select the best 'm' players from the given group. (Assume the players already have their match history updated).
Return 2 lists: one containing the players which were selected and the other containing all other players.
 
hint: sort
-}
selectPlayers :: [Player] -> Int -> ([Player], [Player])
selectPlayers players m = ???
 
{-
Given a list of groups, simulate their matches and select the best 'm' players from each, and return 2 lists: the selected players and the rest.
-}
playGroups :: [[Player]] -> Int -> ([Player], [Player])
playGroups groups m = ???

playElimination :: [Player] -> Tree Player
playElimination = ???

runTournament :: [Player] -> Int -> Int -> [Player]
runTournament players n m = ???