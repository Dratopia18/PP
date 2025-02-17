nats :: [Integer]
nats = [0..]

odds :: [Integer]
odds = [x | x <- nats, odd x]

square :: [Integer]
square = [x^2 | x <- nats]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

data BTree = Node Int BTree BTree | Nil deriving Show
tree :: BTree
tree = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)

data StreamBTree = StreamNode Int StreamBTree StreamBTree
 
sbtree = StreamNode 1 sbtree sbtree

sliceTree :: Int -> StreamBTree -> BTree
sliceTree 0 _ = Nil
sliceTree k (StreamNode v l r) = Node v (sliceTree (k-1) l) (sliceTree (k-1) r)

repeatTree :: Int -> StreamBTree
repeatTree x = StreamNode x (repeatTree x) (repeatTree x)

generateTree :: Int -> (Int -> Int) -> (Int -> Int) -> StreamBTree
generateTree x leftF rightF = StreamNode x (generateTree (leftF x) leftF rightF) (generateTree (rightF x) leftF rightF)

build :: (Double -> Double) -> Double -> [Double]
build g x = x : build g (g x)

alternatingBinary :: [Double]
alternatingBinary = build (\x -> 1 - x) 0

alternatingCons :: [Double]
alternatingCons = build (\x -> if x > 0 then -(x+1) else -(x-1)) 0

alternatingPowers :: [Double]
alternatingPowers = build (\x -> if x > 0 then -2*x else -0.5*x) 1