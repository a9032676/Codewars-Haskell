module CanYouGetTheLoop where

import Data.List

data Node a = Node a (Node a)

instance Eq a => Eq (Node a) where
    Node i _ == Node j _ = i == j

next :: Node a -> Node a
next (Node _ n) = n

loopSize :: Eq a => Node a -> Int
loopSize = f []
    where
        f stk node = maybe (1 + f (stk ++ [node]) (next node)) (\i -> -i) $ node `elemIndex` stk


-- node1 = Node 1 node2
-- node2 = Node 2 node3
-- node3 = Node 3 node4
-- node4 = Node 4 node5
-- node5 = Node 5 node6
-- node6 = Node 6 node7
-- node7 = Node 7 node8
-- node8 = Node 8 node9
-- node9 = Node 9 node10
-- node10 = Node 10 node11
-- node11 = Node 11 node12
-- node12 = Node 12 node13
-- node13 = Node 13 node14
-- node14 = Node 14 node4