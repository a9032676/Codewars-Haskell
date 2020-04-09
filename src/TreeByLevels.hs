module TreeByLevels where


data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
} deriving Show

-- BFS timeout problem
treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = maybe [] $ f . pure
    where
        f  nodes     = f' nodes []
        f' []     rs = rs
        f' (x:xs) rs = f' (xs ++ maybe [] pure (left x) ++ maybe [] pure (right x)) $ rs ++ [value x]

--        a
--    b       c
--  d   e   f   g
-- h i j k l m n o
-- [a, b, c, d, h, i]

-- unvis: [b, c]
-- visited: [a]
-- curr: b
