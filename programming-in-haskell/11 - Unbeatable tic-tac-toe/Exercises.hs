import TicTacToe as T

-- 1
countNodes :: T.Tree T.Grid -> Int
countNodes (T.Node _ []) = 1
countNodes (T.Node _ ts) = 1 + sum [ countNodes t | t <- ts ]

tree :: T.Tree T.Grid
tree = T.gametree empty O

countTrees :: Int
countTrees = countNodes $ T.prune T.depth tree

maxDepth :: T.Tree T.Grid -> Int
maxDepth (T.Node _ []) = 0
maxDepth (T.Node _ ts) = maximum [ 1 + maxDepth t | t <- ts ]

depth :: Int
depth = maxDepth tree

-- 2
-- Looks at TicTacToe.hs file for solution

-- 3
