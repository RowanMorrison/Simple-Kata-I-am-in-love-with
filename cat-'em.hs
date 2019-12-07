module TreeByLevels where

import TreeByLevels.TreeNode
import Control.Applicative((<**>))
import Data.Maybe(catMaybes)

treeByLevels :: Show a => Maybe (TreeNode a) -> [a]
treeByLevels Nothing  = []
treeByLevels (Just t) = byLev [t]
  where
    byLev [] = []
    byLev ts = map value ts ++ byLev (catMaybes $ ts <**> [left, right])