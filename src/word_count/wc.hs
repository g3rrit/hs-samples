import qualified Data.Map as M
import Data.Sort
import System.Environment
import Control.Monad

map_reduce :: (Ord ki, Ord ko)
  => ((ki, vi) -> [(ko, vm)]) -- distribute
  -> (ko -> [vm] -> vo)       -- collect
  -> M.Map ki vi              -- input
  -> M.Map ko vo              -- output
map_reduce d c i =
  M.mapWithKey c
  $ M.fromListWith (++)
  $ map (\(ko, vm) -> (ko, [vm]))
  $ concat
  $ map d 
  $ M.toList i

main = do
  files <- getArgs
  texts <- forM files readFile
  let tf  = M.fromList $ zip files texts
  let wcl = map_reduce 
              (\(ki, vi) -> map (\w -> (w, 1)) $ words vi)
              (\ko vms -> sum vms) 
              tf 
  let r = reverse $ sortBy (\(k0, v0) (k1, v1) -> v0 `compare` v1) 
                  $ M.toList wcl
  forM r $ \(k, v) -> putStrLn $ "|" ++ k ++ " > " ++ (show v)
  
