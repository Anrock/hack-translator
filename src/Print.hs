module Print where

import Common.Types

printSource :: (Show a) => [Source p s a] -> IO ()
printSource ls = sequence_ $ putStrLn <$> (show . unSource <$> ls)
