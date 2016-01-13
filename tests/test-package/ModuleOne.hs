module ModuleOne (
	test,
	forkIO,
	f
	) where

import Control.Concurrent (forkIO)

-- | Some test function
test :: IO ()
test = return ()

-- | Some function without type
f x y = x + y
