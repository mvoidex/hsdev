module ModuleOne (
	test,
	forkIO,
	untypedFoo
	) where

import Control.Concurrent (forkIO)

-- | Some test function
test :: IO ()
test = return ()

-- | Some function without type
untypedFoo x y = x + y
