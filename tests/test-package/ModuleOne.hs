module ModuleOne (
	test,
	newChan,
	untypedFoo
	) where

import Control.Concurrent.Chan (newChan)

-- | Some test function
test :: IO ()
test = return ()

-- | Some function without type
untypedFoo x y = x + y
