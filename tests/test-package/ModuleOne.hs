module ModuleOne (
	test,
	newChan,
	untypedFoo,
	Ctor(..), ctor,
	) where

import Control.Concurrent.Chan (newChan)

-- | Some test function
test :: IO ()
test = return ()

-- | Some function without type
untypedFoo x y = x + y

-- | Same name of type and ctor
data Ctor a = Ctor a

ctor :: Ctor Int
ctor = Ctor 10
