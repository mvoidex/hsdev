module ModuleTwo (
	f,
	twice
	) where

import ModuleOne (f)

-- | Apply function twice
twice :: (a -> a) -> a -> a
twice f = f . f
