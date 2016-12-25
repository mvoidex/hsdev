module ModuleTwo (
	untypedFoo,
	twice,
	overloadedStrings
	) where

import ModuleOne (untypedFoo)

import Data.String

-- | Apply function twice
twice :: (a -> a) -> a -> a
twice f = f . f

data MyString = MyString String

instance IsString MyString where
	fromString = MyString

overloadedStrings :: MyString
overloadedStrings = "Hello"
