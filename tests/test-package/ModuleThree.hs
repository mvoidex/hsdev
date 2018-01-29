module ModuleThree (
	smth,

	module ModuleOne,
	module ModuleTwo
	) where

import Control.Concurrent.Chan

import ModuleOne (test)
import ModuleTwo hiding (twice)

smth :: [Int] -> [Int]
smth [] = [1]
smth (x:xs) = map (+x) xs
