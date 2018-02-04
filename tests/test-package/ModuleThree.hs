module ModuleThree (
	smth,

	module ModuleOne,
	module T
	) where

import Control.Concurrent.Chan

import ModuleOne (test)
import ModuleTwo as T hiding (twice)

smth :: [Int] -> [Int]
smth [] = [1]
smth (x:xs) = map (+x) xs
