module Main (
	main
	) where

import Control.Monad (liftM, forM)
import Data.Maybe
import HsDev.Tools.Hayoo

import Tool

main :: IO ()
main = toolMain "hshayoo" [
	jsonCmd "" ["query"] options "search in hayoo" hayoo']
	where
		options = [
			req "page" "n" `short` ['p'] `desc` "page number (0 by default)",
			req "pages" "count" `short` ['n'] `desc` "pages count (1 by default)"]

		hayoo' (Args [q] opts) = liftM concat $ forM [page .. page + pred pages] $ \i -> liftM (mapMaybe hayooAsDeclaration . resultResult) $ hayoo q (Just i) where
			page = fromMaybe 0 (narg "page" opts)
			pages = fromMaybe 1 (narg "pages" opts)
		hayoo' _ = toolError "Invalid arguments"
