module System.Console.Cmd (
	CmdAction, notMatch, failMatch,
	Cmd(..), cmdAct, cutName, cmd, defCmd,
	CmdHelp(..), withHelp, printWith,
	run, runArgs, runOn,

	module System.Console.Args
	) where

import Control.Arrow
import Control.Monad (join, (>=>))
import Data.List (stripPrefix, unfoldr, isPrefixOf)
import Control.Monad.Error
import Data.Map (Map)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, maybeToList, isJust)
import qualified Data.Map as M

import System.Console.Args
import Text.Format

type CmdAction a = ErrorT String Maybe a

-- | Arguments doesn't match command
notMatch :: CmdAction a
notMatch = lift Nothing

-- | Invalid command arguments
failMatch :: String -> CmdAction a
failMatch = throwError

data Cmd a = Cmd {
	cmdName :: String,
	cmdArgs :: [String],
	cmdOpts :: [Opt],
	cmdDesc :: Maybe String,
	cmdRun :: Args -> CmdAction a }

instance Functor Cmd where
	fmap f cmd' = cmd' {
		cmdRun = fmap f . cmdRun cmd' }

-- | Make CmdAction function
cmdAct :: (Args -> a) -> Args -> CmdAction a
cmdAct f = return . f

-- | Cut name of command from arguments and checks if it matches
--
-- > cutName >=> cmdAct act
cutName :: String -> Args -> CmdAction Args
cutName name args@(Args (cmd:as) os)
	| cmd == name = return args
	| otherwise = notMatch
cutName _ (Args [] _) = notMatch

verifyOpts :: [Opt] -> Args -> CmdAction Args
verifyOpts os = ErrorT . Just . verify os

cmd :: String -> [String] -> [Opt] -> String -> (Args -> a) -> Cmd a
cmd "" as os desc act = defCmd as os desc act
cmd name as os desc act = Cmd {
	cmdName = name,
	cmdArgs = as,
	cmdOpts = os,
	cmdDesc = if null desc then Nothing else Just desc,
	cmdRun = cutName name >=> verifyOpts os >=> cmdAct act }

-- | Unnamed command
defCmd :: [String] -> [Opt] -> String -> (Args -> a) -> Cmd a
defCmd as os desc act = Cmd {
	cmdName = "",
	cmdArgs = as,
	cmdOpts = os,
	cmdDesc = if null desc then Nothing else Just desc,
	cmdRun = verifyOpts os >=> cmdAct act }

data CmdHelp =
	HelpUsage [String] |
	HelpCommands [(String, [String])]
		deriving (Eq, Ord, Read, Show)

withHelp :: String -> (Either String CmdHelp -> a) -> [Cmd a] -> [Cmd a]
withHelp tool toCmd cmds = cmds' where
	cmds' = helpcmd : cmds
	helpcmd = fmap toCmd $ Cmd
		"help"
		["command"]
		[flag "help" `short` ['?'] `desc` "show help (when using form 'command -?')"]
		(Just ("help command, can be called in form '$ [command] -?'" ~~ tool))
		(cutHelp >=> cmdAct onHelp)
	cutHelp :: Args -> CmdAction Args
	cutHelp a
		| flagSet "help" (namedArgs a) = return $ a {
			namedArgs = Opts $ M.delete "help" $ getOpts $ namedArgs a }
		| listToMaybe (posArgs a) == Just "help" = return $ a {
			posArgs = tail (posArgs a) }
		| otherwise = notMatch
	onHelp (Args [] _) = Right $ HelpUsage [tool ++ " " ++ brief c | c <- cmds']
	onHelp (Args cmdnames _) = liftM (HelpCommands . concat) $ mapM getHelp cmdnames
	getHelp cmdname = case filter ((cmdname ==) . cmdName) cmds' of
		[] -> Left $ "Unknown command: " ++ cmdname
		helps -> Right $ map (cmdName &&& (addHeader . indented)) helps
	addHeader [] = []
	addHeader (h:hs) = (tool ++ " " ++ h) : hs

printWith :: (String -> a) -> (Either String CmdHelp -> a)
printWith fn = fn . either id (unlines . print') where
	print' :: CmdHelp -> [String]
	print' (HelpUsage u) = map ('\t':) u
	print' (HelpCommands cs) = map ('\t':) $ concatMap snd cs

instance Help (Cmd a) where
	brief c = unwords $ filter (not . null) $ [cmdName c, unwords (map angled (cmdArgs c)), brief (cmdOpts c)] ++ maybeToList desc' where
		angled s = "<" ++ s ++ ">"
		desc' = fmap ("-- " ++) $ cmdDesc c
	help = help . cmdOpts

-- | Run commands
run :: [Cmd a] -> a -> (String -> a) -> [String] -> a
run cmds onDef onError = runOn cmds onDef onError (tryParse . cmdOpts)

-- | Run commands with parsed args
runArgs :: [Cmd a] -> a -> (String -> a) -> Args -> a
runArgs cmds onDef onError = runOn cmds onDef onError (const id)

-- | Run commands with 
runOn :: [Cmd a] -> a -> (String -> a) -> (Cmd a -> c -> Args) -> c -> a
runOn cmds onDef onError f as = maybe onDef (either onError id) found where
	found = listToMaybe $ mapMaybe (runErrorT . (`act` as)) cmds
	act c = cmdRun c . f c
