module System.Console.Cmd (
	CmdAction, notMatch, failMatch, runCmd, defaultOpts, validateArgs, alterArgs,
	Cmd(..), cmdAct, cutName, cmda, cmda_, cmd, cmd_, defCmd,
	CmdHelp(..), helpCommand, withHelp, printWith,
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
	cmdDesc :: String,
	cmdGetArgs :: Args -> CmdAction Args,
	-- ^ Get command arguments from source arguments, by default it cuts command name
	cmdAction :: Args -> CmdAction a }

instance Functor Cmd where
	fmap f cmd' = cmd' {
		cmdAction = fmap f . cmdAction cmd' }

-- | Run cmd
runCmd :: Cmd a -> Args -> CmdAction a
runCmd c = cmdGetArgs c >=> cmdAction c

-- | Set default opts
defaultOpts :: Opts String -> Cmd a -> Cmd a
defaultOpts opts = alterArgs (cmdAct $ withOpts $ defOpts opts)

-- | Validate Args in command
validateArgs :: (Args -> CmdAction ()) -> Cmd a -> Cmd a
validateArgs p c = c {
	cmdAction = \a -> p a >> cmdAction c a }

-- | Alter Args in command
alterArgs :: (Args -> CmdAction Args) -> Cmd a -> Cmd a
alterArgs f c = c {
	cmdGetArgs = f >=> cmdGetArgs c }

-- | Make CmdAction function
cmdAct :: (b -> a) -> b -> CmdAction a
cmdAct f = return . f

-- | Cut name of command from arguments and checks if it matches
--
-- > cutName >=> cmdAct act
cutName :: String -> Args -> CmdAction Args
cutName name args@(Args as os) = case stripPrefix (words name) as of
	Just as' -> return (Args as' os)
	Nothing -> notMatch

verifyOpts :: [Opt] -> Args -> CmdAction Args
verifyOpts os = ErrorT . Just . verify os

cmda :: String -> [String] -> [Opt] -> String -> (Args -> CmdAction a) -> Cmd a
cmda name as os desc act = Cmd {
	cmdName = name,
	cmdArgs = as,
	cmdOpts = os,
	cmdDesc = desc,
	cmdGetArgs = cut',
	cmdAction = verifyOpts os >=> act }
	where
		cut'
			| null name = return
			| otherwise = cutName name

cmda_ :: String -> [Opt] -> String -> (Opts String -> CmdAction a) -> Cmd a
cmda_ name os desc act = validateArgs noPos $ cmda name [] os desc (act . namedArgs) where
	noPos (Args [] _) = return ()
	noPos (Args _ _) = failMatch "No positional argument expected"

cmd :: String -> [String] -> [Opt] -> String -> (Args -> a) -> Cmd a
cmd name as os desc act = cmda name as os desc (cmdAct act)

cmd_ :: String -> [Opt] -> String -> (Opts String -> a) -> Cmd a
cmd_ name os desc act = cmda_ name os desc (cmdAct act)

-- | Unnamed command
defCmd :: [String] -> [Opt] -> String -> (Args -> a) -> Cmd a
defCmd as os desc act = cmda "" as os desc (cmdAct act)

data CmdHelp =
	HelpUsage [String] |
	HelpCommands [(String, [String])]
		deriving (Eq, Ord, Read, Show)

-- | Make help command, which will show help on for specified commands
helpCommand :: String -> (Either String CmdHelp -> a) -> [Cmd a] -> Cmd a
helpCommand tool toCmd cmds = helpcmd where
	helpcmd = fmap toCmd $ alterArgs (cmdAct checkHelp) $ cmd
		"help"
		["command"]
		[flag "help" `short` ['?'] `desc` "show help (when using form 'command -?' or 'command --help')"]
		("help command, can be called in form '$ [command] -?' or '$ [command] --help" ~~ tool % tool)
		onHelp
	checkHelp :: Args -> Args
	checkHelp a
		| flagSet "help" (namedArgs a) = a {
			posArgs = "help" : posArgs a,
			namedArgs = Opts $ M.delete "help" $ getOpts $ namedArgs a }
		| otherwise = a
	onHelp (Args [] _) = Right $ HelpUsage [tool ++ " " ++ brief c | c <- (helpcmd:cmds)]
	onHelp (Args cmdname _) = case filter ((cmdname ==) . words . cmdName) (helpcmd:cmds) of
		[] -> Left $ "Unknown command: " ++ unwords cmdname
		helps -> Right $ HelpCommands $ map (cmdName &&& (addHeader . indented)) helps
	addHeader [] = []
	addHeader (h:hs) = (tool ++ " " ++ h) : hs

-- | Add help command
withHelp :: String -> (Either String CmdHelp -> a) -> [Cmd a] -> [Cmd a]
withHelp tool toCmd cmds = helpCommand tool toCmd cmds : cmds

printWith :: (String -> a) -> (Either String CmdHelp -> a)
printWith fn = fn . either id (unlines . print') where
	print' :: CmdHelp -> [String]
	print' (HelpUsage u) = map ('\t':) u
	print' (HelpCommands cs) = map ('\t':) $ concatMap snd cs

instance Help (Cmd a) where
	brief c = unwords $ filter (not . null) $ [cmdName c, unwords (map angled (cmdArgs c)), brief (cmdOpts c)] ++ desc' where
		angled s = "<" ++ s ++ ">"
		desc'
			| null (cmdDesc c) = []
			| otherwise = ["-- " ++ cmdDesc c]
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
	act c = runCmd c . f c
