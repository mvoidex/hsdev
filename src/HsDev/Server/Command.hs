module HsDev.Server.Command (
	) where

import HsDev.Server.Message

data CommandOptions = CommandOptions {
	commandDatabase :: DB.Async Database,
	commandWriteCache :: Database -> IO ()
	commandReadCache :: (FilePath -> ErrorT String IO Structured) -> IO (Maybe Database),
	commandRoot :: FilePath,
#if mingw32_HOST_OS
	commandMmapPool :: Maybe Pool,
#endif
	commandNotify :: Notification -> IO (),
	commandLink :: IO (),
	commandHold :: IO (),
	commandExit :: IO () }

type Command = CommandOptions -> Request -> IO Result
