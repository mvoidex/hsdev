module HsDev.Server.Message.Lisp (
	Msg,
	isLisp, msg,
	jsonMsg, lispMsg,

	decodeMsg, encodeMsg
	) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.ByteString.Lazy.Char8 (ByteString)

import Data.Lisp

type Msg a = (Bool, a)

isLisp :: Lens' (Msg a) Bool
isLisp = _1

msg :: Lens (Msg a) (Msg b) a b
msg = _2

jsonMsg :: a -> Msg a
jsonMsg = (,) False

lispMsg :: a -> Msg a
lispMsg = (,) True

-- | Decode lisp or json
decodeMsg :: FromJSON a => ByteString -> Either (Msg String) (Msg a)
decodeMsg bstr = over _Left decodeType' decodeMsg' where
	decodeType'
		| isLisp' = lispMsg
		| otherwise = jsonMsg
	decodeMsg' = (lispMsg <$> decodeLisp bstr) <|> (jsonMsg <$> eitherDecode bstr)
	isLisp' = fromMaybe False $ mplus (try' eitherDecode False) (try' decodeLisp True)
	try' :: (ByteString -> Either String Value) -> Bool -> Maybe Bool
	try' f l = either (const Nothing) (const $ Just l) $ f bstr

-- | Encode lisp or json
encodeMsg :: ToJSON a => Msg a -> ByteString
encodeMsg m
	| view isLisp m = encodeLisp $ view msg m
	| otherwise = encode $ view msg m
