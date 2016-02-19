module Data.Lisp (
	Lisp(..),
	lisp,
	encodeLisp, decodeLisp
	) where

import Prelude hiding (String, Bool)
import qualified Prelude as P (String, Bool)

import Data.Aeson (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe, parseEither)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isAlpha, isDigit)
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import Data.List (unfoldr)
import Data.Scientific
import Data.String (fromString)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as LT (pack, unpack)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8, decodeUtf8)
import qualified Text.ParserCombinators.ReadP as R
import Text.Read (readMaybe)
import qualified Data.Vector as V

data Lisp =
	Null |
	Bool P.Bool |
	Symbol P.String |
	String P.String |
	Number Scientific |
	List [Lisp]
		deriving (Eq)

readable :: Read a => Int -> R.ReadP a
readable = R.readS_to_P . readsPrec

lisp :: Int -> R.ReadP Lisp
lisp n = R.choice [
	do
		s <- symbol
		return $ case s of
			"null" -> Null
			"true" -> Bool True
			"false" -> Bool False
			_ -> Symbol s,
	fmap String string,
	fmap Number number,
	fmap List list]
	where
		symbol :: R.ReadP P.String
		symbol = concat <$> sequence [
			R.option [] (pure <$> R.char ':'),
			pure <$> R.satisfy isAlpha,
			R.munch (\ch -> isAlpha ch || isDigit ch || ch == '-')]

		string :: R.ReadP P.String
		string = (R.<++ R.pfail) $ do
			('\"':_) <- R.look
			readable n

		number :: R.ReadP Scientific
		number = do
			s <- R.munch1 (\ch -> isDigit ch || ch `elem` ['e', 'E', '.', '+', '-'])
			maybe R.pfail return $ readMaybe s

		list :: R.ReadP [Lisp]
		list = R.between (R.char '(') (R.char ')') $ R.sepBy (lisp n) R.skipSpaces

instance Read Lisp where
	readsPrec = R.readP_to_S . lisp

instance Show Lisp where
	show Null = "null"
	show (Bool b)
		| b = "true"
		| otherwise = "false"
	show (Symbol s) = s
	show (String s) = show s
	show (Number n) = either show show (floatingOrInteger n :: Either Double Integer)
	show (List vs) = "(" ++ unwords (map show vs) ++ ")"

instance ToJSON Lisp where
	toJSON Null = toJSON A.Null
	toJSON (Bool b) = toJSON b
	toJSON (Symbol s) = toJSON s
	toJSON (String s) = toJSON s
	toJSON (Number n) = toJSON n
	toJSON (List vs)
		| null keywords = toJSON $ map toJSON vals
		| null vals = keywordsObject
		| otherwise = toJSON $ map toJSON vals ++ [keywordsObject]
		where
			(vals, keywords) = partitionEithers $ unfoldr cutKeyword vs
			keywordsObject = A.object [fromString (dropColon k) .= v | (k, v) <- keywords]

			dropColon :: P.String -> P.String
			dropColon (':' : s) = s
			dropColon s = s

			cutKeyword :: [Lisp] -> Maybe (Either Lisp (P.String, Lisp), [Lisp])
			cutKeyword [] = Nothing
			cutKeyword (Symbol s : []) = Just (Right (s, Null), [])
			cutKeyword (Symbol s : Symbol h : hs) = Just (Right (s, Null), Symbol h : hs)
			cutKeyword (Symbol s : h : hs) = Just (Right (s, h), hs)
			cutKeyword (h : hs) = Just (Left h, hs)

instance FromJSON Lisp where
	parseJSON A.Null = return Null
	parseJSON (A.Bool b) = return $ Bool b
	parseJSON (A.String s) = return $ String $ T.unpack s
	parseJSON (A.Number n) = return $ Number n
	parseJSON (A.Array vs) = fmap List $ mapM parseJSON $ V.toList vs
	parseJSON (A.Object obj) = fmap (List . concat) $ mapM (\(k, v) -> sequence [pure $ Symbol (':' : T.unpack k), parseJSON v]) $ HM.toList obj

decodeLisp :: FromJSON a => ByteString -> Either P.String a
decodeLisp str = do
	sexp <- maybe (Left "Not a s-exp") Right . readMaybe . LT.unpack . LT.decodeUtf8 $ str
	parseEither parseJSON $ toJSON (sexp :: Lisp)

encodeLisp :: ToJSON a => a -> ByteString
encodeLisp r = LT.encodeUtf8 . LT.pack $ maybe
	"(:error \"can't convert to s-exp\")"
	(show :: Lisp -> P.String)
	(parseMaybe parseJSON (toJSON r))
