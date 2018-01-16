{-# LANGUAGE CPP, PatternSynonyms, ViewPatterns, OverloadedStrings #-}

module HsDev.Symbols.Name (
	Name, qualName, unqualName, nameModule, nameIdent, pattern Name, fromName_, toName_, toModuleName_, fromModuleName_, fromName, toName,
	) where

import Control.Arrow
import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as T
import Language.Haskell.Exts (QName(..), ModuleName(..), Boxed(..), SpecialCon(..))
import qualified Language.Haskell.Exts as Exts (Name(..))

-- | Qualified name
type Name = QName ()

qualName :: String -> String -> Name
qualName m = Qual () (ModuleName () m) . toName_ . fromString

unqualName :: String -> Name
unqualName = UnQual () . toName_ . fromString

nameModule :: Name -> Maybe Text
nameModule (Qual _ (ModuleName _ m) _) = Just $ fromString m
nameModule _ = Nothing

nameIdent :: Name -> Text
nameIdent (Qual _ _ n) = fromName_ n
nameIdent (UnQual _ n) = fromName_ n
nameIdent s = fromName s

pattern Name :: Maybe Text -> Text -> Name
pattern Name m n <- ((nameModule &&& nameIdent) -> (m, n)) where
	Name Nothing n = UnQual () (Exts.Ident () (T.unpack n))
	Name (Just m) n = Qual () (ModuleName () (T.unpack m)) (Exts.Ident () (T.unpack n))

fromName_ :: Exts.Name () -> Text
fromName_ (Exts.Ident _ s') = fromString s'
fromName_ (Exts.Symbol _ s') = fromString s'

toName_ :: Text -> Exts.Name ()
toName_ txt
	| T.null txt = Exts.Ident () ""
	| isAlpha (T.head txt) && (T.all validChar $ T.tail txt) = Exts.Ident () . T.unpack $ txt
	| otherwise = Exts.Symbol () . T.unpack $ txt
	where
		validChar ch = isAlphaNum ch || ch == '_'

toModuleName_ :: Text -> ModuleName ()
toModuleName_ = ModuleName () . T.unpack

fromModuleName_ :: ModuleName () -> Text
fromModuleName_ (ModuleName () m) = T.pack m

toName :: Text -> Name
toName "()" = Special () (UnitCon ())
toName "[]" = Special () (ListCon ())
toName "->" = Special () (FunCon ())
toName "(:)" = Special () (Cons ())
toName "(# #)" = Special () (UnboxedSingleCon ())
toName tup
	| T.all (== ',') noBraces = Special () (TupleCon () Boxed (succ $ T.length noBraces))
	where
		noBraces = T.dropAround (`elem` ['(', ')']) tup
toName n = case T.split (== '.') n of
	[n'] -> UnQual () (Exts.Ident () $ T.unpack n')
	ns -> Qual () (ModuleName () (T.unpack $ T.intercalate "." $ init ns)) (toName_ $ last ns)

fromName :: Name -> Text
fromName (Qual _ (ModuleName _ m) n) = T.concat [fromString m, ".", fromName_ n]
fromName (UnQual _ n) = fromName_ n
fromName (Special _ c) = case c of
	UnitCon _ -> "()"
	ListCon _ -> "[]"
	FunCon _ -> "->"
	TupleCon _ _ i -> fromString $ "(" ++ replicate (pred i) ',' ++ ")"
	Cons _ -> "(:)"
	UnboxedSingleCon _ -> "(# #)"
#if MIN_VERSION_haskell_src_exts(1,20,0)
	ExprHole _ -> "_"
#endif
