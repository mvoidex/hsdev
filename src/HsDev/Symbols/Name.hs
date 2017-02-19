{-# LANGUAGE PatternSynonyms, ViewPatterns, OverloadedStrings #-}

module HsDev.Symbols.Name (
	Name, nameModule, nameIdent, pattern Name, namePrefix, fromName_, toName_, fromName, toName,
	) where

import Control.Arrow
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text as T
import Language.Haskell.Exts (QName(..), ModuleName(..), Boxed(..), SpecialCon(..))
import qualified Language.Haskell.Exts as Exts (Name(..))

-- | Qualified name
type Name = QName ()

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

namePrefix :: Name -> Name -> Bool
namePrefix p s = nameModule p == nameModule s && nameIdent p `T.isPrefixOf` nameIdent s

fromName_ :: Exts.Name () -> Text
fromName_ (Exts.Ident _ s') = fromString s'
fromName_ (Exts.Symbol _ s') = fromString s'

toName_ :: Text -> Exts.Name ()
toName_ = Exts.Ident () . T.unpack

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
	ns -> Qual () (ModuleName () (T.unpack $ T.intercalate "." $ init ns)) (Exts.Ident () (T.unpack $ last ns))

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

