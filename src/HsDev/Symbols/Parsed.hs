{-# LANGUAGE RankNTypes #-}

module HsDev.Symbols.Parsed (
	Ann, Parsed,
	names, binders, locals, globals, references, unresolveds,
	usages,

	annL, file, pos, defPos, resolvedName,
	isBinder, isLocal, isGlobal, isReference, isUnresolved,
	refsTo,
	nameInfoL, positionL, fileL,
	symbolNameL
	) where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Language.Haskell.Exts hiding (Name(..))
import qualified Language.Haskell.Exts as E (Name(..))
import Language.Haskell.Names

import HsDev.Symbols.Name
import HsDev.Symbols.Location (Position(..))

-- | Annotation of parsed and resolved nodes
type Ann = Scoped SrcSpanInfo

-- | Parsed and resolved module
type Parsed = Module Ann

-- | Get all names
names :: Data (ast Ann) => Traversal' (ast Ann) (E.Name Ann)
names = biplate

-- | Get all binders
binders :: Data (ast Ann) => Traversal' (ast Ann) (E.Name Ann)
binders = names . filtered isBinder

-- | Get all names locally defined
locals :: Data (ast Ann) => Traversal' (ast Ann) (E.Name Ann)
locals = names . filtered isLocal

-- | Get all names, references global symbol
globals :: Data (ast Ann) => Traversal' (ast Ann) (E.Name Ann)
globals = names . filtered isGlobal

-- | Get all resolved references
references :: Data (ast Ann) => Traversal' (ast Ann) (E.Name Ann)
references = names . filtered isReference

-- | Get all names with not in scope error
unresolveds :: Data (ast Ann) => Traversal' (ast Ann) (E.Name Ann)
unresolveds = names . filtered isUnresolved

-- | Get all usages of symbol
usages :: Data (ast Ann) => Name -> Traversal' (ast Ann) (E.Name Ann)
usages n = globals . filtered (refsTo n)

-- | Get annotation
annL :: Annotated ast => Lens' (ast a) a
annL = lens ann (\v a' -> amap (const a') v)

-- | Get source file
file :: Annotated ast => Lens' (ast Ann) FilePath
file = annL . fileL

-- | Get source location
pos :: Annotated ast => Lens' (ast Ann) Position
pos = annL . positionL

-- | Definition position, if binder - returns current position
defPos :: Annotated ast => Traversal' (ast Ann) Position
defPos = annL . defLoc' where
	defLoc' :: Traversal' Ann Position
	defLoc' f (Scoped (LocalValue s) i) = Scoped <$> (LocalValue <$> positionL f s) <*> pure i
	defLoc' f (Scoped (TypeVar s) i) = Scoped <$> (TypeVar <$> positionL f s) <*> pure i
	defLoc' f (Scoped ValueBinder i) = Scoped ValueBinder <$> positionL f i
	defLoc' f (Scoped TypeBinder i) = Scoped TypeBinder <$> positionL f i
	defLoc' _ s = pure s

-- | Resolved global name
resolvedName :: Annotated ast => Traversal' (ast Ann) Name
resolvedName = annL . nameInfoL . biplate . symbolNameL

-- | Does ast node binds something
isBinder :: Annotated ast => ast Ann -> Bool
isBinder e = (e ^. annL . nameInfoL) `elem` [TypeBinder, ValueBinder]

-- | Does ast node locally defined
isLocal :: Annotated ast => ast Ann -> Bool
isLocal e = case e ^. annL . nameInfoL of
	LocalValue _ -> True
	TypeVar _ -> True
	_ -> False

-- | Does ast node reference something
isGlobal :: Annotated ast => ast Ann -> Bool
isGlobal e = case e ^. annL . nameInfoL of
	GlobalSymbol _ _ -> True
	_ -> False

-- | Does ast node reference something
isReference :: Annotated ast => ast Ann -> Bool
isReference e = isLocal e || isGlobal e

-- | Is ast node not resolved
isUnresolved :: Annotated ast => ast Ann -> Bool
isUnresolved e = case e ^. annL . nameInfoL of
	ScopeError _ -> True
	_ -> False

-- | Node references to specified symbol
refsTo :: Annotated ast => Name -> ast Ann -> Bool
refsTo n a = Just n == a ^? resolvedName

nameInfoL :: Lens' (Scoped a) (NameInfo a)
nameInfoL = lens g' s' where
	g' (Scoped i _) = i
	s' (Scoped _ s) i' = Scoped i' s

positionL :: (SrcInfo isrc, Data isrc) => Lens' isrc Position
positionL = lens g' s' where
	g' i = Position l c where
		SrcLoc _ l c = getPointLoc i
	s' i (Position l c) = over biplate upd i where
		Position sl sc = g' i -- Old location
		-- main line: set new line and move column
		-- other lines: just move line, because altering first line's column doesn't affect other lines
		upd :: SrcLoc -> SrcLoc
		upd (SrcLoc f' l' c')
			| l' == sl = SrcLoc f' l (c' - sc + c)
			| otherwise = SrcLoc f' (l' - sl + l) c'

fileL :: (SrcInfo isrc, Data isrc) => Lens' isrc FilePath
fileL = lens g' s' where
	g' = fileName
	s' i f = set biplate f i

-- | Get 'Symbol' as 'Name'
symbolNameL :: Lens' Symbol Name
symbolNameL = lens g' s' where
	g' sym' = Qual () (symbolModule sym') (symbolName sym')
	s' sym' (Qual _ m n) = sym' { symbolModule = m, symbolName = n }
	s' sym' (UnQual _ n) = sym' { symbolName = n }
	s' sym' _ = sym'
