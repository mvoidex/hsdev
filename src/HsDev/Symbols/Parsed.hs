module HsDev.Symbols.Parsed (
	Ann, Parsed,
	names, binders, locals,
	annL, pos, defPos, resolvedName,
	isBinder, isLocal,
	nameInfoL, positionL
	) where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Language.Haskell.Exts
import Language.Haskell.Names

import HsDev.Symbols.Location (Position(..))

-- | Annotation of parsed and resolved nodes
type Ann = Scoped SrcSpanInfo

-- | Parsed and resolved module
type Parsed = Module Ann

-- | Get all names
names :: Data (ast Ann) => Traversal' (ast Ann) (Name Ann)
names = biplate

-- | Get all binders
binders :: Data (ast Ann) => Traversal' (ast Ann) (Name Ann)
binders = names . filtered isBinder

-- | Get all names locally defined
locals :: Data (ast Ann) => Traversal' (ast Ann) (Name Ann)
locals = names . filtered isLocal

-- | Get annotation
annL :: Annotated ast => Lens' (ast a) a
annL = lens ann (\v a' -> amap (const a') v)

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
resolvedName :: Annotated ast => Traversal' (ast Ann) (QName ())
resolvedName = annL . nameInfoL . qL where
	qL :: Traversal' (NameInfo SrcSpanInfo) (QName ())
	qL f (GlobalSymbol s q) = GlobalSymbol s <$> f q
	qL _ i = pure i

-- | Does ast node binds something
isBinder :: Annotated ast => ast Ann -> Bool
isBinder e = b `elem` [TypeBinder, ValueBinder] where
	Scoped b _ = ann e

-- | Does ast node locally defined
isLocal :: Annotated ast => ast Ann -> Bool
isLocal e = case b of
	LocalValue _ -> True
	TypeVar _ -> True
	_ -> False
	where
		Scoped b _ = ann e

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
