{-# LANGUAGE TemplateHaskell #-}

module HsDev.Inspect.Types (
	Preloaded(..), preloadedId, preloadedMode, preloadedModule, asModule, toImport, preloaded,
	InspectedPreloaded,
	Environment, FixitiesTable,
	Resolved(..), resolvedModule, resolvedSource, resolvedDefs, resolvedImports, resolvedExports, resolvedScope, resolvedFixities,
	InspectedResolved,

	resolvedEnv, resolvedFixitiesTable,

	dropScope, noScope, withNoScope
	) where

import Control.DeepSeq
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.String
import Data.Text (Text)
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names as N
import qualified Language.Haskell.Names.GlobalSymbolTable as N

import HsDev.Symbols.Types
import HsDev.Symbols.Parsed (Parsed, pos)

-- | Preloaded module with contents and extensions
data Preloaded = Preloaded {
	_preloadedId :: ModuleId,
	_preloadedMode :: H.ParseMode,
	_preloadedModule :: H.Module H.SrcSpanInfo,
	-- ^ Loaded module head without declarations
	_preloaded :: Text }

instance NFData Preloaded where
	rnf (Preloaded mid _ _ cts) = rnf mid `seq` rnf cts

asModule :: Lens' Preloaded Module
asModule = lens g' s' where
	g' p = Module {
		_moduleId = _preloadedId p,
		_moduleDocs = Nothing,
		_moduleImports = map toImport idecls,
		_moduleExports = mempty,
		_moduleFixities = mempty,
		_moduleScope = mempty,
		_moduleSource = Just $ fmap (N.Scoped N.None) $ _preloadedModule p }
		where
			H.Module _ _ _ idecls _ = _preloadedModule p
	s' p m = p {
		_preloadedId = _moduleId m,
		_preloadedModule = maybe (_preloadedModule p) dropScope (_moduleSource m) }

toImport :: H.ImportDecl H.SrcSpanInfo -> Import
toImport idecl@(H.ImportDecl _ mname qual _ _ _ alias _) = Import (idecl ^. pos) (fromString $ getModuleName mname) qual (fmap (fromString . getModuleName) alias) where
	getModuleName (H.ModuleName _ s) = s

type InspectedPreloaded = Inspected ModuleLocation ModuleTag Preloaded

-- | Symbols environment, used to resolve names in source
type Environment = N.Environment

-- | Fixities environment, needed to parse source
type FixitiesTable = Map Name H.Fixity

-- | Resolved module
data Resolved = Resolved {
	_resolvedModule :: H.ModuleName (),
	_resolvedSource :: Parsed,
	_resolvedDefs :: [Symbol],
	_resolvedImports :: [Import],
	_resolvedExports :: [N.Symbol],
	_resolvedScope :: N.Table,
	_resolvedFixities :: [H.Fixity] }

instance NFData Resolved where
	rnf (Resolved _ _ defs imps _ _ _) = rnf defs `seq` rnf imps

-- | Like @InspectedModule@, but for @Resolved@
type InspectedResolved = Inspected ModuleLocation ModuleTag Resolved

-- | Get environment for resolved module
resolvedEnv :: Resolved -> Environment
resolvedEnv r = M.singleton (_resolvedModule r) (_resolvedExports r)

-- | Get fixities table from resolved module
resolvedFixitiesTable :: Resolved -> FixitiesTable
resolvedFixitiesTable r = mconcat [M.singleton n f | f@(H.Fixity _ _ n) <- _resolvedFixities r]

-- | Drop extra info
dropScope :: Functor f => f (N.Scoped l) -> f l
dropScope = fmap (\(N.Scoped _ a) -> a)

-- | Empty scope info
noScope :: l -> N.Scoped l
noScope = N.Scoped N.None

-- | Set empty scope
withNoScope :: Functor f => f l -> f (N.Scoped l)
withNoScope = fmap noScope

makeLenses ''Preloaded
makeLenses ''Resolved
