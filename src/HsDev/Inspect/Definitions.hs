{-# LANGUAGE CPP #-}

module HsDev.Inspect.Definitions (
	getSymbols,
	getDecl
	) where

import Control.Lens
import Control.Monad
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Data.Function
import Data.Ord
import Data.String
import Data.Text (Text)
import qualified Language.Haskell.Exts as H

import HsDev.Symbols.Types
import HsDev.Symbols.Parsed
import HsDev.Symbols.Resolve (symbolUniqId)

-- | Get top symbols
getSymbols :: [H.Decl Ann] -> [Symbol]
getSymbols decls =
	map mergeSymbols .
	groupBy ((==) `on` symbolUniqId) .
	sortBy (comparing symbolUniqId) $
	concatMap getDecl decls
	where
		mergeSymbols :: [Symbol] -> Symbol
		mergeSymbols [] = error "impossible"
		mergeSymbols [s] = s
		mergeSymbols ss@(s:_) = Symbol
			(view symbolId s)
			(msum $ map (view symbolDocs) ss)
			(msum $ map (view symbolPosition) ss)
			(foldr1 mergeInfo $ map (view symbolInfo) ss)

		mergeInfo :: SymbolInfo -> SymbolInfo -> SymbolInfo
		mergeInfo (Function lt) (Function rt) = Function $ lt `mplus` rt
		mergeInfo (PatConstructor las lt) (PatConstructor ras rt) = PatConstructor (if null las then ras else las) (lt `mplus` rt)
		mergeInfo (Selector lt lp lc) (Selector rt rp rc)
			| lt == rt && lp == rp = Selector lt lp (nub $ lc ++ rc)
			| otherwise = Selector lt lp lc
		mergeInfo l _ = l

-- | Get symbols from declarations
getDecl :: H.Decl Ann -> [Symbol]
getDecl decl' = case decl' of
	H.TypeDecl _ h _ -> [mkSymbol (tyName h) (Type (tyArgs h) [])]
	H.TypeFamDecl _ h _ _ -> [mkSymbol (tyName h) (TypeFam (tyArgs h) [] Nothing)]
	H.ClosedTypeFamDecl _ h _ _ _ -> [mkSymbol (tyName h) (TypeFam (tyArgs h) [] Nothing)]
	H.DataDecl _ dt mctx h dcons _ -> mkSymbol nm ((getCtor dt) (tyArgs h) (getCtx mctx)) : concatMap (getConDecl nm) dcons where
		nm = tyName h
	H.GDataDecl _ dt mctx h _ gcons _ -> mkSymbol nm ((getCtor dt) (tyArgs h) (getCtx mctx)) : concatMap (getGConDecl nm) gcons where
		nm = tyName h
	H.DataFamDecl _ mctx h _ -> [mkSymbol (tyName h) (DataFam (tyArgs h) (getCtx mctx) Nothing)]
	H.ClassDecl _ mctx h _ clsDecls -> mkSymbol nm (Class (tyArgs h) (getCtx mctx)) : concatMap (getClassDecl nm) (fromMaybe [] clsDecls) where
		nm = tyName h
	H.TypeSig _ ns tsig -> [mkSymbol n (Function (Just $ oneLinePrint tsig)) | n <- ns]
#if MIN_VERSION_haskell_src_exts(1,21,0)
	H.PatSynSig _ ns mas _ _ _ t ->
#else
	H.PatSynSig _ ns mas _ _ t ->
#endif
		[mkSymbol n (PatConstructor (maybe [] (map prp) mas) (Just $ oneLinePrint t)) | n <- ns'] where
#if MIN_VERSION_haskell_src_exts(1,20,0)
			ns' = ns
#else
			ns' = [ns]
#endif
	H.FunBind _ ms -> [mkSymbol (matchName m) (Function Nothing) | m <- ms] where
		matchName (H.Match _ n _ _ _) = n
		matchName (H.InfixMatch _ _ n _ _ _) = n
	H.PatBind _ p _ _ -> [mkSymbol n (Function Nothing) | n <- patNames p] where
		patNames :: H.Pat Ann -> [H.Name Ann]
		patNames = childrenBi
	H.PatSyn _ p _ _ -> case p of
		H.PInfixApp _ _ qn _ -> [mkSymbol (qToName qn) (PatConstructor [] Nothing)]
		H.PApp _ qn _ -> [mkSymbol (qToName qn) (PatConstructor [] Nothing)]
		H.PRec _ qn fs -> mkSymbol (qToName qn) (PatConstructor [] Nothing) :
			[mkSymbol (qToName n) (PatSelector Nothing Nothing (prp $ qToName qn)) | n <- (universeBi fs :: [H.QName Ann])]
		_ -> []
		where
			qToName (H.Qual _ _ n) = n
			qToName (H.UnQual _ n) = n
			qToName _ = error "invalid qname"
	_ -> []
	where
		tyName :: H.DeclHead Ann -> H.Name Ann
		tyName = head . universeBi
		tyArgs :: Data (ast Ann) => ast Ann -> [Text]
		tyArgs n = map prp (universeBi n :: [H.TyVarBind Ann])
		getCtx :: Maybe (H.Context Ann) -> [Text]
		getCtx mctx = map prp (universeBi mctx :: [H.Asst Ann])
		getCtor (H.DataType _) = Data
		getCtor (H.NewType _) = NewType

getConDecl :: H.Name Ann -> H.QualConDecl Ann -> [Symbol]
getConDecl ptype (H.QualConDecl _ _ _ cdecl) = case cdecl of
	H.ConDecl _ n ts -> [mkSymbol n (Constructor (map prp ts) (prp ptype))]
	H.InfixConDecl _ lt n rt -> [mkSymbol n (Constructor (map prp [lt, rt]) (prp ptype))]
	H.RecDecl _ n fs -> mkSymbol n (Constructor [prp t | H.FieldDecl _ _ t <- fs] (prp ptype)) :
		[mkSymbol fn (Selector (Just $ prp ft) (prp ptype) [prp n]) | H.FieldDecl _ fns ft <- fs, fn <- fns]

getGConDecl :: H.Name Ann -> H.GadtDecl Ann -> [Symbol]
#if MIN_VERSION_haskell_src_exts(1,21,0)
getGConDecl _ (H.GadtDecl _ n _ _ Nothing t) =
#else
getGConDecl _ (H.GadtDecl _ n Nothing t) =
#endif
	[mkSymbol n (Constructor (map prp as) (prp res))] where
		(as, res) = tyFunSplit t
		tyFunSplit = go [] where
			go as' (H.TyFun _ arg' res') = go (arg' : as') res'
			go as' t' = (reverse as', t')
#if MIN_VERSION_haskell_src_exts(1,21,0)
getGConDecl ptype (H.GadtDecl _ n _ _ (Just fs) t) =
#else
getGConDecl ptype (H.GadtDecl _ n (Just fs) t) =
#endif
	mkSymbol n (Constructor [prp ft | H.FieldDecl _ _ ft <- fs] (prp t)) :
		[mkSymbol fn (Selector (Just $ prp ft) (prp ptype) [prp n]) | H.FieldDecl _ fns ft <- fs, fn <- fns]

getClassDecl :: H.Name Ann -> H.ClassDecl Ann -> [Symbol]
getClassDecl pclass (H.ClsDecl _ (H.TypeSig _ ns tsig)) = [mkSymbol n (Method (Just $ oneLinePrint tsig) (prp pclass)) | n <- ns]
getClassDecl _ _ = []

prp :: H.Pretty a => a -> Text
prp = fromString . H.prettyPrint


mkSymbol :: H.Name Ann -> SymbolInfo -> Symbol
mkSymbol nm = Symbol (SymbolId (fromName_ $ void nm) (ModuleId (fromString "") noLocation)) Nothing (nm ^? binders . defPos)


-- | Print something in one line
oneLinePrint :: (H.Pretty a, IsString s) => a -> s
oneLinePrint = fromString . H.prettyPrintStyleMode (H.style { H.mode = H.OneLineMode }) H.defaultMode
