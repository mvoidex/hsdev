# hsdev

[![Build Status](https://travis-ci.org/mvoidex/hsdev.png)](https://travis-ci.org/mvoidex/hsdev)

Haskell development library and tool with support of autocompletion, symbol info, go to declaration, find references, hayoo search etc.

## Usage

Use `hsdev server start` to start remove server. Specify `--cache`, where `hsdev` will store information.

### Commands

* `server` — server commands: `start`, `run` and `stop`
* `scan` — scan installed modules, cabal projects and files
* `list` — list modules, packages and projects
* `symbol`, `module` and `project` — get info about symbol, module or project
* `lookup`, `whois` — find visible symbol, or symbol in scope of file
* `scope` — get modules or declarations, accessible from file
* `complete` — get completions for file and input
* `hayoo` — search in hayoo
* `cabal list` — search packages info
* `ghc-mod type` — get type of expression at line and column of file
* `dump` — dump modules or projects info

### Examples

<pre>
PS> hsdev server start --cache cache
Server started at port 4567
PS> hsdev scan cabal
{}
PS> hsdev scan --proj hsdev
{}
PS> hsdev list modules --proj hsdev | json | % { $_.name } | select -first 3
Data.Async
Data.Group
HsDev
PS> hsdev symbol enumProject | json | % { $_.declaration } | % { $_.name + ' :: ' + $_.decl.type }
enumProject :: Project -> ErrorT String IO ProjectToScan
PS> hsdev complete C -f .\hsdev\tools\hsdev.hs | json | % { $_.declaration.name }
ClientOpts
CommandAction
CommandOptions
CommandResult
PS> hsdev symbol foldr | json | % { $_.declaration.name + ' :: ' + $_.declaration.decl.type + ' -- ' + $_.'module-id'.name } | select -first 3
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a -- Data.ByteString
foldr :: (Char -> a -> a) -> a -> ByteString -> a -- Data.ByteString.Char8
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a -- Data.ByteString.Lazy
PS> hsdev server stop
{}
</pre>

## Tools

### HsInspect

Tool to inspect source files, .cabal files and installed modules

<pre>
PS> hsinspect cabal .\hsdev.cabal | json | % { $_.description.library.modules[3] }
HsDev.Cache
PS> hsinspect file .\tools\hsdev.hs | json | % { $_.module.declarations } | % { $_.name + ' :: ' + $_.decl.type }
main :: IO ()
printMainUsage :: IO ()
printUsage :: IO ()
PS> hsinspect module Data.Either | json | % { $_.module.declarations } | % { $_.name + ' :: ' + $_.decl.type }
Either :: data Either a b
Left :: a -> Either a b
Right :: b -> Either a b
either :: (a -> c) -> (b -> c) -> Either a b -> c
isLeft :: Either a b -> Bool
isRight :: Either a b -> Bool
lefts :: [Either a b] -> [a]
partitionEithers :: [Either a b] -> ([a], [b])
rights :: [Either a b] -> [b]
</pre>

### HsCabal

Cabal helper with JSON output. For now only `list` command supported

<pre>
PS> hscabal list aeson | json | ? { $_.'installed-versions' } | % { $_.name + ' - ' + $_.'installed-versions' }
aeson - 0.7.0.3
aeson-pretty - 0.7.1
</pre>

### HsHayoo

Search in hayoo

<pre>
PS> hshayoo "(a -> b) -> c" | json | % { $_.declaration } | % { $_.name + ' :: ' + $_.decl.type } | select -first 5
unC :: (tau -> a) -> b
map3 :: (a -> b) -> f (g (h a)) -> f (g (h b))
map2 :: (a -> b) -> f (g a) -> f (g b)
map' :: (a -> b) -> map a -> map b
map :: (a -> b) -> map a -> map b
</pre>

### Hayoo in GHCi

You can use this `ghci.conf` to allow search from `ghci`:

<pre>
:set prompt "λ> "

import Control.Monad.Error
import HsDev.Tools.Hayoo

:{
let
	showHayooFunction f =
		(hayooName f ++ " :: " ++ hayooSignature f) :
		(map ('\t':) $
			lines (untagDescription (hayooDescription f)) ++
			["-- Defined in '" ++ hayooModule f ++ "', " ++ hayooPackage f])
	showHayoo = concatMap showHayooFunction . hayooFunctions
:}

:def hayoo \s -> return $ "runErrorT (hayoo \"" ++ s ++ "\") >>= (mapM_ putStrLn) . either (return . (\"Error: \" ++)) showHayoo"
</pre>

Usage:

<pre>
λ> :hayoo (a -> c) -> (b -> c)
either :: (a -> c) -> (b -> c) -> Either a b -> c
	Case analysis for the Either type.
	 If the value is Left a, apply the first function to a;
	 if it is Right b, apply the second function to b.
	-- Defined in 'Prelude', base
...
</pre>

### JSON

#### PowerShell

I'm using PowerShell and this function (in `$env:USERPROFILE\Documents`) to parse JSON output:

<pre>
Add-Type -AssemblyName System.Web.Extensions

function json
{
    <#
    .synopsis
    Decode JSON
    .example
    PS> echo "{'x':123,'y':22}" | json | % { echo $_.y }
    22
    #>

    param(
        [Parameter(ValueFromPipeline = $true)]$i)

    $jsser = new-object System.Web.Script.Serialization.JavaScriptSerializer
    $jsser.MaxJsonLength = $i.length + 100 # Make limit big enough
    $jsser.RecursionLimit = 100
    $jsser.DeserializeObject($i)
}
</pre>

which returns `PSObject`, that can be inspected in common way:

<pre>
PS> hsinspect module GHC -g "-package ghc" | json | % { $_.module.declarations } | % { $_.name + ' :: ' + $_.decl.type } | select -first 5
ABE :: id -> id -> HsWrapper -> TcSpecPrags -> ABExport id
ABExport :: data ABExport id
ACoAxiom :: CoAxiom Branched -> TyThing
AConLike :: ConLike -> TyThing
ATyCon :: TyCon -> TyThing
</pre>

#### [jq](http://stedolan.github.io/jq/)

Another way is to use [jq](http://stedolan.github.io/jq/) tool, which is pretty simple:

<pre>
PS> hsinspect module GHC -g "-package ghc" | jq -r '.module.declarations[range(0;5)] | .name + \" :: \" + .decl.type'
ABE :: id -> id -> HsWrapper -> TcSpecPrags -> ABExport id
ABExport :: data ABExport id
ACoAxiom :: CoAxiom Branched -> TyThing
AConLike :: ConLike -> TyThing
ATyCon :: TyCon -> TyThing
</pre>
