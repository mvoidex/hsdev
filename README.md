# hsdev

[![Hackage version](https://img.shields.io/hackage/v/hsdev.svg?style=flat)](http://hackage.haskell.org/package/hsdev) [![Build Status](https://travis-ci.org/mvoidex/hsdev.png)](https://travis-ci.org/mvoidex/hsdev)

Haskell development library and tool with support of autocompletion, symbol info, go to declaration, find references, hayoo search etc.
Uses [fsnotify](http://hackage.haskell.org/package/fsnotify) to watch for changes.
And several utils `hsinspect`, `hsclearimports`, `hscabal`, `hshayoo`, `hsautofix`

## Usage

Use `hsdev start` to start remote server. Specify `--cache`, where `hsdev` will store information.
Then you can connect to server and send requests (see [requests/responses](MESSAGES.md)) or you can use `hsdev` itself. It will send command to server and outputs the response.

### Commands

* `version` — returns version number
* `start`, `run` and `stop` — server commands
* `connect` — interactive connect to server
* `ping` — ping server
* `listen` — connect to server and listen for its log (for debug)
* `add` — add inspected modules
* `scan`, `rescan` — scan installed modules, cabal projects and files (`rescan` is deprecated because of using `fsnotify`)
* `remove` — unload data
* `modules`, `packages`, `projects`, `sandboxes` — list information about specified modules, packages, projects or sandboxes
* `resolve` — resolve scope symbols and exports for sources module
* `symbol`, `module`, `project` — find symbol, module or project
* `lookup`, `whois` — find visible or imported symbol
* `scope`, `scope modules` — get modules or declarations, accessible from file
* `complete` — get completions for file and input
* `hayoo` — search in hayoo
* `cabal list` — search packages info
* `lint`, `check`, `lint-check` — lint or check source files. These commands have some advantages over `ghc-mod` ones: `lint` uses `hlint` as library, `check` returns more precise source position and also uses project description to pass `-package` flags, these commands also can accept file contents
* `ghc-mod type`, `ghc-mod check`, `ghc-mod lint`, `ghc-mod lang`, `ghc-mod flags` — run `ghc-mod` command in corresponding ghc-mod worker (separate workers per project and per sandbox)
* `autofix show`, `autofix fix` — commands to fix some warnings and apply `hlint` suggestions
* `dump` — dump modules or projects info
* `load` — load data

#### TODO: Detailed commands description with examples

#### Scan

Scans sources, projects, directories, sandboxes and installed modules. After scan hsdev watches for changes in directory and automatically rescans modifified sources.
<pre>
PS> hsdev scan --cabal --path path/to/projects --project path/to/some/project --file File.hs
</pre>

#### Resolve

Resolve source module scope, taking into account reexports and import/export lists. When flag `--exports` set, resolve only exported declarations.
<pre>
PS> hsdev resolve --file .\src\HsDev\Cabal.hs --exports | json | % { $_.declarations.name }
Cabal
Sandbox
cabalOpt
findPackageDb
getSandbox
isPackageDb
locateSandbox
sandbox
searchSandbox
</pre>

#### Whois

Get information for symbol in context of source file. Understand qualified names and also names qualified with module shortcut (`import ... as`), note `M.` qualified for `map`:
<pre>
PS> hsdev whois M.map --file .\src\HsDev\Symbols\Resolve.hs | json | % { $_.declaration.decl.type }
(a -> b) -> Map k a -> Map k b
</pre>

#### AutoFix

Autofix commands used to assist for automatic fix of some warnings and hints from `hlint`. `autofix show` command parses `check` and `lint` command output, and returns `corrections` — data with regions and suggestions to fix. `autofix fix` command perform fix of selected corrections and also updates positions of other corrections, such that they stay relevant. These updated corrections can be used to pass them to `autofix fix` again.
Example of interactive command, based on this command in SublimeHaskell:
![autofix](https://raw.githubusercontent.com/SublimeHaskell/SublimeHaskell/hsdev/Commands/AutoFix.gif)

<pre>
# Get corrections
PS> $corrs = hsdev check-lint ..\haskell\Test.hs | hsdev autofix show --stdin
# Apply first correction, other corrections passed as --rest param to update their positions
# Result is updated --rest corrections, which can be used again
PS> $corrs2 = ($corrs | jq '[.[1]]' -c -r) | hsdev autofix fix --pure --rest (escape ($corrs | jq '.[1:5]' -c -r)) --stdin
# One more
PS> $corrs2 | hsdev autofix fix --stdin --pure
</pre>


### Examples

<pre>
PS> hsdev start --cache cache
Server started at port 4567
PS> hsdev scan --cabal
{}
PS> hsdev scan --project hsdev
{}
PS> hsdev modules --project hsdev | json | % { $_.result.name } | select -first 3
Data.Async
Data.Group
HsDev
PS> hsdev symbol enumProject | json | % { $_.result.declaration } | % { $_.name + ' :: ' + $_.decl.type }
enumProject :: Project -> ErrorT String IO ProjectToScan
PS> hsdev complete C -f .\hsdev\tools\hsdev.hs | json | % { $_.result.declaration.name }
ClientOpts
CommandAction
CommandOptions
CommandResult
PS> hsdev symbol foldr | json | % result | % { $_.declaration.name + ' :: ' + $_.declaration.decl.type + ' -- ' + $_.'module-id'.name } | select -first 3
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a -- Data.ByteString
foldr :: (Char -> a -> a) -> a -> ByteString -> a -- Data.ByteString.Char8
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a -- Data.ByteString.Lazy
PS> hsdev stop
{}
</pre>

## Tools

### HsInspect

Tool to inspect source files, .cabal files and installed modules. For source files it also scan docs and try infer types.

<pre>
PS> hsinspect .\hsdev.cabal | json | % { $_.description.library.modules[3] }
HsDev.Cache
PS> hsinspect .\tools\hsdev.hs | json | % { $_.module.declarations } | % { $_.name + ' :: ' + $_.decl.type }
main :: IO ()
printMainUsage :: IO ()
printUsage :: IO ()
PS> hsinspect Data.Either | json | % { $_.module.declarations } | % { $_.name + ' :: ' + $_.decl.type }
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

### HsAutoFix

Tool to fix some build warnings and to apply hlint suggestions

* `hsautofix show` — read output messages and return suggestions
* `hsautofix fix -n <i> -n ...` — fix selected suggestions (by index) and return updated rest suggestions, so this command can be chained. There are also flag `--pure`. If set, files will not be modified. Use it, if you don't want `hsautofix` to apply fixes itself.

Here's how you can apply first three suggestions sequentically. On each step suggestion is removed, so we just apply first suggestion three times.
We can do the same in reverse order, and in that case indices will be 3, 2, 1. And we can apply all three in one call.
Three commands below have same effect
<pre>
PS> ghc-mod Test.hs | hsautofix show | hsautofix fix -n 1 | hsautofix fix -n 1 | hsautofix fix -n 1
...
PS> ghc-mod Test.hs | hsautofix show | hsautofix fix -n 3 | hsautofix fix -n 2 | hsautofix fix -n 1
...
PS> ghc-mod Test.hs | hsautofix show | hsautofix fix -n 1 -n 2 -n 3
...
</pre>

### Hayoo in GHCi

You can use this `ghci.conf` to allow search from `ghci`:

<pre>
:set prompt "λ> "

import Control.Monad.Error
import HsDev.Tools.Hayoo
import Data.List (intercalate)

:{
let
    showHayooFunction f =
        (hayooName f ++ " :: " ++ hayooSignature f) :
        (map ('\t':) $
            lines (untagDescription (hayooDescription f)) ++
            ["-- Defined in '" ++ intercalate ", " (hayooModules f) ++ "', " ++ hayooPackage f])
    showHayoo = concatMap showHayooFunction . resultResult
:}

:def hayoo \s -> return $ "runErrorT (hayoo \"" ++ s ++ "\" Nothing) >>= (mapM_ putStrLn) . either (return . (\"Error: \" ++)) showHayoo"
</pre>

Usage:

<pre>
λ> :hayoo (a -> c) -> (b -> c)
query :: (a -> c) -> b -> c
        query f x walks the structure x (bottom up) and applies f
         to every a, appending the results.
        -- Defined in 'Text.Pandoc.Walk', pandoc-types
...
</pre>

### JSON

#### PowerShell

I'm using PowerShell and `json` function from [PSUtils](https://github.com/mvoidex/PSUtils) to parse JSON output, which returns `PSObject`, that can be inspected in common way:

<pre>
PS> hsinspect module GHC -g "-package ghc" | json | % { $_.module.declarations } | % { $_.name + ' :: ' + $_.decl.type } | select -first 5
ABE :: id -&gt; id -&gt; HsWrapper -&gt; TcSpecPrags -&gt; ABExport id
ABExport :: data ABExport id
ACoAxiom :: CoAxiom Branched -&gt; TyThing
AConLike :: ConLike -&gt; TyThing
ATyCon :: TyCon -&gt; TyThing
</pre>

#### [jq](http://stedolan.github.io/jq/)

Another way is to use [jq](http://stedolan.github.io/jq/) tool, which is pretty simple:

<pre>
PS> hsinspect module GHC -g "-package ghc" | jq -r '.module.declarations[range(0;5)] | .name + \" :: \" + .decl.type'
ABE :: id -&gt; id -&gt; HsWrapper -&gt; TcSpecPrags -&gt; ABExport id
ABExport :: data ABExport id
ACoAxiom :: CoAxiom Branched -&gt; TyThing
AConLike :: ConLike -&gt; TyThing
ATyCon :: TyCon -&gt; TyThing
</pre>
