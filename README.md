# hsdev

[![Hackage version](https://img.shields.io/hackage/v/hsdev.svg?style=flat)](http://hackage.haskell.org/package/hsdev) [![Build Status](https://travis-ci.org/mvoidex/hsdev.png)](https://travis-ci.org/mvoidex/hsdev)

Haskell development library and tool with support of autocompletion, symbol info, go to declaration, find references, hayoo search etc.

## Usage

Use `hsdev start` to start remote server. Specify `--cache`, where `hsdev` will store information.

### Commands

* `start`, `run` and `stop` — server commands
* `connect` — interactive connect to server
* `ping` — ping server
* `add` — add inspected modules
* `scan`, `rescan` — scan installed modules, cabal projects and files
* `remove` — unload data
* `modules`, `packages`, `projects` — list information about specified modules, packages or projects
* `resolve` — resolve scope symbols and exports for sources module
* `symbol`, `module`, `project` — find symbol, module or project
* `lookup`, `whois` — find visible or imported symbol
* `scope` — get modules or declarations, accessible from file
* `complete` — get completions for file and input
* `hayoo` — search in hayoo
* `cabal list` — search packages info
* `ghc-mod type`, `ghc-mod check`, `ghc-mod lint`, `ghc-mod lang`, `ghc-mod flags` — run `ghc-mod` command in corresponding ghc-mod worker (separate workers per project and per sandbox)
* `dump` — dump modules or projects info
* `load` — load data

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

## Requests/responses

Client sends request, server sends zero or more notifications and then response. All notification and response has same id as request.
Requests ans responses can be in JSON or s-exp.

Requests to `hsdev` have following structure (but must not be in pretty format):
<pre>
{
    "id": &lt;message id&gt;
    "command": &lt;commandname&gt;,
    "args": &lt;positional arguments&gt;,
    "opts": &lt;named arguments&gt;,
}
</pre>
or as s-exp:
<pre>
(:id &lt;message id&gt; :command &lt;commandname&gt; :args &lt;positional arguments&gt; :opts &lt;named arguments&gt;)
</pre>

* `id` : string, optional — message id
* `command` : string, required — name of command, for example "scan", "symbol" or "ghc-mod type"
* `args` : list of strings, optional — positional arguments for command
* `opts` : dictionary, optional — named arguments for command, simple rules:
  `--port 1234` ⇒ `"port": "1234"` or `:port "1234"` for s-exp
  `--quiet` ⇒ `"quiet": null` or `:quiet null` for s-exp
  `--file f1 --file f2 --file f3` ⇒ `"file": ["f1", "f2", "f3"]` or `:file ("f1" "f2" "f3")` for s-exp

Notification
JSON:
<pre>
{
    "id": &lt;notification id&gt;,
    "notify": &lt;notification&gt;
}
</pre>
s-exp:
<pre>
(:id &lt;notification id&gt; :notify &lt;notification&gt;)
</pre>

Response
JSON:
<pre>
{
    "id": &lt;notification id&gt;,
    "result": &lt;response data&gt;
}
</pre>
or
<pre>
{
    "id": &lt;notification id&gt;,
    "error": &lt;error message&gt;,
    "details": &lt;error details&gt;
}
</pre>
s-exp:
<pre>
(:id &lt;notification id&gt; :result &lt;response data&gt;)
</pre> 
or
<pre>
(:id &lt;notification id&gt; :error &lt;error message&gt; :details &lt;error details&gt;)
</pre>

If request has flag `--silent`, no notifications will be sent from server.

### Examples

JSON:
<pre>
⋙ {"id":"1",command":"scan","opts":{"cabal":null}}
⋘ {"id":"1","notify":{...}}
⋘ {"id":"1","notify":{...}}
⋘ {"id":"1","result":[]}
⋙ {"id":"2","command":"symbol","args":["either"]}
⋘ {"id":"2","result":[{...},{...},{...},{...}]}
</pre>
s-exp:
<pre>
⋙ (:id "1" :command "scan" :opts (:cabal null))
⋘ (:id "1" :notify (...))
⋘ (:id "1" :notify (...))
⋘ (:id "1" :result ())
⋙ (:id "2" :command "symbol" :args ("either"))
⋘ (:id "2" :result ((...) (...) (...) (...)))
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

I'm using PowerShell and this function (in `$env:USERPROFILE\Documents\WindowsPowerShell\profile.ps1`) to parse JSON output:

<pre>
Add-Type -AssemblyName System.Web.Extensions

function json
{
    &lt;#
    .synopsis
    Decode JSON
    .example
    PS&gt; echo "{'x':123,'y':22}" | json | % { echo $_.y }
    22
    #&gt;

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
