# hsdev
=====

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
