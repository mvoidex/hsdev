# hsdev requests/responses

Client sends request, server sends zero or more notifications and then response. All notification and response has same id as request.

Message is JSON object with string `id` or `null`.<br>
Request also specifies:
  * String `command` — name of command
  * Optional string `current-directory` (default is `.`) — clients current directory, all file arguments can be relative.
  * Optional flag `no-file` (default is `false`) — don't use memory mapped file (on Windows)
  * Optional `timeout` value (default is `0` for infinite) — timeout for command response
  * Optional flag `silent` (default is `false`) — don't send notifications

Response on request specifies same `id` and can be one of:
  * Result — have `result` object, which contents depends on command
  * Error — have string `error` field and `details` object with detailed error information
  * Notification — have `notify` object with some notification

There are may be zero or many notifications and then result or error.

For example:
<pre>
⋙ {"id":"0","command":"ping"}
⋘ {"id":"0","result":{"message":"pong"}}
⋙ {"id":"1","command":"pyng"}
⋘ {"id":"1","error":"Invalid request","details":{"request":...}}
⋙ {"id":"2","command":"scan","sandboxes":["cabal"]}
⋘ {"id":"2","notify":[{"name":"cabal","task":"scanning","type":"cabal","status":"working","progress":null}]}
⋘ {"id":"2","notify":[{"name":"cabal","task":"scanning","type":"cabal","status":"ok","progress":null}]}
⋘ {"id":"2","result":[]}
</pre>

## Requests/Responses

#### Ping

Ping server

Command: `ping`  
Response: `{"message":"pong"}`

#### Listen

Listen to server's log

Command: `listen`  
Args:
  * `level` — optional log level to listen with, one of `trace`, `debug`, `info`, `warning`, `error`, `fatal`
Response: `{}`

#### Set log level

Command: `set-log`  
Args:
  * `level` — log level to set, one of `trace`, `debug`, `info`, `warning`, `error`, `fatal`
Response: `{}`

#### Scan

Scan sources and installed modules

Command: `scan`  
Args:
  * `projects` — list of projects paths or `.cabal` files
  * `cabal` — scan global and user package-db (`true` or `false`)
  * `sandboxes` — list of `sandbox path`s, where path may be one of:
    - `<path>/.cabal-sandbox` — cabal sandbox
    - `<path>/.stack-work` — stack sandbox
  * `files` — list of `source-file` objects to scan
  * `build-tool` — preferred build tool when scanning projects, `cabal` or `stack`
  * `paths` — list of directories to scan
  * `ghc-opts` — additional ghc options
  * `docs` — also get docs for sources (may be slow, you can do this by separate command)
  * `infer` — infer types for sources (slow, prefer separate command)
Response: `{}`

#### Scan project

Scan project

Command `scan project`
Args:
  * `project` — path to project
  * `build-tool` — build tool to use, `cabal` or `stack`
  * `scan-deps` — also scan dependent package-dbs
Response: `{}`

#### Scan file

Scan file

Command `scan file`
Args:
  * `file` — file path
  * `build-tool` — build tool to use, `cabal` or `stack`
  * `scan-project` — scan related project (if any)
  * `scan-deps` — also scan dependent package-dbs
Response: `{}`

#### Scan package-dbs

Scan list of package-db

Command `scan package-dbs`
Args:
  * `package-db-stack` — list of `package-db`s in reverse order, i.e. `global-db` should be last
Response: `{}`

#### Set file contents

Set source string to use instead of file contents. Calls update. All commands will use this source instead of file. Can be used to perform actions on unsaved file data

Command: `set-file-contents`
Args:
  * `file` — file to set contents
  * `contents` — optional contents, if omitted, source will be dropped
Response: `{}`

#### Refine docs / Infer types

Scan docs or infer types for sources

Command: `docs`/`infer`  
Args:
  * `projects` — list of project paths or names
  * `files` — list of sources
Response: `{}`

#### Remove data

Command: `remove`
Args:
  * `projects` — list of project `.cabal` files or names to remove
  * `cabal` — remove global-db and user-db
  * `sandboxes` — list of `sandbox` objects to remove (see above)
  * `files` — list of sources
Response: `{}`

#### Remove all data

Command: `remove-all`  
Response: `{}`

#### List packages / projects / sandboxes

Command: `packages`/`projects`/`sandboxes`  
Response: list of `package`/`project`/`package-db` objects

#### Symbol

Get symbols info

Command: `symbol`  
Args:
  * `query` — `query` object:
    - `input` — input string
    - `type` — query type, one of:
      + `exact` — exact match
      + `prefix` — prefix match
      + `infix` — infix match
      + `suffix` — suffix match
  * `filters` — list of `filter`-objects:
    - `{"project":<project .cabal or name>}`
    - `{"file":<source file>}`
    - `{"module":<module name>}`
    - `{"package":<package name>}`
    - `"installed"` — search for installed modules
    - `"sources"` — search for sourced
    - `"standalone"` — search for standalone (project-free)
  * `locals` — search in local declarations (default is `false`)
  * `header` — return only header — name and module
Response: list of `symbol-id` (if `header`) or `symbol` objects

#### Module

Get modules info

Command: `module`  
Args:
  * `query` — `query` object
  * `filters` — list of `filter` objects
  * `header` — return only module header — name and location
  * `inspection` — include inspection data
Response: list of `module-id` (if `header`) or `module` objects

#### Project

Get project info

Command: `project`  
Args:
  * `name` — name of project
  * `path` — path to `.cabal`
Response: `project` object

#### Sandbox

Get sandboxes info

Command: `sandbox`  
Args:
  * `path` — path to search sandbox in
Response: list of `sandbox` object

#### Lookup

Lookup for symbol, this ignores imports and search in projects dependencies

Command: `lookup`  
Args:
  * `name` — name of symbol
  * `file` — context file
Response: list of `symbol` objects

#### Whois

Get info about symbol in scope of file

Command: `whois`  
Args:
  * `name` — name of symbol, can be qualified (`Data.List.foldr`), qualified with synonym (`T.pack`)
  * `file` — context file
Response: list of `symbol` objects

#### Whoat

Get info about symbol under cursor

Command: `whoat`  
Args:
  * `line` — line number
  * `column` — column number
  * `file` — file name
Response: list of `symbol` objects

#### Scope modules

Resolve modules in scope of file (which can be imported)

Command: `scope modules`  
Args:
  * `query` — `query` object
  * `file` — context file
Response: list of `module-id` objects

#### Scope

Resolve declarations in scope of file

Command: `scope`  
Args:
  * `query` — `query` object
  * `file` — context file
Response: list of `symbol-id` objects

#### Usages

Find usages of symbol

Command: `usages`  
Args:
  * `line` — line number
  * `column` — column number
  * `file` — file name
Response: list of `symbol-usage` objects

#### Complete

Get completions

Command: `complete`  
Args:
  * `prefix` — input prefix
  * `wide` — wide completions, ignores imports
  * `file` — context file
Response: list of `symbol` objects


#### Hayoo

Search in hayoo

Command: `hayoo`  
Args:
  * `query` — hayoo query
  * `page` (default is `0`) — start page
  * `pages` (default is `1`) — number of pages
Response: list of `symbol` objects

#### Cabal list

Cabal list

Command: `cabal list`  
Args:
  * `packages` — packages to list
Response: list of `cabal-package` objects

#### Unresolveds

List all unresolved symbols in source file

Command: `unresolveds`  
Args:
  * `files` — list of files
Response: list of `unresolved-symbol` objects

#### Lint / Check / Check & Lint / Types

Lint/check source, infer types

Command: `lint`/`check`/`check-lint`/`types`  
Args:
  * `files` — list of `source-file` objects
  * `ghc-opts` — list of additional ghc options (not applied for `lint` command)
  * `lint-opts` — list of hlint options
  * `clear` (except `lint`) — set to clear targets before running command
Response: list of `note output-message` objects

#### Autofixes

Show suggestions

Command: `autofixes`  
Args:
  * `messages` — list of `note output-message` objects, which returned by `lint`/`check`/`check-lint` commands
Response: list of `note refact` objects

#### Refactor

Apply some suggestions and return updated locations of rest suggestions

Command: `refactor`  
Args:
  * `messages` — list of `note refact` objects to apply
  * `rest` — list of `note refact` objects to update locations
  * `pure` — just update locations, don't actually modify file, useful when some editor applies suggestions itself
Response: list of `note refact` objects with updated locations

#### Rename

Get corrections to rename symbol, applied with `refactor`

Command: `rename`  
Args:
  * `name` — symbol to rename
  * `new-name` — new name
  * `file` — symbol definition file
Response: list of `note refact` objects

#### Ghc eval

Evaluate expression

Command: `ghc eval`  
Args:
  * `exprs` — list of expressions
  * `file` — optional `file-source` context
Response: list of `repl-result` objects

#### Ghc type

Get type of expression

Command: `ghc type`
Args:
  * `exprs` — list of expressions
  * `file` — optional `file-source` context
Response: list of `repl-result` objects

#### Langs

List language extensions

Command: `langs`  
Response: list of language extensions as strings

#### flags

List compiler flags

Command: `flags`  
Response: list of flags as strings

#### Link

Link to server, so that it will exit after client disconnects

Command: `link`  
Args:
  * `hold` — don't send response to this command
Response: `{}`

#### Exit

Stop server

Command: `exit`  
Response: `{}`

### Objects

#### Package

Package object  
Fields:
  * `name` — name of package
  * `version` — stringized version of package

#### Project

Project object  
Fields:
  * `name` — name of project
  * `path` — path to project (directory)
  * `cabal` — path to cabal file
  * `description` — object:
    - `version` — project version
    - `library` — library description, object:
      + `modules` — list of exposed modules
      + `info` — library `build-info` object
    - `executables` — executable descriptions, list of objects:
      + `name` — executable name
      + `path` — path to executable
      + `info` — executable `build-info` object
    - `tests` — tests descriptions, list of objects:
      + `name` — test name
      + `enabled` — is enabled
      + `main` — main module
      + `info` — test `build-info` object

#### Build-info

Build info object  
Fields:
  * `build-depends` — list of dependencies
  * `language` — language
  * `extensions` — list of extensions
  * `ghc-options` — list of ghc options
  * `source-dirs` — list of source directories
  * `other-modules` — list of other modules

#### Package-db

Package db  
One of:
  * `"global-db"` — global package-db
  * `"user-db"` — user package-db
  * `"package-db:<path>"` — custom package-db

#### Sandbox

Just string with path to sandbox

#### Module-id

Module id  
Fields:
  * `name` — module name
  * `location` — location object, one of:
    - source file:
      + `file` — path to source file
      + `project` — path to cabal file (or `null`)
    - installed module:
      + `dirs` — list of installed dirs
      + `package` — package in form `<package>-<version>`
      + `name` — name of module
    - other:
      + `source` — some ident for module
    - no location: empty object `{}`

#### Symbol-id

Symbol id  
Fields:
  * `name` — symbol name
  * `module` — `module-id` object

#### Module

Module info  
Fields:
  * `id` — `module-id`
  * `docs` — module docs
  * `exports` — exports, list of `symbol`s
  * `fixities` — list of fixities objects:
    - `assoc` — associativity, one of `none`, `left` or `right`
    - `prior` — priority
    - `name` — name of operator

#### Symbol

Symbol info  
Fields:
  * `id` — `symbol-id`
  * `docs` — symbol docs
  * `pos` — symbol position, object:
    - `line` — line
    - `column` — column
  * `info` — symbol info, object:
    - `what` — symbol kind, one of:
      + `"function"` — function
      + `"method"` — class method
      + `"selector"` — record selector
      + `"ctor"` — constructor
      + `"type"` — type
      + `"newtype"` — newtype
      + `"data"` — data
      + `"class"` — class
      + `"type-family"` — type family
      + `"data-family"` — data family
      + `"pat-ctor"` — pattern constructor
      + `"pat-selector"` — pattern selector
    - `args` — list of arguments (type symbols)
    - `associate` — ? (`type-family`/`data-family`)
    - `class` — class name (`method`)
    - `constructor` — constructor name (`pat-selector`)
    - `constructors` — constructors names (`selector`)
    - `ctx` — list of contexts (type symbols)
    - `parent` — record name (`selector`)
    - `pat-type` — pattern type (`pat-type`/`pat-selector`)
    - `type` — function type (`function`/`method`/`selector`/`ctor`/`pat-selector`)

#### Symbol-usage

Symbol usage  
Fields:
  * `symbol` — `symbol` object
  * `qualifier` — qualifeir symbol used with (optional)
  * `in` — `module-id`, where symbol used
  * `at` — `region`, where symbol used

#### Cabal-package

Cabal package info:
Fields:
  * `name` — package name
  * `synopsis` — package description
  * `default-version` — default cabal version
  * `installed-versions` — list of installed versions
  * `homepage` — url of homepage
  * `license` — name of license

#### Unresolved-symbol

Unresolved symbol  
Fields:
  * `qualified` — qualifier
  * `name` — name
  * `line` — line
  * `column` — column

#### Source-file

Source file
Fields:
  * `file` — path to file
  * `contents` — optional contents of file


#### Note

Note object with some additional info  
Fields:
  * `source` — module location of note, `location`
  * `region` — region of note, `region` object
  * `level` — severity of note, one of `error`/`warning`/`hint`
  * `note` — additional note info

#### Output-message

Lint / Check / etc. output message  
Fields:
  * `message` — message text
  * `suggestion` — suggestion message

#### Refact

Refact object  
Fields:
  * `message` — refact message
  * `action` — refact action object:
    - `region` — region to replace
    - `contents` — replace with

#### Region

Region object  
Fields:
  * `from` — start of region:
    - `line` — line
    - `column` — column
  * `to` — end of region


#### Repl-result

Result of some interactive evaluations
Object with only one field:
  * `ok` — evaluation result
  * `error` — error message
