# hsdev requests/responses

Client sends request, server sends zero or more notifications and then response. All notification and response has same id as request.

Message is JSON object with string `id` or `null`.<br>
Request also specifies:
 * String `command` — name of command
 * Optional string `current-directory` (default is `.`) — clients current directory
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

### Requests

List of commands with additional fiels:
 * `ping` — ping server
 * `listen` — listen to server's log
     - `level` — optional log level to listen with
 * `set-log` — set log level
     - `level` — log level to set
 * `add` — add data to database
     - `data` — list of objects:
         + `{"database":<db>}`
         + `{"module":<inspected module>}`
         + `{"project":<project>}`
 * `scan` — scan sources and installed modules
     - `projects` — list of projects paths or `.cabal` files
     - `cabal` — scan global and user package-db (`true` of `false`)
     - `sandboxes` — list of `sandbox path`s, where path may contain or be one of:
         + `<path>/.cabal-sandbox` — cabal sandbox
         + `<path>/.stack-work` — stack sandbox
     - `files` — list of `source-file` objects to scan
         + `{"file":<file>,"contents":<contents>}`
     - `paths` — list of directories to scan
     - `ghc-opts` — additional ghc options
     - `docs` — also get docs for sources (may be slow, you can do this by separate command)
     - `infer` — infer types for sources (slow, prefer separate command)
 * `docs`/`infer` — scan docs or infer types for sources
     - `projects` — list of project paths or names
     - `files` — list of sources
     - `modules` — list of module names
 * `remove` — remove data
     - `projects` — list of project `.cabal` files or names to remove
     - `sandboxes` — list of `sandbox` objects to remove (see above)
     - `files` — list of sources
 * `remove-all` — remove all data
 * `packages` — list of packages
 * `projects` — list of projects
 * `sandboxes` — list of sandboxes
 * `symbol` — get symbol info
     - `query` — `query` object:
         + `input` — input string
         + `type` — query type, one of:
             * `exact` — exact match
             * `prefix` — prefix match
             * `infix` — infix match
             * `suffix` — suffix match
             * `regex` — regex match
     - `filters` — list of `filter`-objects:
         + `{"project":<project .cabal or name>}`
         + `{"file":<source file>}`
         + `{"module":<module name>}`
         + `{"deps":<dependent project/source>}`
         + `{"cabal":<"true" or "false">` — restrict to global and user package-db
         + `{"sandbox":<sandbox path>}`
         + `{"db":<package-db>}` — restrict to specific package-db, where `<package-db>` is one of
             * `"global-db"` — global package-db
             * `"user-db"` — user package-db
             * `{"package-db":<path to package-db>}` — specific package-db
         + `{"package":<package name>}`
         + `"sources"` — search for sourced
         + `"standalone"` — search for standalone (project-free)
     - `locals` — search in local declarations (default is `false`)
 * `module` — get module info
     - `query` — `query` object
     - `filters` — list of `filter` objects
     - `header` — return only module header — name and location
 * `project` — get project info, one of:
     - `name` — name of project
     - `path` — path to `.cabal`
 * `sandbox` — get sandbox info
     - `path` — path to sandbox
 * `lookup` — lookup for symbol, this ignores imports and search in projects dependencies
     - `name` — name of symbol
     - `file` — context file
 * `whois` — get info about symbol in scope of file
     - `name` — name of symbol, can be qualified (`Data.List.foldr`), qualified with synonym (`T.pack`)
     - `file` — context file
 * `scope modules` — resolve modules in scope of file (which can be imported)
     - `query` — `query` object
     - `file` — context file
 * `scope` — resolve declarations in scope of file
     - `query` — `query` object
     - `file` — context file
     - `global` — ignore imports
 * `complete` — get completions
     - `prefix` — input prefix
     - `wide` — wide completions, ignores imports
     - `file` — context file
 * `hayoo` — search in hayoo
     - `query` — hayoo query
     - `page` (default is `0`) — start page
     - `pages` (default is `1`) — number of pages
 * `lint`/`check`/`check-lint`/`types` — lint/check source, infer types
     -  `files` — list of fils
     -  `contents` — list of `content` objects
     -  `ghc-opts` — list of additional ghc options (not applied for `lint` command)
 * `autofix show` — show suggestions
     -  `messages` — list of `message` objects, which returned by `lint`/`check`/`check-lint` commands
 * `autofix fix` — apply some suggestions and return updated locations of rest suggestions
     -  `messages` — list of `message` objects to apply
     -  `rest` — list of `message` objects to update locations
     -  `pure` — just update locations, don't actually modify file, useful when some editor applies suggestions itself
 * `ghc eval` — evaluate expression
     - `exprs` — list of expressions
 * `link` — link to server, so that it will exit after client disconnects
     - `hold` — don't send response to this command
 * `exit` — stop server
