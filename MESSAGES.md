# hsdev requests/responses

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

