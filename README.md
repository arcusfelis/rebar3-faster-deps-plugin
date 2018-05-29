# Even faster deps!

This plugin optimizes the way rebar dependencies are dowloaded from GitHub.

It changes how rebar3 handles dependencies hosted on github.

It analyzes context of `rebar.lock`, and downloads the locked dependencies
using github ZIP API, using links like this:

```erlang
http://github.com/$USER/$REPO/archive/$SHA.zip
http://github.com/soranoba/bbmustache/archive/d731d93e351a2ae567f93c4991c3f421626fab5c.zip
```

It caches the archives inside `~/.cache/rebar3/gitcache/`. It would not dowload
the same dependency twice.

Add `~/.cache/rebar3` to cache on Travis to speed up your builds. :)
Caching is very similar to how Hex packages implement it.

If downloading fails, rebar3 would retry to download using the original dependency
address.

# How to enable

```erlang
{plugins,
 [
  {rebar_faster_deps,
    {git, "https://github.com/arcusfelis/rebar3-faster-deps-plugin.git",
      {branch, "master"}}}
 ]}.
```

No need to touch `deps` section.

No need to touch `rebar.lock` (but ensure that it exist and not empty!).
Not locked deps are downloaded regularly, without this plugin help.

# rebar_tidy_deps is cool too :)

Based on rebar_tidy_deps originally.
It's a good place to start developing a rebar3 plugin.
Currently has no common parts with it.
