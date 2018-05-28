-module(rebar_gitcache_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

lock(Dir, Source) ->
    rebar_git_resource:lock(Dir, untidy_dep(Source)).

download(Dir, Source, State) ->
    io:format(user, "Download ~p into ~p", [Source, Dir]),
    rebar_git_resource:download(Dir, untidy_dep(Source), State).

needs_update(Dir, Source) ->
    rebar_git_resource:make_vsn(Dir, untidy_dep(Source)).

make_vsn(Dir) ->
    rebar_git_resource:make_vsn(Dir).

-spec untidy_dep(tuple()) -> [tuple()].
untidy_dep({gitcache, Repo, Vsn}) ->
    {git, Repo, Vsn}.
