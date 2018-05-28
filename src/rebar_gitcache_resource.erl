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
    try download_untidy(Dir, untidy_dep(Source), State) of
        {ok, _} = Result ->
            rebar_git_resource:download(Dir, untidy_dep(Source), State);
        Other ->
            io:format(user, "Download ~p into ~p failed. Reason ~p",
                      [Source, Dir, Other]),
            rebar_git_resource:download(Dir, untidy_dep(Source), State)
    catch Class:Reason ->
            io:format(user, "Download ~p into ~p failed. Reason ~p",
                      [Source, Dir, {Class, Reason}]),
            rebar_git_resource:download(Dir, untidy_dep(Source), State)
    end.

needs_update(Dir, Source) ->
    rebar_git_resource:make_vsn(Dir, untidy_dep(Source)).

make_vsn(Dir) ->
    rebar_git_resource:make_vsn(Dir).

-spec untidy_dep(tuple()) -> [tuple()].
untidy_dep({gitcache, Repo, Vsn}) ->
    {git, Repo, Vsn}.

download_untidy(Dir, {git, "git://github.com/" ++ AddrRest, {ref,GitRef}}, State) ->
    UserRepo = remove_git_suffix(AddrRest),
    EscapedGitRef = rebar_utils:escape_chars(GitRef),
    EscapedUserRepo = rebar_utils:escape_chars(UserRepo),
    Cmd = io_lib:format("wget -O /tmp/~ts.zip https://github.com/~ts/archive/~ts.zip",
                                 [EscapedGitRef, EscapedUserRepo, EscapedGitRef]),
    io:format(user, "Execute ~ts", [Cmd]),
    rebar_utils:sh(Cmd, []);
download_untidy(Dir, _, State) ->
    {error, pattern_nomatch}.

remove_git_suffix(AddrRest) ->
    case lists:suffix(".git", AddrRest) of
        true ->
            [_,_,_,_|Tail] = lists:reverse(AddrRest),
            lists:reverse(Tail);
        false ->
            AddrRest
    end.
