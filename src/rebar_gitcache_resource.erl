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
            Result;
        Other ->
            io:format(user, "Download ~p into ~p failed. Reason ~p",
                      [Source, Dir, Other]),
            rebar_git_resource:download(Dir, untidy_dep(Source), State)
    catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            io:format(user, "Download ~p into ~p failed. Reason ~p",
                      [Source, Dir, {Class, Reason, Stacktrace}]),
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
    CacheDir = cache_directory(State),
    UserRepo = remove_git_suffix(AddrRest),
    ZipName = GitRef ++ ".zip",
    ZipPath = filename:join([CacheDir] ++ string:tokens(UserRepo, "/") ++ [ZipName]),
    filelib:ensure_dir(ZipPath),
    EscapedGitRef = rebar_utils:escape_chars(GitRef),
    EscapedUserRepo = rebar_utils:escape_chars(UserRepo),
    EscapedZipPath = rebar_utils:escape_chars(ZipPath),
    case file:read_file_info(ZipPath) of
        {ok, _} ->
            %% exist
            ok;
        _ ->
            wget_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef)
    end,
    unzip_cached(EscapedZipPath, Dir);
download_untidy(_Dir, _, _State) ->
    {error, pattern_nomatch}.

remove_git_suffix(AddrRest) ->
    case lists:suffix(".git", AddrRest) of
        true ->
            [_,_,_,_|Tail] = lists:reverse(AddrRest),
            lists:reverse(Tail);
        false ->
            AddrRest
    end.

cache_directory(State) ->
    filename:join([rebar_dir:global_cache_dir(rebar_state:opts(State)), "gitcache"]).

wget_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef) ->
    Cmd = io_lib:format("wget -O ~ts https://github.com/~ts/archive/~ts.zip",
                                 [EscapedZipPath, EscapedUserRepo, EscapedGitRef]),
    io:format(user, "Execute ~ts", [Cmd]),
    rebar_utils:sh(Cmd, []).

unzip_cached(EscapedZipPath, Dir) ->
    UnzipCmd = io_lib:format("unzip ~ts", [EscapedZipPath]),
    io:format(user, "Execute ~ts", [UnzipCmd]),
    rebar_utils:sh(UnzipCmd, [{cd, Dir}]),
    %% Remove one level of inclusion (i.e. zip parent directory)
    rebar_utils:sh("mv ./*/* .", [{cd, Dir}]).
