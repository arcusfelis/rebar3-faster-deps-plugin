-module(rebar_gitcache_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

lock(Dir, Source) ->
    case file:read_file_info(filename:join(Dir, ".git")) of
        {ok, _} ->
            %% Repo was converted from raw to git (after needs_update called)
            rebar_git_resource:lock(Dir, untidy_dep(Source));
        _ ->
            %% Locked deps stay locked
            untidy_dep(Source)
    end.

download(Dir, Source, State) ->
    rebar_log:log(debug, "Download ~p into ~p", [Source, Dir]),
    try download_untidy(Dir, untidy_dep(Source), State) of
        {ok, _} = Result ->
            Result;
        Other ->
            rebar_log:log(warning, "Download ~p into ~p failed. Reason ~p",
                          [Source, Dir, Other]),
            rebar_git_resource:download(Dir, untidy_dep(Source), State)
    catch Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            rebar_log:log(warning, "Download ~p into ~p failed. Reason ~p",
                          [Source, Dir, {Class, Reason, Stacktrace}]),
            rebar_git_resource:download(Dir, untidy_dep(Source), State)
    end.

needs_update(_Dir, _Source) ->
    false.

make_vsn(Dir) ->
    rebar_git_resource:make_vsn(Dir).

-spec untidy_dep(tuple()) -> [tuple()].
untidy_dep({gitcache, Repo, Vsn}) ->
    {git, http_or_https_to_git(Repo), Vsn}.

http_or_https_to_git("https://" ++ Rest) ->
    "git://" ++ Rest;
http_or_https_to_git("http://" ++ Rest) ->
    "git://" ++ Rest;
http_or_https_to_git(Other) ->
    Other.

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
    rebar_log:log(debug, "Execute ~ts", [Cmd]),
    rebar_utils:sh(Cmd, []).

unzip_cached(EscapedZipPath, Dir) ->
    UnzipCmd = io_lib:format("unzip ~ts", [EscapedZipPath]),
    rebar_log:log(debug, "Execute ~ts", [UnzipCmd]),
    Result = rebar_utils:sh(UnzipCmd, [{cd, Dir}]),
    %% Unwrap from a root zip directory
    {ok, [RootDir]} = file:list_dir(Dir),
    FullRootDir = filename:join(Dir, RootDir),
    {ok, RepoFiles} = file:list_dir(FullRootDir),
    [ok = file:rename(filename:join(FullRootDir, File), filename:join(Dir, File))
     || File <- RepoFiles],
    file:del_dir(FullRootDir),
    Result.

