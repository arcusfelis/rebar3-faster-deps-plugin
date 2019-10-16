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
            rebar_git_resource:lock(Dir, gitcache_to_git_dep(Source));
        _ ->
            %% Locked deps stay locked
            gitcache_to_git_dep(Source)
    end.

download(Dir, Source, State) ->
    case os:getenv("PREFER_GIT_CLONE") of
        "true" ->
            rebar_git_resource:download(Dir, gitcache_to_git_dep(Source), State);
        _ ->
            download_2(Dir, Source, State)
    end.

download_2(Dir, Source, State) ->
    rebar_log:log(debug, "Download ~p into ~p", [Source, Dir]),
    try download_from_github(Dir, http_or_https_to_git_dep(gitcache_to_git_dep(Source)), State) of
        {ok, _} = Result ->
            Result;
        Other ->
            rebar_log:log(warn, "Download ~p into ~p failed. Reason ~p",
                          [Source, Dir, Other]),
            rebar_git_resource:download(Dir, gitcache_to_git_dep(Source), State)
    catch Class:Reason:Stacktrace ->
            rebar_log:log(warn, "Download ~p into ~p failed. Reason ~p",
                          [Source, Dir, {Class, Reason, Stacktrace}]),
            rebar_git_resource:download(Dir, gitcache_to_git_dep(Source), State)
    end.

needs_update(Dir, Source) ->
    case file:read_file_info(filename:join(Dir, ".git")) of
        {ok, _} ->
            %% already a local git repo
            %% Let's rebar_git_resource to decide what to do
            rebar_git_resource:needs_update(Dir, gitcache_to_git_dep(Source));
        _ ->
            nogit_needs_update(Dir, Source)
    end.

nogit_needs_update(_Dir, _Source) ->
    case os:getenv("PREFER_GIT_CLONE") of
        "true" ->
            true; %% ask to convert to git repo
        _ ->
            false %% do nothing
    end.

make_vsn(Dir) ->
    rebar_git_resource:make_vsn(Dir).

-spec gitcache_to_git_dep(tuple()) -> [tuple()].
gitcache_to_git_dep({gitcache, Repo, Vsn}) ->
    {git, Repo, Vsn}.

http_or_https_to_git_dep({Type, Repo, Vsn}) ->
    {Type, http_or_https_to_git(Repo), Vsn}.

http_or_https_to_git("https://" ++ Rest) ->
    "git://" ++ Rest;
http_or_https_to_git("http://" ++ Rest) ->
    "git://" ++ Rest;
http_or_https_to_git(Other) ->
    Other.

download_from_github(Dir, {git, "git://github.com/" ++ AddrRest, {ref,GitRef}}, State) ->
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
            curl_or_wget_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef)
    end,
    unzip_cached(EscapedZipPath, Dir);
download_from_github(_Dir, _, _State) ->
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

curl_or_wget_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef) ->
    case os:find_executable("curl") of
        false ->
            wget_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef);
        _ ->
            curl_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef)
    end.

curl_from_github(EscapedZipPath, EscapedUserRepo, EscapedGitRef) ->
    %% -L to follow redirects
    Cmd = io_lib:format("curl -L -o ~ts https://github.com/~ts/archive/~ts.zip",
                                 [EscapedZipPath, EscapedUserRepo, EscapedGitRef]),
    rebar_log:log(debug, "Execute ~ts", [Cmd]),
    rebar_utils:sh(Cmd, []).

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

