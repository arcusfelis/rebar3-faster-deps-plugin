-module(rebar_faster_deps).

-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    case is_supported() of
        false ->
            rebar_log:log(warn, "[rebar_faster_deps] Disabled. Please install unzip and wget", []),
            {ok, State};
        true ->
            use_gitcache_for_locked_deps(State)
    end.

%% ===================================================================
%% Code helpers
%% ===================================================================

use_gitcache_for_locked_deps(State) ->
    Locks = rebar_state:get(State, {locks, default}, []),
    Locks2 = [preprocess_lock_entry(Lock) || Lock <- Locks],
    State2 = rebar_state:set(State, {locks, default}, Locks2),
    rebar_log:log(info, "[rebar_faster_deps] Rewrite locks", []),
    rebar_log:log(info, "[rebar_faster_deps] To clone using git command, set env variable PREFER_GIT_CLONE=true and run rebar3 get-deps", []),
    rebar_log:log(info, "[rebar_faster_deps] Example: PREFER_GIT_CLONE=true ./rebar3 get-deps", []),

    {ok, rebar_state:add_resource(State2, {gitcache, rebar_gitcache_resource})}.

preprocess_lock_entry({DepName, {git,"git://github.com/" ++ _ = Addr, {ref, ShaRef}}, DepLevel}) ->
    {DepName, {gitcache, Addr, {ref, ShaRef}}, DepLevel};
preprocess_lock_entry({DepName, {git,"http://github.com/" ++ _ = Addr, {ref, ShaRef}}, DepLevel}) ->
    {DepName, {gitcache, Addr, {ref, ShaRef}}, DepLevel};
preprocess_lock_entry({DepName, {git,"https://github.com/" ++ _ = Addr, {ref, ShaRef}}, DepLevel}) ->
    {DepName, {gitcache, Addr, {ref, ShaRef}}, DepLevel};
preprocess_lock_entry(Other) ->
    Other.

is_supported() ->
    has_command_alternatives(command_alternatives()).

command_alternatives() ->
    [["curl", "wget"],
     ["unzip"]].

has_command_alternatives(Alternatives) ->
    lists:all(fun has_any_command/1, Alternatives).

has_any_command(Commands) ->
    lists:any(fun has_command/1, Commands).

has_command(Cmd) ->
    os:find_executable(Cmd) =/= false.
