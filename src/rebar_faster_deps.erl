-module(rebar_faster_deps).

-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    case is_supported() of
        false ->
            rebar_log:log(warn, "[rebar_faster_deps] Disabled", []),
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
    lists:all(fun(Cmd) -> os:find_executable(Cmd) =/= false end, ["wget", "unzip"]).
