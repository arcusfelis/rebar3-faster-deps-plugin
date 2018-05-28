-module(rebar_faster_deps).

-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Locks = rebar_state:get(State, {locks, default}, []),
    Locks2 = Locks,
    State2 = rebar_state:set(State, {locks, default}, Locks2),
    io:format(user, "locks ~p", [Locks]),
    {ok, rebar_state:add_resource(State2, {gitcache, rebar_gitcache_resource})}.
