-module(rebar_faster_deps).

-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, rebar_state:add_resource(State, {gitcache, rebar_gitcache_resource})}.
