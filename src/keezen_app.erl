%%%-------------------------------------------------------------------
%% @doc keezen public API
%% @end
%%%-------------------------------------------------------------------

-module(keezen_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    keezen_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
