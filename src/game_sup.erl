-module(game_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(NAME, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?NAME}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    EventServer = #{id => event_server,
                    start => {event_server, start_link, []}},
    GameServerSpec = #{id => game_server,
                       start => {game_server, start_link, []}},
    ChildSpecs = [EventServer, GameServerSpec],
    {ok, {SupFlags, ChildSpecs}}.
