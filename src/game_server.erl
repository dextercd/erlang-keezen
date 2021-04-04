-module(game_server).

-behaviour(gen_server).

-export([start_link/0, get/1]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(PlayerNum) ->
    gen_server:call(?MODULE, {get, PlayerNum}).

init(_) ->
    G = keezen:new(),
    O = keezen:move_options(G),
    {ok, {G, O}}.

handle_call({get, PlayerNum}, _From, {G, O}=State) ->
    PlayerData = keezen:get_player(G, PlayerNum),
    {reply, {PlayerData, O}, State}.

handle_cast(_Req, _State) ->
    error(notsupported).
