-module(game_server).

-behaviour(gen_server).

-export([start_link/0, get/1, play_card/3]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(PlayerNum) ->
    gen_server:call(?MODULE, {get, PlayerNum}).

play_card(PlayerNum, Card, Target) ->
    gen_server:call(?MODULE, {play_card, PlayerNum, Card, Target}).

init(_) ->
    G = keezen:new(),
    O = keezen:move_options(G),
    {ok, {G, O}}.

handle_call({get, PlayerNum}, _From, {G, O}=State) ->
    PlayerData = keezen:get_player(G, PlayerNum),
    {reply, {PlayerData, O}, State};
handle_call({play_card, PlayerNum, Card, Target}, _From, {G, O}=State) ->
    case keezen:play_card(G, PlayerNum, Card, Target) of
        {error, E} ->
            {reply, {error, E}, State};
        {ok, G2} ->
            O2 = keezen:move_options(G2),
            {reply, ok, {G2, O2}}
    end.

handle_cast(_Req, _State) ->
    error(notsupported).
