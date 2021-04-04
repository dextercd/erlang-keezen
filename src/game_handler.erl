-module(game_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("keezen_records.hrl").

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> handle_get(Req, State);
        _ -> method_not_allowed(Req, State)
    end.

hand_to_ser(Hand) ->
    [[Suite, Rank] || {Suite, Rank} <- Hand].

to_json({P, Movement}) ->
    jsx:encode([
        {player, [
            {pawns_left, P#player.pawns_left},
            {finished_pawns, P#player.finished_pawns},
            {hand, hand_to_ser(P#player.hand)}
        ]}]).

handle_get(Req0, State) ->
    Msg = to_json(game_server:get(1)),
    Req = cowboy_req:reply(200, #{}, Msg, Req0),
    {ok, Req, State}.

method_not_allowed(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<"Method not allowed.">>, Req0),
    {ok, Req, State}.
