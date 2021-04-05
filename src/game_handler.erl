-module(game_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("keezen_records.hrl").

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> handle_get(Req, State);
        _ -> method_not_allowed(Req, State)
    end.

rank_to_ser(Rank) when is_integer(Rank) ->
    list_to_binary(integer_to_list(Rank));
rank_to_ser(Rank) ->
    % Only integers need special handling
    Rank.

hand_to_ser(Hand) ->
    [[Suite, rank_to_ser(Rank)] || {Suite, Rank} <- Hand].

pawn_option_to_ser({#pawn{player=P, area=A, position=Pos}, Ranges}) ->
    [
        {pawn, [
            {area, A},
            {player, P},
            {position, Pos}
        ]},
        {ranges,
            Ranges
        }].

to_json({P, Movement}) ->
    jsx:encode([
        {player, [
            {pawns_left, P#player.pawns_left},
            {finished_pawns, P#player.finished_pawns},
            {hand, hand_to_ser(P#player.hand)}
        ]},
        {options, [pawn_option_to_ser(P) || P <- Movement]}]).

handle_get(Req0, State) ->
    Msg = to_json(game_server:get(1)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"no-cache">>
    }, Msg, Req0),
    {ok, Req, State}.

method_not_allowed(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<"Method not allowed.">>, Req0),
    {ok, Req, State}.
