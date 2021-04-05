-module(game_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-include("keezen_records.hrl").

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> handle_get(Req, State);
        <<"POST">> -> handle_post(Req, State);
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

decode_suite(<<"clubs">>)    -> clubs;
decode_suite(<<"diamonds">>) -> diamonds;
decode_suite(<<"hearts">>)   -> hearts;
decode_suite(<<"spades">>)   -> spades.

decode_rank(<<"2">>)     -> 2;
decode_rank(<<"3">>)     -> 3;
decode_rank(<<"4">>)     -> 4;
decode_rank(<<"5">>)     -> 5;
decode_rank(<<"6">>)     -> 6;
decode_rank(<<"7">>)     -> 7;
decode_rank(<<"8">>)     -> 8;
decode_rank(<<"9">>)     -> 9;
decode_rank(<<"10">>)    -> 10;
decode_rank(<<"jack">>)  -> jack;
decode_rank(<<"queen">>) -> queen;
decode_rank(<<"king">>)  -> king;
decode_rank(<<"ace">>)   -> ace.

decode_card(Data) ->
    [Suite, Rank] = jsx:decode(Data),
    {decode_suite(Suite), decode_rank(Rank)}.

handle_post(Req0, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    Card = decode_card(Data),
    case game_server:play_card(1, Card, {}) of
        {error, E} ->
            Msg = jsx:encode([{error, E}]),
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, Msg, Req1),
            {ok, Req, State};
        ok ->
            Msg = jsx:encode([{success, true}]),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Msg, Req1),
            {ok, Req, State}
    end.

method_not_allowed(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<"Method not allowed.">>, Req0),
    {ok, Req, State}.
