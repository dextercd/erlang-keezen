% Contains all board logic, but nothing wrt cards or turns.

-module(board).

-export([new/0, move_options/1, place_pawn/2, start_position/1]).

-include("keezen_records.hrl").

-type direction() :: forward | backward.

-type board_distance() :: non_neg_integer().

-type board_position() :: non_neg_integer().
-type non_normalised_board_position() :: board_position() | neg_integer().

-spec board_positions() -> pos_integer().
board_positions() -> 64.

-spec start_position(keezen:player_no()) -> board_position().
start_position(1) -> 0;
start_position(2) -> 16;
start_position(3) -> 32;
start_position(4) -> 48.

-spec normalise_position(non_normalised_board_position()) -> board_position().
normalise_position(Pos) ->
    (board_positions() + (Pos rem board_positions())) rem board_positions().

-spec end_position(keezen:player_no()) -> board_position().
end_position(P) ->
    normalise_position(start_position(P) - 1).

-spec distance_to(board_position(), board_position()) -> board_distance().
distance_to(PosA, PosB) ->
    case PosB >= PosA of
        true -> PosB - PosA;
        false -> board_positions() - (PosA - PosB)
    end.

-spec distance_to(direction(), board_position(), board_position()) -> board_distance().
distance_to(forward, PosA, PosB) ->
    distance_to(PosA, PosB);
distance_to(backward, PosA, PosB) ->
    distance_to(PosB, PosA).

-spec distance_to_end(keezen:player_no(), board_position()) -> board_distance().
distance_to_end(Player, Position) ->
    End = end_position(Player),
    distance_to(Position, End).

-spec available_finish_depth(keezen:player_no()) -> board_distance().
available_finish_depth(N) ->
    % TODO
    4.

-spec new() -> #board{}.
new() ->
    Pawns = [],
    #board{pawns=Pawns}.

-spec blockades(#board{}) -> [board_position()].
blockades(#board{pawns=Pawns}) ->
    [Pos || #pawn{player=P, area=board, position=Pos} <- Pawns,
            Pos =:= start_position(P)].

-spec place_pawn(#board{}, #pawn{}) -> {ok, #board{}} | {error, occupied}.
place_pawn(#board{pawns=ExistingPawns}=B, #pawn{area=A, position=Pos}=P) ->
    Occupied = lists:any(fun(#pawn{area=A1, position=Pos1}) ->
                            A =:= A1 andalso Pos =:= Pos1
                         end,
                         ExistingPawns),
    case Occupied of
        true ->
            {error, occupied};
        false ->
            {ok, B#board{pawns=[P|ExistingPawns]}}
    end.

furthest_possible_move(Max, Dir, Pos, Blockades) ->
    % -1, because we can not move onto a blockade
    BlockDistances = [distance_to(Dir, Pos, Block) - 1 || Block <- Blockades, Block =/= Pos],
    lists:min([Max|BlockDistances]).

pawn_movement_options(#pawn{position=Pos, player=P}, Blockades) ->
    DistanceToEnd = distance_to_end(P, Pos),
    ForwardUpperLimit = lists:min([12, DistanceToEnd]),
    MaxForward = furthest_possible_move(ForwardUpperLimit, forward, Pos, Blockades),
    ForwardPossible = case MaxForward of
        0 -> [];
        F -> [{1, F}]
    end,
    MaxBackward = -furthest_possible_move(4, backward, Pos, Blockades),
    BackToStart = -distance_to(backward, Pos, start_position(P)),
    BackwardPossible = case {MaxBackward, BackToStart} of
        { 0,  _}            -> [];
        {-1, -1}            -> [];
        { B,  0}            -> [{-1, B}];
        { B, -1}            -> [{-2, B}];
        { B,  S} when B > S -> [{-1, B}];
        { B,  B}            -> [{-1, B + 1}];
        { B,  S} when B < S -> [{-1, S + 1}, {S + 1, B}]
    end,
    BackwardPossible ++ ForwardPossible.

move_options(#board{pawns=Pawns}=Board) ->
    BlockadePositions = blockades(Board),
    [{Pawn, pawn_movement_options(Pawn, BlockadePositions)} || Pawn <- Pawns].
