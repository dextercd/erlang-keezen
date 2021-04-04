-module(keezen).
-compile(export_all).

-include("keezen_records.hrl").

-record(pawn, {player :: player_no(),
               area :: board | finish,
               position :: non_neg_integer()}).

-record(game, {players :: [any()],
               pawns :: [any()],
               deck :: [any()],
               played_cards :: [any()],
               turn :: player_no()}).

starting_pawn_count() -> 4.

board_positions() -> 64.

start_position(1) -> 0;
start_position(2) -> 16;
start_position(3) -> 32;
start_position(4) -> 48.

normalise_position(Pos) ->
    (board_positions() + (Pos rem 64)) rem 64.

end_position(P) ->
    normalise_position(start_position(P) - 1).

distance_to(PosA, PosB) ->
    case PosB >= PosA of
        true -> PosB - PosA;
        false -> board_positions() - distance_to(PosB, PosA)
    end.

distance_to(forward, PosA, PosB) ->
    distance_to(PosA, PosB);
distance_to(backward, PosA, PosB) ->
    distance_to(PosB, PosA).

distance_to_end(Player, Position) ->
    End = end_position(Player),
    distance_to(Position, End).

available_finish_depth(N) ->
    4.

new() ->
    InitialDeck = deck:new_shuffled(),
    {PlayerData, D2} = lists:mapfoldl(
                            fun(N, D) ->
                                {Hand, DRest} =  lists:split(5, D),
                                {{N, Hand}, DRest}
                            end,
                            InitialDeck, lists:seq(1,4)),
    #game{pawns=[#pawn{player=P, area=board, position=start_position(P)} || P <- lists:seq(1,4)],
          players=[#player{n=N,
                           pawns_left=starting_pawn_count() - 1,
                           hand=H} || {N, H} <- PlayerData],
          deck=D2,
          played_cards=[],
          turn=1}.

teamates_of(1) -> [1, 3];
teamates_of(2) -> [2, 4];
teamates_of(3) -> [1, 3];
teamates_of(4) -> [2, 4].

get_player(#game{players=Ps}, N) ->
    lists:nth(N, Ps).

current_player(#game{turn=N}=G) ->
    get_player(G, N).

pawns_of(#game{pawns=Pawns}, PlayerNs) ->
    F = fun(P) -> lists:member(P#pawn.player, PlayerNs) end,
    lists:filter(F, Pawns).

controlable_pawns(Game, Player) ->
    case Player#player.pawns_left of
        0 -> pawns_of(Game, [teamates_of(Player#player.n)]);
        _ -> pawns_of(Game, [Player#player.n])
    end.

blockades(#game{pawns=Pawns}) ->
    [Pos || #pawn{player=P, area=A, position=Pos} <- Pawns,
          A =:= board,
          Pos =:= start_position(P)].

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

move_options(Game) ->
    CurrentPlayer = current_player(Game),
    Pawns = Game#game.pawns,
    BlockadePositions = blockades(Game),
    [pawn_movement_options(Pawn, BlockadePositions) || Pawn <- Pawns].
