-module(keezen).
-compile(export_all).

-type player_no() :: 1..4.

-record(pawn, {player :: player_no(),
               area :: board | finish,
               position :: non_neg_integer()}).

-record(player, {n :: player_no(),
                 pawns_left :: non_neg_integer(),
                 finished_pawns=0 :: non_neg_integer(),
                 hand :: [any()]}).

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

current_player(#game{players=Ps, turn=N}) ->
    lists:nth(N, Ps).

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

move_options(Game) ->
    CurrentPlayer = current_player(Game),
    ControlablePawns = controlable_pawns(Game, CurrentPlayer),
    BlockadePositions = blockades(Game),
    MaxDistance =
        [begin
            HardLimit = distance_to_end(P, Pos) + available_finish_depth(P),
            % Minus one since we can't move onto blockades
            ToBlockades = [distance_to(Pos, B) - 1 || B <- BlockadePositions
                                                    , B =/= Pos],
            lists:min([HardLimit|ToBlockades])
         end || #pawn{player=P, position=Pos} <- ControlablePawns],
    MaxDistance.
