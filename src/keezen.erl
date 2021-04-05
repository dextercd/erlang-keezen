-module(keezen).

-include("keezen_records.hrl").

-export([new/0]).

-type player_no() :: 1..4.
-export_type([player_no/0]).

-spec starting_pawn_count() -> pos_integer().
starting_pawn_count() -> 4.

new() ->
    InitialDeck = deck:new_shuffled(),
    Board0 = board:new(),
    % Place pawns on starting positions
    Board = lists:foldl(fun(N, B) ->
                            P = #pawn{player=N,
                                      area=board,
                                      position=board:start_position(N)},
                            {ok, B1} = board:place_pawn(B, P),
                            B1
                        end,
                        Board0,
                        lists:seq(1, 4)),

    {PlayerData, D2} = lists:mapfoldl(
                            fun(N, D) ->
                                {Hand, DRest} =  lists:split(5, D),
                                {{N, Hand}, DRest}
                            end,
                            InitialDeck, lists:seq(1,4)),
    #game{board=Board,
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
