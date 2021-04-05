-module(keezen).

-include("keezen_records.hrl").

-export([new/0, move_options/1, get_player/2, play_card/4]).

-type player_no() :: 1..4.
-export_type([player_no/0]).

-spec starting_pawn_count() -> pos_integer().
starting_pawn_count() -> 4.

teamates_of(1) -> [1, 3];
teamates_of(2) -> [2, 4];
teamates_of(3) -> [1, 3];
teamates_of(4) -> [2, 4].

get_player(#game{players=Ps}, N) ->
    lists:nth(N, Ps).

current_player(#game{turn=N}=G) ->
    get_player(G, N).

new() ->
    InitialDeck = deck:new_shuffled(),
    Board = board:new(),
    {PlayerData, D2} = lists:mapfoldl(
                            fun(N, D) ->
                                {Hand, DRest} =  lists:split(5, D),
                                {{N, Hand}, DRest}
                            end,
                            InitialDeck, lists:seq(1,4)),
    #game{board=Board,
          players=[#player{n=N,
                           hand=H} || {N, H} <- PlayerData],
          deck=D2,
          played_cards=[],
          turn=1}.

player_has_card(Player, Card) ->
    lists:member(Card, Player#player.hand).

remove_card_from_player(#game{players=Ps}=G, N, Card) ->
    {Left, [Player|Right]} = lists:split(N - 1, Ps),
    Hand = Player#player.hand,
    NewHand = Hand -- [Card],
    NewPlayer = Player#player{hand=NewHand},
    G#game{players=Left ++ [NewPlayer|Right]}.

play_card(#game{turn=N}=G, N, Card, Target) ->
    Player = get_player(G, N),
    case player_has_card(Player, Card) of
        false ->
            {error, no_card};
        true ->
            case apply_card_effect(G, N, Card, Target) of
                {error, E} ->
                    {error, E};
                {ok, G2} ->
                    G3 = remove_card_from_player(G2, N, Card),
                    {ok, G3}
            end
    end;
play_card(#game{turn=_}, _, _, _) ->
    {error, wrong_turn}.

apply_pawn_starter(#game{board=B1, players=Players}=G, PlayerN) ->
    Pawn = #pawn{player=PlayerN,
                 area=board,
                 position=board:start_position(PlayerN)},
    case board:place_pawn(B1, Pawn) of
        {ok, B} ->
            {ok, G#game{board=B}};
        Error ->
            Error
    end.

-spec apply_card_effect(#game{}, player_no(), deck:card(), any()) -> {ok, #game{}} | {error, any()}.
apply_card_effect(Game, PlayerN, {_, king}, {}) ->
    apply_pawn_starter(Game, PlayerN);
apply_card_effect(Game, PlayerN, {_, ace}, {}) ->
    apply_pawn_starter(Game, PlayerN);
apply_card_effect(G, _, _, _) ->
    {ok, G}.

move_options(#game{board=B}) ->
    board:move_options(B).
