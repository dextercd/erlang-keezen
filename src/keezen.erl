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
    Game = #game{board=Board,
                 players=[#player{n=N,
                                  hand=H} || {N, H} <- PlayerData],
                 deck=D2,
                 played_cards=[],
                 turn=1},
    skip_no_options(Game).

player_has_card(Player, Card) ->
    lists:member(Card, Player#player.hand).

remove_card_from_player(#game{players=Players, played_cards=Played}=Game, PlayerNumber, Card) ->
    {Left, [Player|Right]} = lists:split(PlayerNumber - 1, Players),
    Hand = Player#player.hand,
    NewHand = Hand -- [Card],
    NewPlayer = Player#player{hand=NewHand},
    Game#game{players=Left ++ [NewPlayer|Right], played_cards=[Card|Played]}.

increment_turn(#game{turn=4}=Game) ->
    Game#game{turn=1};
increment_turn(#game{turn=N}=Game) ->
    Game#game{turn=N + 1}.

give_cards(#game{players=Players, deck=Deck}=Game, Count) ->
    {NewPlayers, NewDeck} = lists:mapfoldl(fun(Player, D0) ->
                                                {Hand, D1} = lists:split(Count, D0),
                                                NewPlayer = Player#player{hand=Hand},
                                                {NewPlayer, D1}
                                           end,
                                           Deck, Players),
    Game#game{players=NewPlayers, deck=NewDeck}.

deal_cards(Game) ->
    AnyCards = lists:any(fun(#player{hand=[]})    -> false;
                            (#player{hand=[_|_]}) -> true end,
                         Game#game.players),
    case AnyCards of
        true -> Game;
        false ->
            case Game#game.deck of
                [] ->
                    PlayedCards = Game#game.played_cards,
                    NewDeck = deck:shuffle(PlayedCards),
                    give_cards(Game#game{deck=NewDeck, played_cards=[]}, 5);
                Deck ->
                    give_cards(Game, 4)
            end
    end.

discard_all_cards(#game{turn=N, played_cards=PlayedCards, players=Players}=Game) ->
    {Left, [CurrentPlayer|Right]} = lists:split(N - 1, Players),
    NewPlayer = CurrentPlayer#player{hand=[]},
    NewPlayers = Left ++ [NewPlayer|Right],
    Game#game{players=NewPlayers,
              played_cards=CurrentPlayer#player.hand ++ PlayedCards}.

get_card_actions({_, 2})     -> [{move, 2}];
get_card_actions({_, 3})     -> [{move, 3}];
get_card_actions({_, 4})     -> [{move, -4}];
get_card_actions({_, 5})     -> [{move, 5}];
get_card_actions({_, 6})     -> [{move, 6}];
get_card_actions({_, 7})     -> [{split_move, 7}];
get_card_actions({_, 8})     -> [{move, 8}];
get_card_actions({_, 9})     -> [{move, 9}];
get_card_actions({_, 10})    -> [{move, 9}];
get_card_actions({_, jack})  -> [swap];
get_card_actions({_, queen}) -> [{move, 12}];
get_card_actions({_, king})  -> [spawn];
get_card_actions({_, ace})   -> [spawn, {move, 1}].

can_play_this_turn(Game) ->
    Player = current_player(Game),
    AvailableActions = lists:flatmap(fun get_card_actions/1, Player#player.hand),
    AvailableMoves = board:move_options(Game#game.board),
    CanSpawn = pawns_left(Game, Player#player.n) > 0,
    CanSwap = false,
    CanMove = fun(N) -> false end,
    CanSplitMove = fun(N) -> false end,
    lists:any(
        fun(spawn)           -> CanSpawn;
           (swap)            -> CanSwap;
           ({move, N})       -> CanMove(N);
           ({split_move, N}) -> CanSplitMove(N)
        end,
        AvailableActions).

skip_no_options(Game) ->
    case can_play_this_turn(Game) of
        true -> Game;
        false ->
            next_turn(discard_all_cards(Game))
    end.

next_turn(G0) ->
    G1 = increment_turn(G0),
    G2 = deal_cards(G1),
    G3 = skip_no_options(G2),
    G3.

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

pawns_left(#game{board=B}, PlayerNumber) ->
    starting_pawn_count() - length(board:pawns_of_player(B, PlayerNumber)).
