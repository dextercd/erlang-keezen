-module(deck).

-export([new/0, new_shuffled/0, draw/1, shuffle/1]).

-type suite() :: clubs | diamonds | hearts | spades.
-type rank() :: 2..10 | jack | queen | king | ace.
-type card() :: {suite(), rank()}.
-type deck() :: [card()].
-type nonempty_deck() :: [card(), ...].

-spec all_suites() -> [suite()].
all_suites() ->
    [clubs, diamonds, hearts, spades].

-spec all_ranks() -> [rank()].
all_ranks() ->
    [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace].

-spec new() -> deck().
new() -> [{S, R} || S <- all_suites(), R <- all_ranks()].

-spec new_shuffled() -> deck().
new_shuffled() -> shuffle(new()).

-spec shuffle(deck()) -> deck().

shuffle(Deck) ->
    shuffle(Deck, length(Deck), []).

shuffle(_, 0, Shuf) ->
    Shuf;
shuffle(Deck, Len, Shuf) ->
    {LeftHalf, [C | RightHalf]} = lists:split(rand:uniform(Len) - 1, Deck),
    shuffle(LeftHalf ++ RightHalf, Len - 1, [C|Shuf]).

-spec draw([])              -> empty;
          (nonempty_deck()) -> {card(), deck()}.

draw([]) ->
    empty;
draw([Card|Rest]) ->
    {Card, Rest}.
