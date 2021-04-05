-type player_no() :: 1..4.

-record(player, {n :: player_no(),
                 pawns_left :: non_neg_integer(),
                 finished_pawns=0 :: non_neg_integer(),
                 hand :: [any()]}).

-record(pawn, {player :: player_no(),
               area :: board | finish,
               position :: non_neg_integer()}).
