-record(pawn, {player :: keezen:player_no(),
               area :: board | finish,
               position :: non_neg_integer()}).

-record(board, {pawns :: [#pawn{}]}).

-record(player, {n :: keezen:player_no(),
                 pawns_left :: non_neg_integer(),
                 finished_pawns=0 :: non_neg_integer(),
                 hand :: [any()]}).

-record(game, {players :: [#player{}],
               board :: #board{},
               deck :: [any()],
               played_cards :: [any()],
               turn :: keezen:player_no()}).
