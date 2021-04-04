-module(keezen_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, keezen, "static"}},
            {"/", cowboy_static, {priv_file, keezen, "static/index.html"}},
            {"/game", game_handler, {}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    keezen_sup:start_link().

stop(_State) ->
    ok.
