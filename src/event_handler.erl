-module(event_handler).

-behaviour(cowboy_loop).

-export([init/2, info/3]).

-define(TIMEOUT, 100000).

init(Req, State) ->
    #{known := Known} = cowboy_req:match_qs([{known, int}], Req),
    case event_server:fetch_or_listen(Known) of
        {events, Events} ->
            Req1 = reply_events(200, Req, Events),
            {ok, Req1, State};
        listen ->
            start_listen(Req, State)
    end.

reply_events(Status, Req0, Events) ->
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, format_events(Events), Req0).

format_events(Events) ->
    jsx:encode(Events).

start_listen(Req, State) ->
    cowboy_req:cast({set_options, #{
        idle_timeout => ?TIMEOUT + 5000
    }}, Req),
    erlang:send_after(?TIMEOUT, self(), timeout),
    {cowboy_loop, Req, State}.

info({game_event, Events}, Req0, State) ->
    Req = reply_events(200, Req0, Events),
    {stop, Req, State};
info(timeout, Req0, State) ->
    Req = reply_events(504, Req0, []),
    {stop, Req, State}.
