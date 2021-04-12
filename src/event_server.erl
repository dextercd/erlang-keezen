-module(event_server).

-behaviour(gen_server).

-export([start_link/0, add_event/1, fetch_or_listen/1, state_nr/0]).

-export([init/1, handle_call/3, handle_cast/2]).

-define(NAME, ?MODULE).

-type event() :: term().

-record(state, {
    id :: term(),
    state_nr = 0 :: integer(),
    events = [] :: [event()],
    listeners = [] :: [pid()]
}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

-spec add_event(event()) -> ok.
add_event(Event) ->
    gen_server:cast(?NAME, {add_event, Event}).

-spec fetch_or_listen(integer()) -> {events, [event()]} | listen.
fetch_or_listen(StateNr) ->
    gen_server:call(?NAME, {fetch_or_listen, StateNr}).

-spec state_nr() -> integer().
state_nr() ->
    gen_server:call(?NAME, state_nr).

init(_) ->
    {ok, #state{id=game_event}}.

handle_cast({add_event, Event}, #state{id=Id, state_nr=Sn, events=EVs, listeners=Ls}=State) ->
    send_to_all(Id, [Event], Ls),
    {noreply, State#state{state_nr=Sn + 1, events=[Event|EVs], listeners=[]}}.

handle_call({fetch_or_listen, Their}, _, #state{state_nr=Our, events=Events}=State) when Their < Our ->
    Diff = Our - Their,
    {reply, {events, lists:reverse(lists:sublist(Events, Diff))}, State};
handle_call({fetch_or_listen, _}, {Client, _}, #state{listeners=Listeners}=State) ->
    NewState = State#state{listeners=[Client|Listeners]},
    {reply, listen, NewState};
handle_call(state_nr, _From, #state{state_nr=N}=State) ->
    {reply, N, State}.

send_to_all(_, _, []) ->
    ok;
send_to_all(Ident, Events, [L|Listeners]) ->
    L ! {Ident, Events},
    send_to_all(Ident, Events, Listeners).
