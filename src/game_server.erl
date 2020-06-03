-module(game_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DELAY, 1000). % update the game every second.
-define(APPLE_DELAY, 2000).

-record(game_server_state, {
  timers = [],
  size,
  connectingClients = [],
  apples = []
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:format("INIT GAME SERVER!~n"),
  UTimer = timer:send_interval(?DELAY, update),
  ATimer = timer:send_interval(?APPLE_DELAY, spawn_apple),
  {ok, #game_server_state{timers = [ATimer, UTimer], size = {100, 100}}}.

handle_call({connecting, Client}, _, S = #game_server_state{connectingClients = C}) ->
  {reply, ok, S#game_server_state{connectingClients = [Client|C]}};
handle_call({connected, Client}, _, S = #game_server_state{connectingClients = C}) ->
  Clients = lists:filter(fun(A) -> A =/= Client end, C),
  {reply, ok, S#game_server_state{connectingClients = Clients}};
handle_call(start_position, _, State = #game_server_state{size = S}) ->
  {reply, start_position(S), State};
handle_call(_Request, _From, State = #game_server_state{}) ->
  {noreply, State}.

handle_cast(_Request, State = #game_server_state{}) ->
  {noreply, State}.

%% Update the game
handle_info(update, State = #game_server_state{connectingClients = Cs}) ->
  Clients = lists:map(fun({_, Pid, _, _}) -> Pid end, client_sup:which_children()) -- Cs,
  [gen_server:call(C, move) || C <- Clients],
  [gen_server:cast(C, update) || C <- Clients],
  {noreply, State};

%% Spawn an apple
handle_info(spawn_apple, State = #game_server_state{apples = Apples, size = S}) ->
  {noreply, State#game_server_state{apples = [start_position(S)|Apples]}};

handle_info(stop, State = #game_server_state{ timers = Timers}) ->
  % cancel the timer
  [timer:cancel(T) || T <- Timers],
  {noreply, State};
handle_info(_Info, State = #game_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #game_server_state{}) ->
  ok.

code_change(_OldVsn, State = #game_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% return a random number between 0 and N exclusive.
random(N) -> rand:uniform(N+1)-1.

start_position({X, Y}) -> {random(X), random(Y)}.
