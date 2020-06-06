-module(game_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DELAY, 500). % update the game every second.
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

% Mark client as connection. The client_server will be listening for connections so won't reply to call's
handle_call({connecting, Client}, _, S = #game_server_state{connectingClients = C}) ->
  {reply, ok, S#game_server_state{connectingClients = [Client | C]}};
% Mark client as connected. The client_server will now be available for requests.
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
handle_info(update, State = #game_server_state{connectingClients = Cs, apples = A}) ->
  Clients = lists:map(fun({_, Pid, _, _}) -> Pid end, client_sup:which_children()) -- Cs,
  % Eat apple or move
  EatenApples = lists:flatmap(
    fun(C) ->
      case gen_server:call(C, {check_collide, A}) of
        [] ->
          gen_server:call(C, move),
          [];
        [Point] ->
          gen_server:call(C, grow),
          [Point];
        [Point | Ps] ->
          gen_server:call(C, grow),
          [Point | Ps]
      end
    end, Clients),
  NewApples = A -- EatenApples,
  % Check for collisions
  % TODO
  % push update to clients
  Snakes = [gen_server:call(C, snake) || C <- Clients],
  [gen_server:cast(C, {update, jsone:encode(
    #{
      snakes => Snakes,
      apples => utils:coords_to_list(NewApples)
    })}) || C <- Clients],
  {noreply, State#game_server_state{apples = NewApples}};

%% Spawn an apple
handle_info(spawn_apple, State = #game_server_state{apples = Apples, size = S}) ->
  {noreply, State#game_server_state{apples = [start_position(S) | Apples]}};

handle_info(stop, State = #game_server_state{timers = Timers}) ->
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
random(N) -> rand:uniform(N + 1) - 1.

start_position({X, Y}) -> {random(X), random(Y)}.
