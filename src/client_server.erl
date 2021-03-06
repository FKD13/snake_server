-module(client_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(client_server_state, {
  listenSocket,
  socket,
  directions = {north, undefined},
  snake = [],
  id,
  score = 0
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

init([ListenSocket]) ->
  io:format("INIT CLIENT SERVER! ~p~n", [self()]),
  {ok, #client_server_state{listenSocket = ListenSocket, id = erlang:unique_integer([positive])}}.

%% Check if the head of the snake collides with an set of points.
handle_call({check_collide, Points}, _, State = #client_server_state{snake = S, directions = {D, _}}) ->
  [Head | _] = grow(S, D),
  Collide = lists:filter(fun(A) -> A =:= Head end, Points),
  {reply, Collide, State};

%% Check if the snake is inside the field
handle_call({check_field, {MAX_X, MAX_Y}}, _, State = #client_server_state{snake = [{X, Y} | _]}) ->
  {reply, ((MAX_X > X) and (X >= 0)) and ((MAX_Y > Y) and (Y >= 0)), State};

%% Grow the snake in it's direction
handle_call(grow, _, State = #client_server_state{snake = []}) -> {reply, ok, State};
handle_call(grow, _, State = #client_server_state{directions = {D, _}, snake = S}) ->
  Snake = grow(S, D),
  {reply, ok, State#client_server_state{snake = Snake, directions = {D, D}}};

%% Move the snake in it's direction
handle_call(move, _, State = #client_server_state{snake = []}) -> {reply, ok, State};
handle_call(move, _, State = #client_server_state{directions = {D, _}, snake = S}) ->
  Snake = grow(S, D),
  {reply, ok, State#client_server_state{snake = utils:droplast(Snake), directions = {D, D}}};

%% handle death
handle_call({die, FieldSize}, _, State) ->
  {reply, ok, die(State, game_server:start_position(FieldSize))};

%% get the current snake
handle_call(snake, _, State = #client_server_state{snake = S, id = Id}) ->
  {reply, #{id => Id, snake => utils:coords_to_list(S)}, State};

%% Generic catchall
handle_call(_Request, _From, State = #client_server_state{}) ->
  {reply, ok, State}.

%% Push update to clients
handle_cast({update, _}, State = #client_server_state{socket = undefined}) -> {noreply, State};
handle_cast({update, Data}, State = #client_server_state{socket = S}) ->
  gen_tcp:send(S, Data),
  {noreply, State};

%% update the score
handle_cast({score, N}, State = #client_server_state{score = S}) when is_integer(N) ->
  {noreply, State#client_server_state{score = S + N}};

handle_cast(accept, State = #client_server_state{socket = undefined}) ->
  Pid = self(),
  GameServer = snake_server_sup:get_child(game_server),
  gen_server:call(GameServer, {connecting, Pid}),
  {ok, AcceptSocket} = gen_tcp:accept(State#client_server_state.listenSocket),
  client_sup:prepare_client(),
  Pos = gen_server:call(GameServer, start_position),
  gen_server:call(GameServer, {connected, Pid}),
  {noreply, State#client_server_state{socket = AcceptSocket, snake = [Pos]}};
handle_cast(accept, State = #client_server_state{socket = Socket}) ->
  gen_tcp:close(Socket),
  handle_cast(accept, State#client_server_state{socket = undefined});

handle_cast(_Request, State = #client_server_state{}) ->
  io:format("~p~n", [_Request]),
  {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
  io:format("~p~n", [Data]),

  Response =
    case jsone:try_decode(list_to_binary(string:trim(Data))) of
      {ok, Cmd, _} -> handle_cmd(State, Cmd);
      {error, _} -> {error, 'No valid json'}
    end,

  NState = case Response of
    {ok, NS, empty} -> NS;
    {ok, NS, Resp} -> send_response(Socket, Resp), NS;
    {error, Message} -> send_error(Socket, Message), State;
    Else -> io:format("Unexpected Response: ~p~n", [Else]), State
  end,

  listen(Socket),
  {noreply, NState};
handle_info({tcp_closed, _}, State) -> {stop, normal, State};
handle_info(_Info, State = #client_server_state{}) ->
  io:format("~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State = #client_server_state{}) ->
  ok.

% Ignoring for now
code_change(_OldVsn, State = #client_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

listen(Socket) ->
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

grow([{X, Y} | Tail], Direction) ->
  Head =
    case Direction of
      north -> {X, Y - 1};
      east -> {X + 1, Y};
      south -> {X, Y + 1};
      west -> {X - 1, Y}
    end,
  [Head | [{X, Y} | Tail]].

die(State, NewPos) ->
  State#client_server_state{
    directions = {north, undefined},
    score = 0,
    snake = [NewPos]
  }.

send_error(Socket, Error) ->
  send_response(Socket, #{error => Error}).

send_response(Socket, Message) ->
  gen_tcp:send(Socket, jsone:encode(Message)).

handle_cmd(State, #{<<"action">> := <<"info">>}) ->
  GameServer = snake_server_sup:get_child(game_server),
  {ok, State, gen_server:call(GameServer, info)};
handle_cmd(State = #client_server_state{directions = {_, Pd}}, #{<<"action">> := <<"move">>, <<"value">> := V}) ->
  Value = binary_to_list(V),
  case Value of
    "N" when Pd =/= south -> {ok, State#client_server_state{directions = {north, Pd}}, empty};
    "E" when Pd =/= west -> {ok, State#client_server_state{directions = {east, Pd}}, empty};
    "S" when Pd =/= north -> {ok, State#client_server_state{directions = {south, Pd}}, empty};
    "W" when Pd =/= east -> {ok, State#client_server_state{directions = {west, Pd}}, empty};
    "N" -> {ok, State, empty};
    "E" -> {ok, State, empty};
    "S" -> {ok, State, empty};
    "W" -> {ok, State, empty};
    _ -> {error, 'Invalid direction'}
  end;
handle_cmd(_, _) ->
  {error, 'Unexpected action'}.