%%%-------------------------------------------------------------------
%%% @author fklinck
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(client_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(client_server_state, {
  listenSocket,
  socket,
  name,
  direction = north,
  snake = []
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

init([ListenSocket]) ->
  io:format("INIT CLIENT SERVER! ~p~n", [self()]),
  {ok, #client_server_state{listenSocket = ListenSocket}}.

handle_call(move, _, State = #client_server_state{snake = []}) -> {reply, ok, State};
handle_call(move, _, State = #client_server_state{direction = Direction, snake = [{X, Y} | Tail]}) ->
  Head =
    case Direction of
      north -> {X, Y - 1};
      east -> {X + 1, Y};
      south -> {X, Y + 1};
      west -> {X - 1, Y}
    end,
  {reply, ok, State#client_server_state{snake = [Head | droplast(Tail)]}};
handle_call(_Request, _From, State = #client_server_state{}) ->
  {reply, ok, State}.

%% Push update to clients
handle_cast(update, State = #client_server_state{socket = undefined}) -> {noreply, State};
handle_cast(update, State = #client_server_state{socket = S, snake = Snake}) ->
  gen_tcp:send(S, jsone:encode(lists:map(fun({X, Y}) -> [X, Y] end, Snake))),
  {noreply, State};

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
  NewState =
    case string:trim(Data) of
      "N" -> State#client_server_state{direction = north};
      "E" -> State#client_server_state{direction = east};
      "S" -> State#client_server_state{direction = south};
      "W" -> State#client_server_state{direction = west};
      _ ->
        gen_tcp:send(Socket, "?"),
        State
    end,
  listen(Socket),
  {noreply, NewState};
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

droplast([]) -> [];
droplast(A) -> lists:droplast(A).