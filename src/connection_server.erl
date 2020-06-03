%%%-------------------------------------------------------------------
%%% @author fklinck
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(connection_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(connection_server_state, {
  supervisor
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Supervisor) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Supervisor], []).

init([Supervisor]) ->
  io:format("INIT CONNECTION SERVER ~p !~n", [self()]),
  Pid = self(),

  {ok, ListenSocket} = gen_tcp:listen(8080, [{active,false},binary]),
  erlang:spawn_monitor(fun() -> acceptor(ListenSocket, Pid) end),

  {ok, #connection_server_state{supervisor = Supervisor}}.

handle_call(_Request, _From, State = #connection_server_state{}) ->
  io:format("CALL: ~p~n", [_Request]),
  {reply, ok, State}.

handle_cast({connect, Socket}, State) ->
  io:format("CAST: ~p~n", [{connect, Socket}]),
  % get the client supervisor
  [{client_sup, Pid, _, _}] = lists:filter(
    fun({A,_,_,_}) -> A =:= client_sup end,
    supervisor:which_children(State#connection_server_state.supervisor)
  ),

  supervisor:start_child(Pid, [Socket]),

  io:format("CAST: CHILDREN: ~p~n", [Pid]),

  {noreply, State};

handle_cast(_Request, State = #connection_server_state{}) ->
  io:format("~p~n", [_Request]),
  {noreply, State}.

handle_info(_Info, State = #connection_server_state{}) ->
  io:format("~p~n", [_Info]),
  {noreply, State}.

terminate(_Reason, _State = #connection_server_state{}) ->
  ok.

code_change(_OldVsn, State = #connection_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% accept new tcp connections
acceptor(ListenSocket, S) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  io:format("HERE~n"),
  gen_server:cast(S, {connect, AcceptSocket}),
  io:format("HERE~n"),
  acceptor(ListenSocket, S).