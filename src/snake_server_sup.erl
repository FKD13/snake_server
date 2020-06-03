%%%-------------------------------------------------------------------
%% @doc snake_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(snake_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, get_child/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional

child_spec() ->
  [
    #{id => client_sup,
      start => {client_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [client_sup]},
    #{id => game_server,
      start => {game_server, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [game_server]}%,
    %#{id => connection_server,
    %  start => {connection_server, start_link, [self()]},
    %  restart => permanent,
    %  shutdown => infinity,
    %  type => worker,
    %  modules => [connection_server]}
  ].

init([]) ->
  io:format("INIT GLOBAL SUPERVISOR!~n"),
  SupFlags = #{strategy => one_for_all,
    intensity => 0,
    period => 1},
  {ok, {SupFlags, child_spec()}}.

get_child(Id) ->
  [{_, Child, _, _}] = lists:filter(
    fun({_Id, _, _, _}) -> _Id =:= Id end,
    supervisor:which_children(?MODULE)),
  Child.

%% internal functions

