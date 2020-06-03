-module(client_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, prepare_client/0, which_children/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% specify the spec of the children I will be adding.
child_spec(ListenSocket) ->
  #{id => client_server,
    start => {client_server, start_link, [ListenSocket]},
    restart => transient,
    shutdown => infinity,
    type => worker,
    modules => [client_server]}.

init([]) ->
  io:format("INIT CHILD SUPERVISOR!~n"),
  {ok, ListenSocket} = gen_tcp:listen(8080, [{active,once}]),
  {ok, {
    #{strategy => simple_one_for_one,
      intensity => 5,
      period => 30}, [child_spec(ListenSocket)]}}.

prepare_client() ->
  {ok, Child} = supervisor:start_child(?MODULE, []),
  gen_server:cast(Child, accept),
  ok.

which_children() ->
  supervisor:which_children(?MODULE).