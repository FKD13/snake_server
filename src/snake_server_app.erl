%%%-------------------------------------------------------------------
%% @doc snake_server public API
%% @end
%%%-------------------------------------------------------------------

-module(snake_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Supervisor} = snake_server_sup:start_link(),
    client_sup:prepare_client(),
    {ok, Supervisor}.

stop(_State) ->
    ok.

%% internal functions
