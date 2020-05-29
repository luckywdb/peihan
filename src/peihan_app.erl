%%%-------------------------------------------------------------------
%% @doc peihan public API
%% @end
%%%-------------------------------------------------------------------

-module(peihan_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    start(0, 0).

start(_StartType, _StartArgs) ->
    peihan_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
