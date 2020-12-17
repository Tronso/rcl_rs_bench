%%%-------------------------------------------------------------------
%% @doc rcl_rs_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(rcl_rs_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rcl_rs_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
