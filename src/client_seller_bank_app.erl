%%%-------------------------------------------------------------------
%% @doc client_seller_bank public API
%% @end
%%%-------------------------------------------------------------------

-module(client_seller_bank_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    client_seller_bank_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
