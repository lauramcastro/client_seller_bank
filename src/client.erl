%%%-------------------------------------------------------------------
%% @doc client_seller_bank client process
%% @end
%%%-------------------------------------------------------------------
-module(client).
-behaviour(gen_server).

%% Custom API
-export([start/0, pay/1]).

%% GenServer API
-export([start_link/1]).
%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    start_link(?MODULE).

pay(Price) ->
    gen_server:handle_call(?MODULE, {pay, Price}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
