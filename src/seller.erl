%%%-------------------------------------------------------------------
%% @doc client_seller_bank seller process
%% @end
%%%-------------------------------------------------------------------
-module(seller).
-behaviour(gen_server).

%% Custom API
-export([start/0]).

%% GenServer API
-export([start_link/1]).
%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PRICES, #{"Novecento" => 25}).

start() ->
    start_link(?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({client, title, Title}, _From, []) ->
    Price = maps:get(Title, ?PRICES),
    {reply, {?MODULE, price, Price}, {client, Title, Price}};

handle_call({client, ok}, _From, {client, Title, Price}) ->
    bank:price(Price),
    bank ! {seller, start_delegation, seller, self()},
    {noreply, {client, Title, Price}};

handle_call({client, ko}, _From, {client, _Title, _Price}) ->
    {noreply, []};

handle_call({bank, ok}, _From, {client, _Title, _Price}) ->
    {reply, {seller, date, "22 ottobre 2022"}, []};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({bank, end_delegation}, {client, Title, Price}) ->
    {noreply, {client, Title, Price}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
