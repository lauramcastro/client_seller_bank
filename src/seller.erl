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

-define(PRICES, #{"Ultimo tango a Parigi" => 100,
                  "Novecento" => 25,
                  "L'ultimo imperatore" => 75,
                  "Io ballo da sola" => 50}).

-spec start()->'#client?title<String>.client!price<Int>.&(client?ok.bank!+price<Int>.<<bank.bank>>.&(bank?ok.client!date<String>.End,bank?ko.client!ko.End),client?ko.End)'.
start() ->
    start_link(?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% internals

init(_Args) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({client, title, Title}, []) ->
    timer:sleep(100), % pretend it takes a bit to look up the title
    Price = maps:get(Title, ?PRICES),
    gen_server:cast(client, {seller, price, Price}),
    {noreply, {client, Title, Price}};

handle_cast({client, ok}, {client, Title, Price}) ->
    gen_server:cast(bank, {seller, price, Price}),
    gen_server:cast(bank, {seller, start_delegation, self()}),
    {noreply, {client, Title, Price, waiting}};

handle_cast({client, ko}, {client, _Title, _Price}) ->
    {stop, normal, []};

handle_cast({bank, end_delegation}, {client, Title, Price, waiting}) ->
    {noreply, {client, Title, Price, processed}};

handle_cast({bank, ok}, {client, _Title, _Price, processed}) ->
    gen_server:cast(client, {seller, date, "22 ottobre 2022"}),
    {stop, normal, []};
handle_cast({bank, ko}, {client, _Title, _Price, processed}) ->
    gen_server:cast(client, {seller, ko}),
    {stop, normal, []};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
