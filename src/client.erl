%%%-------------------------------------------------------------------
%% @doc client_seller_bank client process
%% @end
%%%-------------------------------------------------------------------
-module(client).
-behaviour(gen_server).

%% Custom API
-export([start/0]).

%% GenServer API
-export([start_link/1]).
%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TITLE, "Novecento").
-define(LIMIT, 50).
-define(CARD_NUMBER, "0000000000000000").

-spec start()->'#seller!title<string>.seller?price<integer>.@(seller!ok.seller?pay<integer>.seller!card<string>.&(seller?date<string>.end,seller?ko.end),seller!ko.end)'.
start() ->
    start_link(?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% internals

init(_Args) ->
    gen_server:cast(seller, {client, title, ?TITLE}),
    {ok, ?TITLE}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({seller, price,  Price}, ?TITLE) when Price < ?LIMIT ->
    gen_server:cast(seller, {client, ok}),
    {noreply, {?TITLE, Price}};
handle_cast({seller, price,  _Price}, ?TITLE) ->
    gen_server:cast(seller, {client, ko}),
    {stop, normal, []};

handle_cast({seller, pay, Price}, {?TITLE, Price}) ->
    gen_server:cast(seller, {client, card, ?CARD_NUMBER}),
    {noreply, {?TITLE, Price, ?CARD_NUMBER}};

handle_cast({seller, date, _Date}, {?TITLE, _Price, ?CARD_NUMBER}) ->
   {stop, normal, []};
handle_cast({seller, ko}, {?TITLE, _Price, ?CARD_NUMBER}) ->
   {stop, normal, []}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
