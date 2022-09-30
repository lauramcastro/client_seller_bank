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

-define(TITLES_OF_INTEREST, ["Ultimo tango a Parigi", "Novecento", "L'ultimo imperatore", "Io ballo da sola"]).
-define(LIMIT, 50).
-define(CARD_NUMBERS, ["0000000000000000", "123456789101112", "9876543210123456789", "9999999999999999"]).

-spec start()->'#seller!title<string>.seller?price<integer>.@(seller!ok.seller?pay<integer>.seller!card<string>.&(seller?date<string>.end,seller?ko.end),seller!ko.end)'.
start() ->
    start_link(?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% internals

init(_Args) ->
    Title = pick(?TITLES_OF_INTEREST),
    ok = gen_server:cast(seller, {client, title, Title}),
    {ok, Title}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.


handle_cast({seller, price,  Price}, Title) when Price =< ?LIMIT ->
    ok = gen_server:cast(seller, {client, ok}),
    {noreply, {Title, Price}};
handle_cast({seller, price,  _Price}, _Title) ->
    ok = gen_server:cast(seller, {client, ko}),
    {stop, normal, []};

handle_cast({seller, pay, Price}, {Title, Price}) ->
    CardNumber = pick(?CARD_NUMBERS),
    ok = gen_server:cast(seller, {client, card, CardNumber}),
    {noreply, {Title, Price, CardNumber}};

handle_cast({seller, date, _Date}, {_Title, _Price, _CardNumber}) ->
   {stop, normal, []};
handle_cast({seller, ko}, {_Title, _Price, _CardNumber}) ->
   {stop, normal, []}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% utility functions

pick(List) ->
    lists:nth(rand:uniform(length(List)), List).