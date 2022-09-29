%%%-------------------------------------------------------------------
%% @doc client_seller_bank client process
%% @end
%%%-------------------------------------------------------------------
-module(client).
-behaviour(gen_server).

%% Custom API
-export([start/0, pay/2]).

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

pay(Who, Price) ->
    gen_server:handle_call(?MODULE, {Who, pay, Price}).

init(_Args) ->
    seller:title(?TITLE),
    {ok, ?TITLE}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({seller, price,  Price}, _From, ?TITLE) when Price < ?LIMIT ->
    {reply, ok, {?TITLE, Price}};
handle_call({seller, price,  Price}, _From, {?TITLE, Price})  ->
    {reply, ko, []};

handle_call({seller, pay, Price}, _From, {?TITLE, Price}) ->
    case seller:card_details({client, card, ?CARD_NUMBER}) of
        {seller, date, _Date} ->
           {reply, ok, []};
        {seller, ko} ->
            {reply, ko, []}
        end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
