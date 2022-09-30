%%%-------------------------------------------------------------------
%% @doc client_seller_bank bank process
%% @end
%%%-------------------------------------------------------------------
-module(bank).
-behaviour(gen_server).

%% Custom API
-export([start/0]).

%% GenServer API
-export([start_link/1]).
%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start()->'seller?price<integer>.seller<<.client!pay<integer>.client?card<string>.>>seller.@(seller!ok.end,seller!ko.end)'.
start() ->
    start_link(?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% internals

init(_Args) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({seller, price, Price}, []) ->
    {noreply, {price, Price}};

handle_cast({seller, start_delegation, From}, {price, Price}) ->
    true = unregister(seller),
    true = unregister(bank),
    true = register(seller, self()),
    gen_server:cast(client, {seller, pay, Price}),
    {noreply, {delegated, From, seller}};

handle_cast({client, card, CardNumber}, {delegated, From, seller}) ->
    true = unregister(seller),
    true = register(seller, From),
    true = register(?MODULE, self()),
    gen_server:cast(seller, {bank, end_delegation}),
    case length(CardNumber)==16 of
        true  -> gen_server:cast(seller, {bank, ok});
        false -> gen_server:cast(seller, {bank, ko})
      end,
    {stop, normal, []}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
