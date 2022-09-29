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

init(_Args) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({seller, price, Price}, _From, []) ->
    {reply, ok, [{price, Price}]};

handle_call({client, card, CardNumber}, _From, {delegated, From, Name}) ->
    true = unregister(Name),
    true = register(Name, From),
    true = register(?MODULE, self()),
    Name ! {?MODULE, end_delegation},
    Reply = case length(CardNumber)==16 of
        true -> ok;
        false -> ko
      end,
    {reply, Reply, []}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({seller, start_delegation, Name, From}, {price, Price}) ->
    true = unregister(Name),
    true = unregister(?MODULE),
    true = register(Name, self()),
    client:pay(Name, Price),
    {noreply, {delegated, From, Name}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
