%% @author woodcol

-module(player_sup).

-behaviour(supervisor).
-export([init/1]).

%% ===========================================================
%% API functions
%% ===========================================================
-export([start_link/0,start_child/2]).

start_link() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

%% ===========================================================
%% Behavioural functions
%% ===========================================================

start_child(Socket,Account) ->
	supervisor:start_child(?MODULE,[Socket,Account]).

%% init/1 
%%=============================================================
%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
           SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
           RestartStrategy :: one_for_all
                            | one_for_one
                            | rest_for_one
                            | simple_one_for_one,
           ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
           StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
           RestartPolicy :: permanent
                          | transient
                          | temporary,
           Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    Element = {player_element,{player_element,start_link,[]},temporary,brutal_kill,worker,[player_element]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
%% ====================================================================
%% Internal functions
%% ====================================================================