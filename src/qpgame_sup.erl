%% @author woodcol
%% @doc @todo Add description to qpgame_sup.


-module(qpgame_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================


start_link(Port,ThreadLen) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port,ThreadLen]).

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
init([Port,ThreadLen]) ->
    Login_sup = {login_sup,{login_sup,start_link,[Port]},permanent,infinity,supervisor,[login_sup]},
	User_sup = {player_sup,{player_sup,start_link,[]},permanent,infinity,supervisor,[player_sup]},
	Hall_sup = {hall_sup,{hall_sup,start_link,[]},permanent,infinity,supervisor,[hall_sup]},
	Room_sup = {room_sup,{room_sup,start_link,[]},permanent,infinity,supervisor,[room_sup]},
	%%DB_sup = {db_sup,{db_sup,start_link,[ThreadLen]},permanent,infinity,supervisor,[db_sup]},
	Log_worker = {qpgame_log,{qpgame_log,start_link,[]},permanent,2000,worker,[]},
    {ok,{{one_for_one,10,60}, [Login_sup,User_sup,Hall_sup,Room_sup,Log_worker]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


