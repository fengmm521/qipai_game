-module(qpgame_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(PORT, 8009).
-define(ThreadLen,3).
%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(_Type, _StartArgs) ->
	player_store:init(),
	hall_store:init(),
	room_store:init(),
	db_api:start(),
	db_Tool:initPool(3),
    case qpgame_sup:start_link(get_serverPort(),get_threadlen()) of
		{ok, Pid} ->
			hall_cache:init(),
			{ok, Pid};
		Error ->
			Error
    end.
	

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_serverPort() ->
    case application:get_env(qipai_game, gameport) of
        undefined -> ?PORT;
        {ok, Port} -> Port
    end.

get_threadlen() ->
	case application:get_env(qipai_game, threadLen) of
		undefined -> ?ThreadLen;
		{ok,Threadlen} -> Threadlen
	end.