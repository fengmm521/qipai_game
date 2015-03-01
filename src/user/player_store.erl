%% @author woodcol

-module(player_store).

%% ===========================================================
%% API functions
%% ===========================================================
-export([
         init/0,
         insert/2,
         delete/1,
         lookup/1
]).
-define(TABLE_ID,?MODULE).

%% ===============================================================
%% Internal functions
%% ================================================================

init() ->
     ets:new(?TABLE_ID,[public,named_table]),
     ok.

insert(Account,Pid) ->
     ets:insert(?TABLE_ID,{Account,Pid}).

lookup([Account,_Socket]) ->
	case ets:lookup(?TABLE_ID,Account) of
		[{Account,Pid}] -> {ok,Pid};
		[]          -> {error,not_found}
	end;
lookup(Account) ->
	case ets:lookup(?TABLE_ID,Account) of
		[{Account,Pid}] -> {ok,Pid};
		[]          -> {error,not_found}
	end.
delete(Pid) ->
	ets:match_delete(?TABLE_ID,{'_',Pid}).

