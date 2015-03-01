%% @author woodcol
%% @doc @todo Add description to testMode.


-module(testMode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-compile(export_all).

-record(dotest,{count = 0}).
-record(doxx,{number,user}).
%% ====================================================================
%% Internal functions
%% ====================================================================

addData(NewD) -> 
	SS = #dotest{count = NewD},
	put(aa,SS).

readData() ->
	DD = get(aa),
	case DD of
		undefined -> undefined;
		_ -> 
			Ndo = DD#dotest.count,
			io:format("Ndo = ~p~n",[Ndo])
	end.
delData() ->
	erase(aa).

recordtest() ->
	List = [#doxx{number=1,user = "aa"},#doxx{number=2,user = "aab"},#doxx{number=3,user = "aac"},#doxx{number=4,user = "aad"},#doxx{number=5,user = "aae"}],
	lists:keyreplace(3, 2, List, #doxx{number=3, user="bbb"}).