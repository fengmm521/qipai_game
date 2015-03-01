%% @author woodcol
%% @doc @todo Add description to qpgame_util.


-module(qpgame_util).
%% ====================================================================
%% API functions
%% ====================================================================
-export([getUUID/0,creatList/1,creatListRange/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

getUUID() ->
	ok.

%%这里用到了一个反转列表lists:reverse([1, 2, 3, 4], [a, b, c]).
creatList(MaxNumber) ->lists:reverse(listCreat(MaxNumber)).

listCreat(N) when N > 0 -> [N|listCreat(N-1)];
listCreat(0) -> [].

%%从一个数From到数To的自然数列表
creatListRange(From,To) -> [From+X || X <- creatList(To - From)].