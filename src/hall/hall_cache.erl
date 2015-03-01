%% @author woodcol

%% =============================================================================
%% Include files
%% =============================================================================
-include("server_config.hrl").
-module(hall_cache).
%% -compile(export_all).

-record(hallCount,{szpCount = ?INIT_HALL_COUNT,xqCount = ?INIT_HALL_COUNT,tqCount = ?INIT_HALL_COUNT}).
%% ===========================================================
%% API functions
%% ===========================================================
-define(HALLSAVE, ?MODULE).
-export([
		 init/0,
		 addNewHall/1,
		 addNewHall/2,
		 readCount/1,
         gotoSZPHall/2,
         gotoXQHall/2,
         gotoTQHall/2,
         exitSZPHall/2,
         exitXQHall/2,
         exitTQHall/2,
         msgFromPlay/2,
         msgFromSystem/2,
         msgToHall/2,
         systemMsgToHall/2
         ]).
%% ===============================================================
%% Internal functions
%% ================================================================
%%初始化时创建大厅,参数为创建数量

init() ->
	
	ListName = qpgame_util:creatList(?INIT_HALL_COUNT),
	case initSZP(ListName) of
		true -> SZPCount = ?INIT_HALL_COUNT;
		_ -> SZPCount = 0
	end,
	case initXQ(ListName) of
		true -> XQCount = ?INIT_HALL_COUNT;
		_ -> XQCount = 0
	end,
	case initTQ(ListName) of
		true -> TQCount = ?INIT_HALL_COUNT;
		_ -> TQCount = 0
	end,
	io:format("Count is(~p,~p,~p)",[SZPCount,XQCount,TQCount]),
	initAllCount(SZPCount,XQCount,TQCount),
	ok.
%%lists:all(Pred, List),函数表示List中的元素分别作为Pred的参数运行
initSZP(List) ->
	SZPList = [{ "szp" ++ integer_to_list(X),szp}|| X <- List],
	Predicate = fun(E) -> creatHall(E) end,
	lists:all(Predicate, SZPList).
initXQ(List) ->
	XQList  = [{"xq" ++ integer_to_list(X),xq} || X <- List],
	Predicate = fun(E) -> creatHall(E) end,
	lists:all(Predicate, XQList).
initTQ(List) ->
	TQList  = [{"tq" ++ integer_to_list(X),tq} || X <- List],
	Predicate = fun(E) -> creatHall(E) end,
	lists:all(Predicate, TQList).
%%添加新的大厅，参数为添加数量
addNewHall(Type) ->
	case Type of
		szp -> addSZPNewHall(?EVERY_ADD_HALL_COUNT);
		xq  -> addXQNewHall(?EVERY_ADD_HALL_COUNT);
		tq  -> addTQNewHall(?EVERY_ADD_HALL_COUNT)
	end.
addNewHall(Type,Count)->
	case Type of
		szp -> addSZPNewHall(Count);
		xq  -> addXQNewHall(Count);
		tq  -> addTQNewHall(Count)
	end.

addSZPNewHall(Count) ->
	OldCount = readCount(szp),
	NewNameList = [{"szp" ++ integer_to_list(X),szp} || X <- qpgame_util:creatListRange(OldCount, Count)],
	Predicate = fun(E) -> creatHall(E) end,
	lists:all(Predicate, NewNameList),
	updateCount(szp,OldCount + Count).
addXQNewHall(Count) ->
	OldCount = readCount(xq),
	NewNameList = [{"xq" ++ integer_to_list(X),xq} || X <- qpgame_util:creatListRange(OldCount, Count)],
	Predicate = fun(E) -> creatHall(E) end,
	lists:all(Predicate, NewNameList),
	updateCount(xq,OldCount + Count).
addTQNewHall(Count) ->
	OldCount = readCount(tq),
	NewNameList = [{"tq" ++ integer_to_list(X),tq} || X <- qpgame_util:creatListRange(OldCount, Count)],
	Predicate = fun(E) -> creatHall(E) end,
	lists:all(Predicate, NewNameList),
	updateCount(tq,OldCount + Count).

%%创建一个编号为HallNumber,大厅类型为Type的游戏大厅
creatHall({HallNumber,Type}) ->
	case Type of
		szp ->
			{ok,Pid} = hall_sup:start_child(HallNumber, Type),
			hall_store:insert(HallNumber, Pid),
			true;
		xq ->
			{ok,Pid} = hall_sup:start_child(HallNumber,Type),
			hall_store:insert(HallNumber, Pid),
			true;
		tq ->
			{ok,Pid} = hall_sup:start_child(HallNumber,Type),
			hall_store:insert(HallNumber, Pid),
			true
	end.

%%1001,进入三张牌大厅,正常返回ok,错误返回error进入帐号和大厅编号
gotoSZPHall([Account,SZPHallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:gotoSZPHall(Pid,([Account,SZPHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:gotoSZPHall(Pid,([Account,SZPHallNumber]))
	end.


%%1002,进入象棋大厅,正常返回ok,错误返回error进入帐号和大厅编号
gotoXQHall([Account,XQHallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:gotoXQHall(Pid,([Account,XQHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:gotoXQHall(Pid,([Account,XQHallNumber]))
	end.


%%1003,进入跳棋大厅,正常返回ok,错误返回error进入帐号和大厅编号
gotoTQHall([Account,TQHallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:gotoTQHall(Pid,([Account,TQHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:gotoTQHall(Pid,([Account,TQHallNumber]))
	end.


%%1004,退出三张牌大厅,正常返回ok,错误返回error退出帐号和大厅编号
exitSZPHall([Account,SZPHallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:exitSZPHall(Pid,([Account,SZPHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:exitSZPHall(Pid,([Account,SZPHallNumber]))
	end.


%%1005,退出象棋大厅,正常返回ok,错误返回error退出帐号和大厅编号
exitXQHall([Account,XQHallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:exitXQHall(Pid,([Account,XQHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:exitXQHall(Pid,([Account,XQHallNumber]))
	end.


%%1006,退出跳棋大厅,正常返回ok,错误返回error退出帐号和大厅编号
exitTQHall([Account,TQHallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:exitTQHall(Pid,([Account,TQHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:exitTQHall(Pid,([Account,TQHallNumber]))
	end.


%%1007,收到大厅玩家发来消息,正常返回ok,错误返回error发消息帐号,消息类型,消息,大厅编号
msgFromPlay([Account,MsgType,Msg,HallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:msgFromPlay(Pid,([Account,MsgType,Msg,HallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:msgFromPlay(Pid,([Account,MsgType,Msg,HallNumber]))
	end.


%%1008,收到系统发来消息,正常返回ok,错误返回error系统消息编号,消息类型,消息,大厅编号
msgFromSystem([SystemNumber,MsgType,Msg,HallNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:msgFromSystem(Pid,([SystemNumber,MsgType,Msg,HallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:msgFromSystem(Pid,([SystemNumber,MsgType,Msg,HallNumber]))
	end.


%%1009,向大厅分发消息,正常返回ok,错误返回error消息类型，消息,发消息帐号
msgToHall([MsgType,Msg,FromAccount],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:msgToHall(Pid,([MsgType,Msg,FromAccount]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:msgToHall(Pid,([MsgType,Msg,FromAccount]))
	end.


%%1010,向大厅分发系统消息,正常返回ok,错误返回error消息类型,消息,系统消息编号
systemMsgToHall([MsgType,Msg,FromSystemNumber],Other) ->
	case hall_store:lookup(Other) of
		{ok,Pid} -> hall_element:systemMsgToHall(Pid,([MsgType,Msg,FromSystemNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = hall_sup:start_child(Socket,Account),
			hall_element:systemMsgToHall(Pid,([MsgType,Msg,FromSystemNumber]))
	end.

initAllCount(SZPCount,XQCount,TQCount) ->
	Record = #hallCount{szpCount = SZPCount,xqCount = XQCount,tqCount = TQCount},
	put(?HALLSAVE,Record).
	

updateCount(szp,Count) ->
	OldRecord = get(?HALLSAVE),
	NewCount = OldRecord#hallCount{szpCount = Count},
	put(?HALLSAVE,NewCount);
updateCount(xq,Count) ->
	OldRecord = get(?HALLSAVE),
	NewCount = OldRecord#hallCount{xqCount = Count},
	put(?HALLSAVE,NewCount);
updateCount(tq,Count) ->
	OldRecord = get(?HALLSAVE),
	NewCount = OldRecord#hallCount{tqCount = Count},
	put(?HALLSAVE,NewCount);
updateCount(all,NewCountRecord) ->
	put(?HALLSAVE,NewCountRecord).

readCount(szp) ->
	Record = get(?HALLSAVE),
	Record#hallCount.szpCount;
readCount(xq) ->
	Record = get(?HALLSAVE),
	Record#hallCount.xqCount;
readCount(tq) ->
	Record = get(?HALLSAVE),
	Record#hallCount.tqCount;
readCount(all) ->
	Record = get(?HALLSAVE),
	[Record#hallCount.szpCount,Record#hallCount.xqCount,Record#hallCount.tqCount].