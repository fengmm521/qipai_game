%% @author woodcol

-module(room_cache).
-include("server_config.hrl").
%% ===========================================================
%% API functions
%% ===========================================================
-export([
		 initRoom/2,
         gotoRoom/2,
         playGameInRoom/2,
         playGameHuiQi/2,
         playGameHuiQiOK/2,
         playGameReady/2,
         playGameEnd/2,
         playGameEscaping/2,
         exitRoom/2,
         msgFromPlayer/2,
         msgToRoom/2,
         msgFromSystem/2,
         systemMsgToRoom/2
         ]).
%% ===============================================================
%% Internal functions
%% ================================================================


%%初始化游戏房间
initRoom(HallNumber,Type) ->
	NameList = qpgame_util:creatList(?ONE_HALL_ROOM_COUNT),
	case Type of
		?SZP_GAMETYPE -> initSZPRoom(HallNumber,NameList);
		?XQ_GAMETYPE -> initXQRoom(HallNumber,NameList);
		?TQ_GAMETYPE -> initTQRoom(HallNumber,NameList)
	end.

initSZPRoom(HallNumber,NameList) ->
	NewNameList = [{HallNumber ++ ?SZPRoomName ++ integer_to_list(X),HallNumber,?SZP_GAMETYPE} || X <- NameList],
	Predicate = fun(E) -> creatRoom(E) end,
	lists:all(Predicate, NewNameList).

initXQRoom(HallNumber,NameList) ->
	NewNameList = [{HallNumber ++ ?XQRoomName ++ integer_to_list(X),HallNumber,?XQ_GAMETYPE} || X <- NameList],
	Predicate = fun(E) -> creatRoom(E) end,
	lists:all(Predicate, NewNameList).

initTQRoom(HallNumber,NameList) ->
	NewNameList = [{HallNumber ++ ?TQRoomName ++ integer_to_list(X),HallNumber,?TQ_GAMETYPE} || X <- NameList],
	Predicate = fun(E) -> creatRoom(E) end,
	lists:all(Predicate, NewNameList).

creatRoom({RoomNumber,HallNumber,Type}) ->
	case Type of
		szp ->
			{ok,Pid} = room_sup:start_child(RoomNumber,HallNumber, Type),
			room_store:insert(RoomNumber, Pid),
			true;
		xq ->
			{ok,Pid} = room_sup:start_child(RoomNumber,HallNumber,Type),
			room_store:insert(RoomNumber, Pid),
			true;
		tq ->
			{ok,Pid} = room_sup:start_child(RoomNumber,HallNumber,Type),
			room_store:insert(RoomNumber, Pid),
			true
	end.

%%2001,玩家进入游戏房间,正常返回ok,错误返回error进入帐号，房间编号，玩家数据
gotoRoom([Account,RoomNumber,GameData],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:gotoRoom(Pid,([Account,RoomNumber,GameData]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:gotoRoom(Pid,([Account,RoomNumber,GameData]))
	end.


%%2002,玩家下注，或者玩家走棋,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
playGameInRoom([Account,RoomNumber,GameData],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:playGameInRoom(Pid,([Account,RoomNumber,GameData]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:playGameInRoom(Pid,([Account,RoomNumber,GameData]))
	end.


%%2003,玩家请求悔棋,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
playGameHuiQi([Account,RoomNumber,Msg],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:playGameHuiQi(Pid,([Account,RoomNumber,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:playGameHuiQi(Pid,([Account,RoomNumber,Msg]))
	end.


%%2004,玩家是否同意悔棋,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
playGameHuiQiOK([Account,RoomNumber,BackMsg],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:playGameHuiQiOK(Pid,([Account,RoomNumber,BackMsg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:playGameHuiQiOK(Pid,([Account,RoomNumber,BackMsg]))
	end.


%%2005,玩家准备游戏,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
playGameReady([Account,RoomNumber,UserType],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:playGameReady(Pid,([Account,RoomNumber,UserType]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:playGameReady(Pid,([Account,RoomNumber,UserType]))
	end.


%%2006,游戏结束或取消准备,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
playGameEnd([Account,RoomNumber,GameRes],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:playGameEnd(Pid,([Account,RoomNumber,GameRes]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:playGameEnd(Pid,([Account,RoomNumber,GameRes]))
	end.


%%2007,玩家非正常退出正在进行中的游戏,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
playGameEscaping([Account,RoomNumber,HallNumber],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:playGameEscaping(Pid,([Account,RoomNumber,HallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:playGameEscaping(Pid,([Account,RoomNumber,HallNumber]))
	end.


%%2008,玩家退出游戏房间,正常返回ok,错误返回error玩家帐号，房间编号，游戏数据
exitRoom([Account,RoomNumber,HallNumber],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:exitRoom(Pid,([Account,RoomNumber,HallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:exitRoom(Pid,([Account,RoomNumber,HallNumber]))
	end.


%%2009,房间收到其他玩家发来的消息,正常返回ok,错误返回error发消息帐号，消息类型，消息
msgFromPlayer([Account,MsgType,Msg],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:msgFromPlayer(Pid,([Account,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:msgFromPlayer(Pid,([Account,MsgType,Msg]))
	end.


%%2010,房间分发某玩家发来的消息,正常返回ok,错误返回error消息类型，消息，消息来源帐号
msgToRoom([MsgType,Msg,FromAccount],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:msgToRoom(Pid,([MsgType,Msg,FromAccount]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:msgToRoom(Pid,([MsgType,Msg,FromAccount]))
	end.


%%2011,房间收到系统消息,正常返回ok,错误返回error消息类型，消息，系统发送者编号
msgFromSystem([MsgType,Msg,SystemNumber],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:msgFromSystem(Pid,([MsgType,Msg,SystemNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:msgFromSystem(Pid,([MsgType,Msg,SystemNumber]))
	end.


%%2012,房间分发系统消息,正常返回ok,错误返回error消息类型，消息，系统消息发送者编号
systemMsgToRoom([MsgType,Msg,SystemNumber],Other) ->
	case room_store:lookup(Other) of
		{ok,Pid} -> room_element:systemMsgToRoom(Pid,([MsgType,Msg,SystemNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = room_sup:start_child(Socket,Account),
			room_element:systemMsgToRoom(Pid,([MsgType,Msg,SystemNumber]))
	end.


