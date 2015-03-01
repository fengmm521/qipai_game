%% @author woodcol

-module(player_cache).

%% ===========================================================
%% API functions
%% ===========================================================
-export([
         gotoSZPHall/2,
         gotoRoom/2,
         changeRoom/2,
         readyGame/2,
         goGame/2,
         takingBack/2,
         okForTakingBack/2,
         escaping/2,
         noReady/2,
         gotoXQHall/2,
         gotoTQHall/2,
         exitRoom/2,
         exitHall/2,
         fromRoomMsg/2,
         fromHallMsg/2,
         fromPlayerMsg/2,
         fromFriendMsg/2,
         formSystemMsg/2,
         toRoomMsg/2,
         toHallMsg/2,
         toPlayerMsg/2,
         toFriendMsg/2,
         toSystemMsg/2,
         formAskFriend/2,
         fromOtherAsk/2,
         fromAskGongHui/2,
         fromGongHuiAsk/2,
         toAskFriend/2,
         toAskOther/2,
         toAskGongHui/2,
         toAskOtherGongHui/2,
         createAccount/2,
         createUDID/2,
         loginTest/2,
         changePaw/2,
         changeName/2,
         setPhone/2,
         deltaGem/2,
         useGem/2
         ]).
%% ===============================================================
%% Internal functions
%% ================================================================

%%4,玩家请求进入三张牌游戏大厅,正常返回ok,错误返回error游戏大厅编号
gotoSZPHall([Account,SZPHallNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			GoToSZPHallRes = player_element:gotoSZPHall(Pid,([Account,SZPHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			GoToSZPHallRes = player_element:gotoSZPHall(Pid,([Account,SZPHallNumber]))
	end,
	case GoToSZPHallRes of
		{ok,SZPHallNumber,GameRoomNumber} -> %%登陆游戏大厅成功
			case GameRoomNumber of
				null -> BackDat = "0";
				_ -> BackDat = "1"
			end,
			B = "4;1;" ++ BackDat,
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,hallNumberError} ->%%登陆游戏大厅编号错误
			B = "4;8;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gotoSZPUnknowError} ->%%登陆游戏大厅未知错误
			B = "4;9;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,regotoHall} ->%%已经登陆了游戏大厅,重复进入大厅错误
			B = "4;10;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "4;0;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.

%%5,玩家请求进入当前大厅中的某个游戏房间,正常返回ok,错误返回error游戏房音编号,playerType是玩家是观看的还是玩家，玩家为1,观看者为0
gotoRoom([Account,RoomNumber,PlayerType],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:gotoRoom(Pid,([Account,RoomNumber,PlayerType]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:gotoRoom(Pid,([Account,RoomNumber,PlayerType]))
	end,
	case Res of
		{ok,RoomNumber} -> %%进入游戏房间正常
			B = "5;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,roomNumberError} ->%%登陆游戏房间的编号错误
			B = "5;11",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,escapingRoom} ->%%玩家逃跑状态，无法进入游戏房间
			B = "5;32",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,regotoRoomUnkonwError} ->%%登陆游戏房间未知错误
			B = "5;12",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,regotoRoom} ->%%已经登陆了游戏房间,重复进入房间错误
			B = "5;13",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,roomfull} ->%%房间已满
			B = "5;14",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,noViewPlayer} ->%%当前游戏房间不允许观看
			B = "5;15",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "5;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%6,房间主设置房间信息，包括踢人，设置房间人数上限，房间名，观看者人数等,正常返回ok,错误返回error游戏房间编号，修改房间参数类型，所要修改的数据
changeRoom([RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:changeRoom(Pid,([RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:changeRoom(Pid,([RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount]))
	end,
	case Res of
		{ok,changeRoomMsg} ->%%修改房间信息成功
			B = "6;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,kickUser} ->%%房间踢人成功
			B = "6;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,roomNumberError} -> %%房间编号错误
			B = "6;16",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,kickUserError} ->%%踢人操作错误,房间用户不存在
			B = "6;17",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,chageRoomUnkonwError} ->%%修改房间信息或踢人未知错误
			B = "6;18",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,userNotVip} ->%%用户不是VIP没有踢人和修改房间信息权限
			B = "6;19",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "6;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%7,玩家准备游戏，等待游戏开始,正常返回ok,错误返回error游戏所在房间编号
readyGame([RoomNumber,Account,UserType],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} ->
			[Account,Socket|_T] = Other,
			Res = player_element:readyGame(Pid,([RoomNumber,Account,UserType]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:readyGame(Pid,([RoomNumber,Account,UserType]))
	end,
	case Res of
		{ok,UserType} ->
			B = "7;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,"3"} ->
			B = "7;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,userNotInRoom} ->%%玩家不在游戏房间,无法准备游戏
			B = "7;20",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,setUserTypeErro} ->%%玩家准备游戏未知错误
			B = "7;21",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "7;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%8,玩家下注，或者玩家走棋,正常返回ok,错误返回error下注数量或走棋的棋子和坐标
goGame([Account,GameData],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:goGame(Pid,([Account,GameData]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:goGame(Pid,([Account,GameData]))
	end,
	case Res of
		{ok,Account} ->
			B = "8;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,noCoin} ->%玩家下注金币数量不足
			B = "8;22",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gameUnknowError} ->%玩家下注或走棋未知错误
			B = "8;23",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "8;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%9,玩家请求悔棋,正常返回ok,错误返回error请求悔棋的内容
takingBack([Account,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Rsg = player_element:takingBack(Pid,([Account,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Rsg = player_element:takingBack(Pid,([Account,Msg]))
	end,
	case Rsg of
		{ok,Account} ->
			B = "9;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,_} -> %%悔棋未知错误
			B = "9;24",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,noBackRoom} ->%%本房间不许悔棋
			B = "9;25",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "9;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,5>>,<<0,3>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%10,玩家回复是否同意悔棋,正常返回ok,错误返回error是否同意悔棋的结果
okForTakingBack([Account,BackMsg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:okForTakingBack(Pid,([Account,BackMsg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:okForTakingBack(Pid,([Account,BackMsg]))
	end,
	case Res of
		{ok,Account} ->%%玩家是否同意悔棋消息成功到达
			B = "10;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,_} ->%%是否同意发送消息错误
			B = "10;26",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,unKnowHuiQi} ->%%未知错误
			B = "10;27",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "10;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%11,游戏进行中，玩家逃跑,正常返回ok,错误返回error逃跑时的游戏数据
escaping([Account,RoomNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:escaping(Pid,([Account,RoomNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:escaping(Pid,([Account,RoomNumber]))
	end,
	case Res of
		{ok,RoomNumber} ->%%从房间逃跑正常
			B = "11;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,roomNumberError} ->%%逃跑房间编号错误
			B = "11;28",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,hallNumberError} ->%%返回游戏大厅编号错误
			B = "11;29",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,userNotInRoom} ->%%玩家不在游戏房间
			B = "11;30",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,unKnowEscaping} ->%%从房间逃跑未知错误
			B = "11;31",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "11;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.

%%12,一局游戏结束，或者玩家取消准备,正常返回ok,错误返回error玩家取消准备或游戏结束相关内容
noReady([Account,RoomNumber,GameRes],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:noReady(Pid,([Account,RoomNumber,GameRes]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:noReady(Pid,([Account,RoomNumber,GameRes]))
	end,
	case Res of
		{ok,RoomNumber} ->%%从房间游戏结束
			B = "12;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,gameEndError} ->%%游戏结束服务器未知错误
			B = "12;33",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gameResDataError} ->%%客户端数据格式数误，"e"取消准备，"1"游戏获胜，"0"游戏失败
			B = "12;34",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,userNotInRoom} ->%%玩家不在游戏房间
			B = "12;30",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "11;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%13,玩家请求进入象棋游戏大厅,正常返回ok,错误返回error象棋大厅编号
gotoXQHall([Account,XQHallNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:gotoXQHall(Pid,([Account,XQHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:gotoXQHall(Pid,([Account,XQHallNumber]))
	end,
	case Res of
		{ok,XQHallNumber,GameRoomNumber} -> %%登陆游戏大厅成功
			case GameRoomNumber of
				null -> BackDat = "0";
				_ -> BackDat = "1"
			end,
			B = "13;1;" ++ BackDat,
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,hallNumberError} ->%%登陆游戏大厅编号错误
			B = "13;8;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gotoSZPUnknowError} ->%%登陆游戏大厅未知错误
			B = "13;9;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,regotoHall} ->%%已经登陆了游戏大厅,重复进入大厅错误
			B = "13;10;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,9>>,<<0,7>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "13;0;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.

%%14,玩家请求进入跳棋游戏大厅,正常返回ok,错误返回error跳棋大厅编号
gotoTQHall([Account,TQHallNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:gotoTQHall(Pid,([Account,TQHallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:gotoTQHall(Pid,([Account,TQHallNumber]))
	end,
	case Res of
		{ok,TQHallNumber,GameRoomNumber} -> %%登陆游戏大厅成功
			case GameRoomNumber of
				null -> BackDat = "0";
				_ -> BackDat = "1"
			end,
			B = "14;1;" ++ BackDat,
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,hallNumberError} ->%%登陆游戏大厅编号错误
			B = "14;8;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gotoSZPUnknowError} ->%%登陆游戏大厅未知错误
			B = "14;9;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,regotoHall} ->%%已经登陆了游戏大厅,重复进入大厅错误
			B = "14;10;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,9>>,<<0,7>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "14;0;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%15,游戏结束，玩家从游戏房间退出到游戏大厅,正常返回ok,错误返回error退出的房间编号，进入的大厅编号
exitRoom([Account,RoomNumber,HallNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:exitRoom(Pid,([Account,RoomNumber,HallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:exitRoom(Pid,([Account,RoomNumber,HallNumber]))
	end,
	case Res of
		{ok,RoomNumber,HallNumber} ->%%从房间逃跑正常
			B = "15;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,_,_} ->%%错误的房间号或错误的大厅号
			B = "15;35",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gamePlaying} ->%%游戏正在进行中，不能正常退出，但可强退，游戏金币将被扣除，对方将会收到一半你的游戏币
			B = "15;36",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,taopao} ->
			B = "15;38",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,unKnowExitRoom} ->
			B = "15;37",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "14;0;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.

%%16,玩家退出游戏大厅到过游戏主界面,正常返回ok,错误返回error退出的大厅编号
exitHall([Account,HallNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> 
			[Account,Socket|_T] = Other,
			Res = player_element:exitHall(Pid,([Account,HallNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			Res = player_element:exitHall(Pid,([Account,HallNumber]))
	end,
	case Res of
		{ok,HallNumber} ->%%从房间逃跑正常
			B = "16;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,6>>,<<0,4>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,gamePlaying} ->%%游戏正在进行中，不能正常退出，但可强退，游戏金币将被扣除，对方将会收到一半你的游戏币
			B = "16;39",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,userNotInHall} ->%%玩家不在游戏大厅中
			B = "16;45",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,unKnowExitHall} ->%%退出大厅出现其他错误
			B = "16;40",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,_} ->%%退出大厅编号错误
			B = "16;41",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{ok,_,_} ->%%错误的房间号或错误的大厅号
			B = "16;42",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,taopao} ->
			B = "16;43",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,unKnowExitRoom} ->
			B = "16;44",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "14;0;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.

%%101,玩家收到游戏房间其他玩家发来的消息,正常返回ok,错误返回error发消息的玩家帐号
fromRoomMsg([FromRoomNumber,FromAccount,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromRoomMsg(Pid,([FromRoomNumber,FromAccount,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromRoomMsg(Pid,([FromRoomNumber,FromAccount,MsgType,Msg]))
	end.


%%102,玩家收到游戏大厅其他玩家发来的消息,正常返回ok,错误返回error好友帐号，好友名，消息类型，消息内容
fromHallMsg([FromHallNumber,FromAccount,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromHallMsg(Pid,([FromHallNumber,FromAccount,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromHallMsg(Pid,([FromHallNumber,FromAccount,MsgType,Msg]))
	end.


%%103,玩家收到其他在线玩家发来的消息,正常返回ok,错误返回error发消息玩家帐号，发消息玩家名，语音类型，语音内容
fromPlayerMsg([FromAccount,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromPlayerMsg(Pid,([FromAccount,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromPlayerMsg(Pid,([FromAccount,MsgType,Msg]))
	end.


%%104,玩家收到好友发来的消息,正常返回ok,错误返回error好友帐号，好友名，语音类型，语音内容
fromFriendMsg([FromFriend,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromFriendMsg(Pid,([FromFriend,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromFriendMsg(Pid,([FromFriend,MsgType,Msg]))
	end.


%%105,玩家收到系统发来的消息,正常返回ok,错误返回error游戏大厅玩家帐号，玩家名，消息类型，消息内容
formSystemMsg({MsgType,Msg},Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:formSystemMsg(Pid,([MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:formSystemMsg(Pid,([MsgType,Msg]))
	end.


%%201,向游戏房间发送消息,消息会被房间进程分发,正常返回ok,错误返回error目标玩家帐号，消息类型，消息内容
toRoomMsg([Account,RoomNumber,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toRoomMsg(Pid,([Account,RoomNumber,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toRoomMsg(Pid,([Account,RoomNumber,MsgType,Msg]))
	end.


%%202,向游戏大厅发送消息，消息会被大厅进程分发,正常返回ok,错误返回error目标好友帐号，消息类型，消息内容
toHallMsg([Account,ToHallNumber,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toHallMsg(Pid,([Account,ToHallNumber,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toHallMsg(Pid,([Account,ToHallNumber,MsgType,Msg]))
	end.


%%203,向其他在线玩家发送消息，也就是密聊其他玩家,正常返回ok,错误返回error目标玩家帐号，语音类型，语音内容
toPlayerMsg([Account,ToAccount,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toPlayerMsg(Pid,([Account,ToAccount,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toPlayerMsg(Pid,([Account,ToAccount,MsgType,Msg]))
	end.


%%204,向好友发送消息，如果好友离线消息会被存入好友的数据库,正常返回ok,错误返回error目标好友帐号，语音类型，语音内容
toFriendMsg([Account,ToFriend,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toFriendMsg(Pid,([Account,ToFriend,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toFriendMsg(Pid,([Account,ToFriend,MsgType,Msg]))
	end.


%%205,向游戏服务器发送系统消息，本消息可实现各种功能,正常返回ok,错误返回error当前游戏房间编号号，语音类型，语音内容
toSystemMsg([Account,MsgType,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toSystemMsg(Pid,([Account,MsgType,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toSystemMsg(Pid,([Account,MsgType,Msg]))
	end.


%%301,有玩家请求加自已来好友,正常返回ok,错误返回error其他请求加自已好友的玩家帐号,玩家信息,请求表白
formAskFriend([AskAccount,PlayerMsg,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:formAskFriend(Pid,([AskAccount,PlayerMsg,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:formAskFriend(Pid,([AskAccount,PlayerMsg,Msg]))
	end.


%%302,自已的请求好友被通过或被拒绝,正常返回ok,错误返回error自已请求的目标玩家帐号,对方同意或者拒绝的理由
fromOtherAsk([AskAccount,AskMsg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromOtherAsk(Pid,([AskAccount,AskMsg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromOtherAsk(Pid,([AskAccount,AskMsg]))
	end.


%%303,有玩家请求加入公会,正常返回ok,错误返回error其他请求加入公会的玩家帐号,玩家信息,请求表白
fromAskGongHui([AskAccount,PlayerMsg,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromAskGongHui(Pid,([AskAccount,PlayerMsg,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromAskGongHui(Pid,([AskAccount,PlayerMsg,Msg]))
	end.


%%304,自已的请求加入公会得到答复,正常返回ok,错误返回error自已请求的目标公会编号,对方同意或者拒绝的理由
fromGongHuiAsk([GongHuiNumber,AskMsg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:fromGongHuiAsk(Pid,([GongHuiNumber,AskMsg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:fromGongHuiAsk(Pid,([GongHuiNumber,AskMsg]))
	end.


%%401,请求加某个玩家为好友,正常返回ok,错误返回error自已帐号，请求的目标帐号,请求表白
toAskFriend([Account,AccountAsk,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toAskFriend(Pid,([Account,AccountAsk,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toAskFriend(Pid,([Account,AccountAsk,Msg]))
	end.


%%402,返回自已是否同意加对方为好友,正常返回ok,错误返回error自已的帐号，拒绝的玩家帐号，自已拒绝的理由
toAskOther([Account,OtherAccount,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toAskOther(Pid,([Account,OtherAccount,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toAskOther(Pid,([Account,OtherAccount,Msg]))
	end.


%%403,请求加入目标公会,正常返回ok,错误返回error自已的帐号，目标公会编号，请求表白
toAskGongHui([Account,GongHuiNumber,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toAskGongHui(Pid,([Account,GongHuiNumber,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toAskGongHui(Pid,([Account,GongHuiNumber,Msg]))
	end.


%%404,公会会长返回是否同意对方加入公会,正常返回ok,错误返回error公会会长帐号，拒绝的帐号，拒绝原因
toAskOtherGongHui([Account,AccountAsk,Msg],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:toAskOtherGongHui(Pid,([Account,AccountAsk,Msg]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:toAskOtherGongHui(Pid,([Account,AccountAsk,Msg]))
	end.


%%999,玩家注册帐号,正常返回ok,错误返回error连接Socket接口，帐号，密码
createAccount([Socket,Email,UserPaw],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:createAccount(Pid,([Socket,Email,UserPaw]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:createAccount(Pid,([Socket,Email,UserPaw]))
	end.


%%998,设备码首次登陆游戏,正常返回ok,错误返回error连接Socket接口，UDID设备码
createUDID([Socket,UdidName],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:createUDID(Pid,([Socket,UdidName]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:createUDID(Pid,([Socket,UdidName]))
	end.


%%997,使用帐号登陆游戏,正常返回ok,错误返回error连接Socket接口，帐号，密码
loginTest([Name,PassWord,Socket],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> LoginRes = player_element:loginTest(Pid,([Name,PassWord,Socket]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			io:format("user not login create pid"),
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_store:insert(Account, Pid),
			LoginRes = player_element:loginTest(Pid,([Name,PassWord,Socket]))
	end,
	case LoginRes of
		{ok,login} -> %%登陆正常
			B = "997;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,loginPassword} ->%%密码错误
			B = "997;2",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,loginName} ->%%用户名不存在
			B = "997;3",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,mysqlerror} ->%%访问数据库错误
			B = "997;4",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "997;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%996,修改帐号密码,正常返回ok,错误返回error帐号，旧密码，新密码
changePaw([Account,OldPaw,NewPaw],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> ChangePawRes = player_element:changePaw(Pid,([Account,OldPaw,NewPaw])),
					[Account,Socket|_T] = Other;
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			ChangePawRes = player_element:changePaw(Pid,([Account,OldPaw,NewPaw]))
	end,
	case ChangePawRes of
		{ok,changePaw} ->%%修改密码成功
			B = "996;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,oldPawError} ->%%旧密码错误,或者新密码与旧密码相同
			B = "996;5",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,mysqlerror} ->%%访问数据库错误
			B = "996;4",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,loginName} ->%%用户名不存在
			B = "996;3",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "996;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
		
	end.

%%995,修改玩家昵称,正常返回ok,错误返回error帐号，帐号安全密码，新呢称
changeName([Email,PassWord,NewName],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> ChangeNameRes = player_element:changeName(Pid,([Email,PassWord,NewName])),
					[Account,Socket|_T] = Other;
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			ChangeNameRes = player_element:changeName(Pid,([Email,PassWord,NewName]))
	end,
	case ChangeNameRes of
		{ok,changeName} ->%%修改密码成功
			B = "995;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,nameError} ->%%旧密码错误,或者新昵称与旧昵称相同
			B = "995;6",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,mysqlerror} ->%%访问数据库错误
			B = "995;4",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,loginName} ->%%用户名不存在
			B = "995;3",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "995;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%994,设置手机号码,正常返回ok,错误返回error电话号码
setPhone([Account,PhoneNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> SetPhoneRes = player_element:setPhone(Pid,([Account,PhoneNumber])),
					[Account,Socket|_T] = Other;
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			SetPhoneRes = player_element:setPhone(Pid,([Account,PhoneNumber]))
	end,
	case SetPhoneRes of
		{ok,setPhone} ->%%修改密码成功
			B = "994;1",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,phoneError} ->%%电话号码已设置
			B = "994;7",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,mysqlerror} ->%%访问数据库错误
			B = "994;4",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,loginName} ->%%用户名不存在
			B = "994;3",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]);
		{error,_} ->%%未知错误
			B = "994;0",
			io:format("login game server back:~p~n", [B]),
			RecData = [<<0,0,0,7>>,<<0,5>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])])
	end.


%%993,玩家充值宝石,正常返回ok,错误返回error充值宝石数量，充值校检码
deltaGem([GemNumber,DeltaKey],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:deltaGem(Pid,([GemNumber,DeltaKey]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:deltaGem(Pid,([GemNumber,DeltaKey]))
	end.


%%992,玩家使用宝石兑换游戏道具,正常返回ok,错误返回error物品类型，物品数量
useGem([GoodType,GoodNumber],Other) ->
	case player_store:lookup(Other) of
		{ok,Pid} -> player_element:useGem(Pid,([GoodType,GoodNumber]));
		{error,_} ->
			[Account,Socket|_T] = Other,
			{ok,Pid} = player_sup:start_child(Socket,Account),
			player_element:useGem(Pid,([GoodType,GoodNumber]))
	end.


