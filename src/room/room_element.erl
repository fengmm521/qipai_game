-module(room_element).
-include("server_config.hrl").
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%% ===========================================================
%% API functions
%% ===========================================================
-export([
        start_link/3,
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
        systemMsgToRoom/2,
        replace/2
        ]).

%%=============================================================
%% state record
%%roomNumber:房间编号,如，hszp_1_1,表示三张牌游戏大厅1号厅中的1号房间
%%hallNumber:房间所在大厅编号,(如，hszp_1)
%%userGameList:房间中的玩家帐号列表
%%userViewList:房间中的观看者玩家帐号列表
%%gameType:游戏类型,1,三张牌，2,象棋,3,跳棋
%%userMax:房间游戏人数上限
%%userCount:房间当前游戏玩家数
%%viewMax:房间的观看者数量上限
%%viewCount:房间当前观看者人数
%%roomStats:房间状态,当前游戏状态,比如正在进行中=1，等待所有人准备=2,没有玩家=3,玩家请求悔棋=4,玩家请求退出游戏=5,
%%nowGameUser:当前游戏正在进行中，等待出牌或走棋的玩家帐号,或者等待用户确认悔棋,或者保存请求退出游戏的玩家帐号
%%userGameDataList:用户游戏数据列表，对于三张牌，这里保存用户当前扑克，对于象棋，这里保存当前玩家走棋数据，对于跳棋，这里保存当前用户走棋数据,
%%userGameTypeList:用户游戏数据类型，对于三张牌，这里保存用户是否为庄家，对于象棋，这里保存用户是否为红棋先走，对于跳棋，这里保存用户棋子颜色
%%userGamePlayCount:用户走步记数
%%userGameTimer:用户走步记时器，当这个时间到了，游戏自动进入下一个玩家状态
%%startUser:启始用户编号,这个编号指userGameList中的用户编号，当游戏开始时，先从这个编号中向后走步，直到最后，再从每一个用户开始，一直循环直到游戏结束
%%gameOverData:游戏结束数据，这个数据将会保存到mysql数据库中,同时写入所有用户游戏记录表中。
%%gameMysqlTableName:写入游戏结束记录时的mysql数据表名.这个名称会在每一局游戏开始时自动生成,当前玩家逃跑时，这个表会写入跳跑玩家的游戏数据表中,逃跑玩家在本局游戏结束之前不得进入其他游戏局开始游戏。
%%12个坐位的房间，玩家坐位安排顺序为1,7,10,5,3,12,6,9,11,4,2,8
%%roomHuiQiType,游戏房间是否允许悔棋,"1",允许，"2",不允许
-record(state, {roomNumber,hallNumber,roomName = "room",roomPassWord = null,userGameList = [],userViewList = [],gameType,userMax,userCount = 0,viewMax,viewCount = 0,roomStats =3,nowGameUser,userGameDataList= [],userGameSeatList,userGamePlayCount,userGameTimer,startUserNumber,
                gameOverData,gameMysqlTableName,roomHuiQiType = "2"
			   }).
-record(userdata,{userAccount,userCoin,userVip,userType,usedCoin = 0}).%%userType,是当前用户状态，1:为正在游戏，2:玩家取消准备，3:玩家已准备,4，玩家已逃跑,5,游戏结束，计算输赢中
-record(seat,{number,user}).
%%玩家坐位号
%%玩家各个游戏数据，三张牌游戏数据szpdata，象棋游戏数据xqdata，跳棋游戏数据:tqdata
%%游戏数据结构
%%{userID,winCount,loseCount,allCount,winCoin,loseCoin,game100Count,game1000,game10000,gameUserCount,gameUserMax,gameLockCoin,lastGameNumber}
%%{用户ID,胜利数，失败数，所有游戏次数，赢得金币数，输去金币数，100金币次数，1000游戏次数，10000游戏次数，游戏玩家个数，最大玩家个数，游戏锁定金币数，上次保存的游戏数据}
%%-record(gamedata,{szpdata = {0},xqdata = {0},tqdata = {0}}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
%%使用房间号和大厅号创建游戏房间,游戏房间创建时为空房间
start_link(RoomNumber,HallNumber,GameType) ->
    gen_server:start_link(?MODULE,[RoomNumber,HallNumber,GameType], []).


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%%创建游戏房间,房间号，大厅号，游戏类型
init([RoomNumber,HallNumber,GameType]) ->
    case GameType of 
        ?SZP_GAMETYPE -> 
			List = [1,7,10,5,3,12,6,9,11,4,2,8],
			UserSeatList = [#seat{number = X,user = null} || X <- List],
			{ok, #state{roomNumber = RoomNumber,hallNumber = HallNumber,gameType = GameType,userGameSeatList = UserSeatList},0};%%三张牌游戏房间
		?XQ_GAMETYPE -> 
			List = [1,2],
			UserSeatList = [#seat{number = X,user = null} || X <- List],
			{ok, #state{roomNumber = RoomNumber,hallNumber = HallNumber,gameType = GameType,userGameSeatList = UserSeatList},0};%%象棋游戏房间
		?TQ_GAMETYPE -> 
			List = [1,5,2,4,6,3],
			UserSeatList = [#seat{number = X,user = null} || X <- List],
			{ok, #state{roomNumber = RoomNumber,hallNumber = HallNumber,gameType = GameType,userGameSeatList = UserSeatList},0}%%跳棋游戏房间
    end.
            


%% ====================================================================
%% 外部访问进程的call调用方法
%% ====================================================================
%%外部调用接口:2001,玩家进入游戏房间,参数说明:进入帐号，房间编号，玩家数据back:{reply,ok,State}
gotoRoom(Pid,[Account,RoomNumber,GameData]) ->
gen_server:call(Pid,{gotoRoom,[Account,RoomNumber,GameData]}).

%%外部调用接口:2002,玩家下注，或者玩家走棋,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
playGameInRoom(Pid,[Account,RoomNumber,GameData]) ->
gen_server:call(Pid,{playGameInRoom,[Account,RoomNumber,GameData]}).

%%外部调用接口:2003,玩家请求悔棋,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
playGameHuiQi(Pid,[Account,RoomNumber,Msg]) ->
gen_server:call(Pid,{playGameHuiQi,[Account,RoomNumber,Msg]}).

%%外部调用接口:2004,玩家是否同意悔棋,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
playGameHuiQiOK(Pid,[Account,RoomNumber,BackMsg]) ->
gen_server:call(Pid,{playGameHuiQiOK,[Account,RoomNumber,BackMsg]}).

%%外部调用接口:2005,玩家准备游戏,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
playGameReady(Pid,[Account,RoomNumber,UserType]) ->
gen_server:call(Pid,{playGameReady,[Account,RoomNumber,UserType]}).

%% =====================================================================
%% 外部使用异步call调用的接口
%% =====================================================================
%%外部调用接口:2006,游戏结束或取消准备,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
playGameEnd(Pid,[Account,RoomNumber,GameData]) ->
	gen_server:call(Pid,{playGameEnd,[Account,RoomNumber,GameData]}).

%%外部调用接口:2007,玩家非正常退出正在进行中的游戏,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
playGameEscaping(Pid,[Account,RoomNumber,HallNumber]) ->
	gen_server:call(Pid,{playGameEscaping,[Account,RoomNumber,HallNumber]}).

%%外部调用接口:2008,玩家退出游戏房间,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
exitRoom(Pid,[Account,RoomNumber,HallNumber]) ->
	gen_server:call(Pid,{exitRoom,[Account,RoomNumber,HallNumber]}).

%%外部调用接口:2009,房间收到其他玩家发来的消息,参数说明:发消息帐号，消息类型，消息back:{reply,ok,State}
msgFromPlayer(Pid,[Account,MsgType,Msg]) ->
	gen_server:call(Pid,{msgFromPlayer,[Account,MsgType,Msg]}).

%%外部调用接口:2010,房间分发某玩家发来的消息,参数说明:消息类型，消息，消息来源帐号back:{reply,ok,State}
msgToRoom(Pid,[MsgType,Msg,FromAccount]) ->
	gen_server:call(Pid,{msgToRoom,[MsgType,Msg,FromAccount]}).

%%外部调用接口:2011,房间收到系统消息,参数说明:消息类型，消息，系统发送者编号back:{reply,ok,State}
msgFromSystem(Pid,[MsgType,Msg,SystemNumber]) ->
	gen_server:call(Pid,{msgFromSystem,[MsgType,Msg,SystemNumber]}).

%%外部调用接口:2012,房间分发系统消息,参数说明:消息类型，消息，系统消息发送者编号back:{reply,ok,State}
systemMsgToRoom(Pid,[MsgType,Msg,SystemNumber]) ->
	gen_server:call(Pid,{systemMsgToRoom,[MsgType,Msg,SystemNumber]}).


%%gen_server的call回调:=============================================================
%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%%gen_server回调:2001,玩家进入游戏房间,参数说明:进入帐号，房间编号，玩家数据back:{reply,ok,State}
handle_call({gotoRoom,[Account,_RoomNumber,GameData]},_From,State) ->
	case GameData of
		{"1",UserCoin,UserVip} -> %%游戏玩家
			UserGameList = State#state.userGameList,
			case [X || X <- UserGameList,X == Account] of
				[] -> 
					NewGameList = [Account|UserGameList],
					TmpUserDat = #userdata{userAccount = Account,userCoin = UserCoin,userVip = UserVip,userType = "2"},%%1:玩家正在进行游戏，2,玩家未准备游戏，3,玩家准备游戏，4,玩家已逃跑,5,游戏结束，计算输赢中
					OldGameDataList = State#state.userGameDataList,
					NewPlayerDataList = [TmpUserDat | OldGameDataList],
					OldCount = State#state.userCount,
					OldSeatList = State#state.userGameSeatList,
					NullSeatList = [N || #seat{number = N,user = X} <- OldSeatList, X == null],
					case NullSeatList of
						[] ->%%说明游戏房间玩家已满
							{reply,{error,roomfull},State};
						[H|_] ->%%取得第一个空坐位号
							NewSeatList = lists:keyreplace(H, 2, OldSeatList, #seat{number=H, user=Account}),
							NewState = State#state{userGameList = NewGameList,userGameDataList = NewPlayerDataList,userCount = OldCount +1,userGameSeatList = NewSeatList,roomStats = 2},
							io:format("goto room new seat:~p,state:~p~n",[H,NewState]),
							%新用户进入游戏房间成功，这里要通知房间中其他玩家
							sendMsgToAllUser(userChange,{userIn,Account},State#state.userGameList,State#state.userViewList),
							{reply,{ok,State#state.roomNumber},NewState}
					end;
					
				[_] -> {reply,{error,regotoRoom},State}
			end;
		{"0",_UserCoin,_UserVip} -> %%观看者
			UserViewList = State#state.userViewList,
			case [X || X <- UserViewList,X == Account] of
				[] -> 
					NewViewList = [Account|UserViewList],
					OldCount = State#state.viewCount,
					NewState = State#state{userViewList = NewViewList,viewCount = OldCount +1},
					io:format("goto room for view new state:~p~n",[NewState]),
					{reply,{ok,State#state.roomNumber},NewState};
				[_] -> {reply,{error,regotoRoom},State}
			end
	end;

%%gen_server回调:2002,玩家下注，或者玩家走棋,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({playGameInRoom,[Account,RoomNumber,GameData]},_From,State) ->
	case GameData of
		[GameCoin] ->
			case GameCoin of
				"p" ->%%玩家弃牌
					{reply,{ok,Account},State};
				"k" ->%%玩家请求开牌
					{reply,{ok,Account},State};
				_Any ->
					OldUsedDataList = State#state.userGameDataList,
					[#userdata{userAccount = Account,userCoin = OldUserCoin,userVip = Vip,userType = UserType,usedCoin = OldUsedCoin}|_] = [#userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N}|| #userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N} <- OldUsedDataList,X == Account ],
					NewUsedDataList = lists:keyreplace(Account, 2, OldUsedDataList, #userdata{userAccount = Account,userCoin = OldUserCoin,userVip = Vip,userType = UserType,usedCoin = OldUsedCoin + list_to_integer(GameCoin)}),
					NewState = State#state{userGameDataList = NewUsedDataList},
					io:format("roomState is:~p~n",[NewState]),
					sendMsgToAllUser(gameMsg,{userGameStep,[Account,GameCoin]},State#state.userGameList,State#state.userViewList),
					{reply,{ok,Account,GameCoin},NewState}
			end;
			
		[OldX,OldY,NewX,NewY] ->%%房间发送玩家走棋坐标给所有人
			sendMsgToAllUser(gameMsg,{userGameStep,[Account,OldX,OldY,NewX,NewY]},State#state.userGameList,State#state.userViewList),
			{reply,{ok,Account},State}
	end;
			
	

%%gen_server回调:2003,玩家请求悔棋,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({playGameHuiQi,[Account,_RoomNumber,Msg]},_From,State) ->
	case State#state.roomHuiQiType of
		"1" -> 
			sendMsgToAllUser(gameMsg,{userAskBack,Msg},State#state.userGameList,State#state.userViewList),
			{reply,{ok,Account},State};
		_ ->{reply,{error,noBackRoom},State}
	end;
			

%%gen_server回调:2004,玩家是否同意悔棋,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({playGameHuiQiOK,[Account,_RoomNumber,BackMsg]},_From,State) ->
	case BackMsg of
		"1" -> 
			sendMsgToAllUser(gameMsg,{userAskBackOk,"1"},State#state.userGameList,State#state.userViewList),%%回复同意悔棋
			{reply,{ok,Account},State};
		_ -> 
			sendMsgToAllUser(gameMsg,{userAskBackOk,"0"},State#state.userGameList,State#state.userViewList),%%回复不同意悔棋
			{reply,{ok,Account},State}
	end;

%%gen_server回调:2005,玩家准备游戏,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({playGameReady,[Account,_RoomNumber,UserType]},_From,State) ->
	GameDataList = State#state.userGameDataList,
	io:format("game data list:~p ~n",[GameDataList]),
	TmpData = [#userdata{userAccount = X,userCoin = C,userVip = V,userType = T}|| #userdata{userAccount = X,userCoin = C,userVip = V,userType = T} <- GameDataList,X == Account ],
	case TmpData of
		[UserData|_] -> 
			#userdata{userCoin = Coin,userVip = Vip} = UserData,
			NewGameDataList = lists:keyreplace(Account, 2, GameDataList, #userdata{userAccount = Account,userCoin = Coin,userVip = Vip,userType = UserType}),
			NewState = #state{userGameDataList = NewGameDataList},
			%%这里还要判断一下是否所有玩家都已经准备游戏，如果是，将要向所有玩家发送游戏开始消息，目前这一步还没有处理
			{reply,{ok,UserType},NewState};
		[] -> {reply,{error,userNotInRoom},State}
	end;
	

%%gen_server回调:2006,游戏结束或取消准备,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({playGameEnd,[Account,_RoomNumber,GameRes]},_From,State) ->
	GameDataList = State#state.userGameDataList,
	TmpData = [#userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N}|| #userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N} <- GameDataList,X == Account ],
	case TmpData of
		[UserData|_] -> 
			#userdata{userCoin = Coin,userVip = Vip,usedCoin = UsedCoin} = UserData,
			case GameRes of
				"e" ->%%玩家取消准备
					NewGameDataList = lists:keyreplace(Account, 2, GameDataList, #userdata{userAccount = Account,userCoin = Coin,userVip = Vip,userType = "2",usedCoin = 0}),
					NewState = State#state{userGameDataList = NewGameDataList},
					{reply,{ok,State#state.roomNumber},NewState};
				"1" ->%%玩家赢得了比赛金币
					NewGameDataList = lists:keyreplace(Account, 2, GameDataList, #userdata{userAccount = Account,userCoin = Coin,userVip = Vip,userType = "5",usedCoin = 0}),
					NewState = State#state{userGameDataList = NewGameDataList},
					{reply,{ok,State#state.roomNumber},NewState};
				"0" ->%%玩家输了比赛
					NewGameDataList = lists:keyreplace(Account, 2, GameDataList, #userdata{userAccount = Account,userCoin = Coin,userVip = Vip,userType = "5",usedCoin = -UsedCoin}),
					NewState = State#state{userGameDataList = NewGameDataList},
					{reply,{ok,State#state.roomNumber},NewState};
				_ ->%%客户端发送数据错误
					{reply,{error,gameResDataError},State}
			end;
		[] ->
			{reply,{error,userNotInRoom},State}
	end;
			

%%gen_server回调:2007,玩家非正常退出正在进行中的游戏,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({playGameEscaping,[Account,_RoomNumber,_HallNumber]},_From,State) ->
	GameDataList = State#state.userGameDataList,
	UserDataTmp =  [#userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N}|| #userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N} <- GameDataList,X == Account ],
	case UserDataTmp of
		[UserData|_] -> 
			#userdata{userAccount = Account,userCoin = Coin,userVip = Vip,usedCoin = UsedCoin} = UserData,
			NowCoin = Coin - UsedCoin,
			NewUserDataList = lists:keyreplace(Account, 2, GameDataList, #userdata{userAccount = Account,userCoin = Coin,userVip = Vip,userType = "4",usedCoin = UsedCoin}),
			NewState = State#state{userGameDataList = NewUserDataList},
			{reply,{ok,State#state.hallNumber,NowCoin},NewState};
		[] ->%%用户不在房间
			{reply,{error,userNotInRoom},State}
	end;
	

%%gen_server回调:2008,玩家退出游戏房间,参数说明:玩家帐号，房间编号，游戏数据back:{reply,ok,State}
handle_call({exitRoom,[Account,RoomNumber,HallNumber]},_From,State) ->
	GameDataList = State#state.userGameDataList,
	UserDataTmp =  [#userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N}|| #userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N} <- GameDataList,X == Account ],
	case UserDataTmp of
		[UserData|_] -> 
			#userdata{userAccount = Account,userCoin = Coin,userType = UserType,userVip = Vip,usedCoin = UsedCoin} = UserData,
			case UserType of
				"1" ->%%玩家正在运行游戏中，不能正常退出游戏
					{reply,{error,gamePlaying},State};
				"5" -> {reply,{error,gamePlaying},State};
				"4" -> {reply,{error,taopao},State};
				_Any ->
					NewUserDataList = [#userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N}|| #userdata{userAccount = X,userCoin = C,userVip = V,userType = T,usedCoin = N} <- GameDataList,X /= Account ],
					OldSeatList = State#state.userGameSeatList,
					NullSeatList = [N || #seat{number = N,user = X} <- OldSeatList, X == Account],
					case NullSeatList of
						[] ->%%未找到在坐位上的玩家
							{reply,{error,userNotInRoom},State};
						[H|_] ->%%清除玩家所在坐位号,并将玩家从房间列表中移除
							NewSeatList = lists:keyreplace(Account, 3, OldSeatList, #seat{number=H, user=null}),
							NewGameUserList = [X || X <- State#state.userGameList, X /= Account],
							NewState = State#state{userGameList = NewGameUserList,userGameSeatList = NewSeatList,userGameDataList = NewUserDataList},
							{reply,{ok,State#state.roomNumber,State#state.hallNumber,Coin},NewState}
					end
			end;
		[] -> %%说明玩家不在游戏列表中，将在观看者中查找，如果有只要从观看列表移除就可以了
			OldViewList = State#state.userViewList,
			UserViewL = [X || X <- OldViewList,X == Account],
			case UserViewL of
				[Account|_] ->
					NewViewList = [X || X <- OldViewList,X /= Account],
					NewState = State#state{userViewList = NewViewList},
					{reply,{ok,State#state.roomNumber,State#state.hallNumber},NewState};
				[] -> {reply,{error,userNotInRoom},State}
			end
	end;
		


%%gen_server回调:2009,房间收到其他玩家发来的消息,参数说明:发消息帐号，消息类型，消息back:{reply,ok,State}
handle_call({msgFromPlayer,[Account,MsgType,Msg]},_From,State) ->
	{reply,ok,State};

%%gen_server回调:2010,房间分发某玩家发来的消息,参数说明:消息类型，消息，消息来源帐号back:{reply,ok,State}
handle_call({msgToRoom,[MsgType,Msg,FromAccount]},_From,State) ->
	%%消息格式	sendAccount,txt,userOfRoom,all,receiveAccount,Msg ||| 消息发送者帐号，消息类型，消息种类,分类接收对象,消息接收者帐号，消息内容  |||在erlang内部消息使用原子标识，erlang与外部通信使用数字编号，如果是数据流，则写入流头部字节，消息内容在最后，可使用二进制流
	{reply,ok,State};

%%gen_server回调:2011,房间收到系统消息,参数说明:消息类型，消息，系统发送者编号back:{reply,ok,State}
handle_call({msgFromSystem,[MsgType,Msg,_SystemNumber]},_From,State) ->
	case MsgType of
		changeRoomMsg ->%改变房间信息
				[_RoomAccount,_Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount] = Msg,
				case KickAccount of
					":" ->%说明这次操作不踢人,踢人时不能更改房间信息
						case ChangeName of
							":" -> NewRoomName = State#state.roomName;
							"#" -> NewRoomName = "room";
							_ -> NewRoomName = ChangeName
			
						end,
						case MaxUser of
							":" -> NewMaxUser = State#state.userMax;%%如果新设置的房间人数少于现在的房间人数，还要踢除一部分人，这个问题，现在先不进行处理
							"#" -> 
								case State#state.gameType of
									?SZP_GAMETYPE -> NewMaxUser = 12;
									?XQ_GAMETYPE -> NewMaxUser = 2;
								?TQ_GAMETYPE -> NewMaxUser = 6
								end;
							_ -> NewMaxUser = list_to_integer(MaxUser)
						end,
						case MaxView of
							":" -> NewMaxView = State#state.viewMax;
							"#" -> NewMaxView = 12;
							_ -> NewMaxView = list_to_integer(MaxView)
						end,
						case RoomPaw of
							":" -> NewRoomPaw = State#state.roomPassWord;
							"#" -> NewRoomPaw = null;
							_ -> NewRoomPaw = RoomPaw
						end,
						NewState = State#state{roomName = NewRoomName,userMax = NewMaxUser,viewMax = NewMaxView,roomPassWord = NewRoomPaw},
						{reply,{ok,State#state.roomNumber},NewState};
					_Other -> 
						case [X || X <- State#state.userViewList,X == KickAccount] of
							[_KickUser|_] ->
								NewUserViewList = [X || X<- State#state.userViewList, X /= KickAccount],
								NewState = State#state{userViewList = NewUserViewList},
								sendMsgToAllUser(userChange,{userOut,KickAccount},State#state.userGameList,State#state.userViewList),
								{reply,{ok,kickUser},NewState};
							[] ->
								case [X || X <- State#state.userGameList,X == KickAccount] of
									[_KickUser|_] -> 
										NewGameList = [X || X <- State#state.userGameList,X /= KickAccount],
										NewUserGameDataList = [#userdata{userAccount = U,userCoin = C,userVip = V,userType = T}||#userdata{userAccount = U,userCoin = C,userVip = V,userType = T} <- State#state.userGameDataList,U /= KickAccount],
										OldSeatList = State#state.userGameSeatList,
										NullSeatList = [N || #seat{number = N,user = X} <- OldSeatList, X == KickAccount],
										[H|_] = NullSeatList,
										NewSeatList = lists:keyreplace(KickAccount, 3, OldSeatList, #seat{number=H, user=null}),%%清除座位上的玩家
										NewState = State#state{userGameList = NewGameList,userGameDataList = NewUserGameDataList,userGameSeatList = NewSeatList},
										sendMsgToAllUser(userChange,{userOut,KickAccount},State#state.userGameList,State#state.userViewList),
										{reply,{ok,kickUser},NewState};
									[] -> {reply,{error,kickUserError},State}
								end
						end
				end;
			_Any -> {reply,{ok,MsgType},State}
	end;
	

%%gen_server回调:2012,房间分发系统消息,参数说明:消息类型，消息，系统消息发送者编号back:{reply,ok,State}
handle_call({systemMsgToRoom,[MsgType,Msg,SystemNumber]},_From,State) ->
	{reply,ok,State};




%%其他未知同步调用
handle_call(_Request,_From,State) ->
	Reply = ok,
	{reply,Reply,State}.




%%通用方法：更新socket连接
replace(Pid,Socket) ->
	gen_server:cast(Pid,{replace,Socket}).




%%gen_server的cast回调:=============================================================
%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================

%%通用方法：更新socket连接
handle_cast({replace,Socket},State) ->
	{noreply,ok,0};

%%其他未知消息
handle_cast(Msg,State) ->
	{noreply,State}.


%%其他外部直接向进程发送的消息,在这里使用handle_info/2接收

%%handle_info/2%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%%同步数据接收超理处理
handle_info(timeout,State) ->
	{noreply,State};

%%其他所有未知消息,如tcp接收等
handle_info(Info,State) ->
	{noreply,State}.


%%terminate,进程终止时调用的方法%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(Reason,State) ->
	ok.


%%代码热更新
%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(OldVsn,State,Extra) ->
	{ok,State}.



%% ====================================================================
%% Internal functions内部使用的函数
%% ====================================================================
%%向所有房间玩家发送消息，也就是房间消息广播
sendMsgToAllUser(MsgType,Msg,GameList,ViewList) ->
	case MsgType of
		userChange -> %%有用户进出
			case Msg of
				{userIn,Account} -> 
					TxtMsg = "9001;wood@woodcol.com;6",
					io:format("user in room:~p,account:~p~n",[TxtMsg,Account]);
				{userOut,Account} -> ok
			end;
		userTextMsg -> %%有用户发言
			case Msg of
				{txt,Account,TxtMsg,PlayerType} ->%%用户发送文本
					case PlayerType of
						"1" ->%%房间所有人
							ok;
						"2" ->%%房间游戏玩家
							ok;
						"3" ->%%房间中的观看者
							ok;
						_Any ->%%房间中的所有人
							ok
					end;
				{imageExpre,Account,ExpNumber} ->%%用户发送图片表情
					ok;
				{soundExpre,Account,ExpNumber} ->%%用户发送语音表情
					ok;
				{sound,Account,SoundData} ->%%用户发送语音
					ok
			end;
		systemTxtMsg -> %%系统消息广播
			ok;
		gameMsg -> %%游戏消息广播,比如游戏开始，有玩家下注，有玩家准备游戏，有玩家取消准备，游戏结束等待开始
			case Msg of
				gamestart -> ok;
				{userGameStep,GameData} -> ok;
				{userAskBack,AskMsgText} -> ok;
				{userAskBackOk,Res} -> ok;
				{userReady,Account} -> ok;
				{userUnready,Account} -> ok;
				gameEnd -> ok
			end
	end.
%%消息发送给玩家,这里发送到玩家服务进程，由服务进程作一定处理，再转发给客户端,些函数不管消息类型，只负责发送给用户帐号进程,消息将由用户进程来解析
sendDataToUser(Account,{MsgType,Msg}) -> player_cache:formSystemMsg({MsgType,Msg},Account).