-module(hall_element).
-include("server_config.hrl").
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%% ===========================================================
%% API functions
%% ===========================================================
-export([
        start_link/2,
        gotoSZPHall/2,
        gotoXQHall/2,
        gotoTQHall/2,
        exitSZPHall/2,
        exitXQHall/2,
        exitTQHall/2,
        msgFromPlay/2,
        msgFromSystem/2,
        msgToHall/2,
        systemMsgToHall/2,
        replace/2
        ]).

%%=============================================================
%% state record
%%玩家的网络连接接口,玩家帐号,玩家所在大厅编号，所在游戏房间编号,玩家的MySQL中的数据:
%%玩家数据mysqlUserData
%%{userID,userName,userPassWord,userUDID,userEmail,sex,loginDate,phoneNumber,lastLoginDate,LoginTimes,friendTable,gameTable,userCoin,userVip,userGem}
%%{用户ID,用户名，用户密码，用户UDID,用户邮箱，性别，创建用户日期，手机号，最后一次登陆日期，登陆次数，好友列表，游戏记录表，游戏金币数，vip等级，宝石数量}
%%游戏数据mysqlGameData
%%{userID,szpLockCoin,xqLockCoin,tqLockCoin,canUsedCoin,firstLoginSZP,firstLoginXQ,firstLoginTQ,loginSZPCount,loginXQCount,loginTQCount}
%%{用户ID,三张牌锁定金币，象棋锁定金币，跳棋锁定金币，可使用金币，首次登陆三张牌，首次登陆象棋，道次登陆跳棋，登陆三张牌次数，登陆象棋次数，登陆跳棋次数}
%%玩家连接，帐号也就是邮箱，玩家所在大厅，玩家所在房间，随机生成的sission,当前游戏类型
-record(state, {hallNumber,hallGameType,playerCount = 0,roomCount,roomIDList,startGameRoomCount,startGameRoomList,outRoomPlayer = 0,outRoomPlayerList = [],inRoomPlayerCount}).


%%玩家各个游戏数据，三张牌游戏数据szpdata，象棋游戏数据xqdata，跳棋游戏数据:tqdata
%%游戏数据结构
%%{userID,winCount,loseCount,allCount,winCoin,loseCoin,game100Count,game1000,game10000,gameUserCount,gameUserMax,gameLockCoin,lastGameNumber}
%%{用户ID,胜利数，失败数，所有游戏次数，赢得金币数，输去金币数，100金币次数，1000游戏次数，10000游戏次数，游戏玩家个数，最大玩家个数，游戏锁定金币数，上次保存的游戏数据}
%%-record(gamedata,{szpdata = {0},xqdata = {0},tqdata = {0}}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
start_link(HallNumber, Type) ->
    gen_server:start_link(?MODULE, [HallNumber, Type], []).


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
init([HallNumber, Type]) ->
	room_cache:initRoom(HallNumber, Type),
    case Type of 
        ?SZP_GAMETYPE -> 
			RoomList = [HallNumber ++ ?SZPRoomName ++ integer_to_list(X) || X <- qpgame_util:creatList(?ONE_HALL_ROOM_COUNT)],
			Statex = #state{hallNumber = HallNumber,hallGameType = ?SZP_GAMETYPE,roomCount = ?ONE_HALL_ROOM_COUNT,roomIDList = RoomList},
			{ok,Statex,0};
        ?XQ_GAMETYPE  -> 
			RoomList = [HallNumber ++ ?XQRoomName ++ integer_to_list(X) || X <- qpgame_util:creatList(?ONE_HALL_ROOM_COUNT)],
			{ok, #state{hallNumber = HallNumber,hallGameType = ?XQ_GAMETYPE,roomCount = ?ONE_HALL_ROOM_COUNT,roomIDList = RoomList},0};
		?TQ_GAMETYPE  -> 
			RoomList = [HallNumber ++ ?TQRoomName ++ integer_to_list(X) || X <- qpgame_util:creatList(?ONE_HALL_ROOM_COUNT)],
			{ok, #state{hallNumber = HallNumber,hallGameType = ?TQ_GAMETYPE,roomCount = ?ONE_HALL_ROOM_COUNT,roomIDList = RoomList},0}
    end.
            


%% ====================================================================
%% 外部访问进程的call调用方法
%% ====================================================================
%%外部调用接口:1001,进入三张牌大厅,参数说明:进入帐号和大厅编号back:{reply,ok,State}
gotoSZPHall(Pid,[Account,SZPHallNumber]) ->
gen_server:call(Pid,{gotoSZPHall,[Account,SZPHallNumber]}).

%%外部调用接口:1002,进入象棋大厅,参数说明:进入帐号和大厅编号back:{reply,ok,State}
gotoXQHall(Pid,[Account,XQHallNumber]) ->
gen_server:call(Pid,{gotoXQHall,[Account,XQHallNumber]}).

%%外部调用接口:1003,进入跳棋大厅,参数说明:进入帐号和大厅编号back:{reply,ok,State}
gotoTQHall(Pid,[Account,TQHallNumber]) ->
gen_server:call(Pid,{gotoTQHall,[Account,TQHallNumber]}).

%%外部调用接口:1004,退出三张牌大厅,参数说明:退出帐号和大厅编号back:{reply,ok,State}
exitSZPHall(Pid,[Account,SZPHallNumber]) ->
	gen_server:call(Pid,{exitSZPHall,[Account,SZPHallNumber]}).

%%外部调用接口:1005,退出象棋大厅,参数说明:退出帐号和大厅编号back:{reply,ok,State}
exitXQHall(Pid,[Account,XQHallNumber]) ->
	gen_server:call(Pid,{exitXQHall,[Account,XQHallNumber]}).

%%外部调用接口:1006,退出跳棋大厅,参数说明:退出帐号和大厅编号back:{reply,ok,State}
exitTQHall(Pid,[Account,TQHallNumber]) ->
	gen_server:call(Pid,{exitTQHall,[Account,TQHallNumber]}).


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
%%gen_server回调:1001,进入三张牌大厅,参数说明:进入帐号和大厅编号back:{reply,ok,State}
handle_call({gotoSZPHall,[Account,_SZPHallNumber]},_From,State) ->
	#state{playerCount = PlayerCount,outRoomPlayer = OutRoomPlayer,outRoomPlayerList = OutRoomPlayerList} = State,
	case [X || X <- OutRoomPlayerList, X == Account] of
		[] -> 
			io:format("hall old state is:~p~n",[State]),
			NewState = State#state{playerCount = PlayerCount + 1,outRoomPlayer = OutRoomPlayer +1,outRoomPlayerList = [Account|OutRoomPlayerList]},
			io:format("hall new state is:~p~n",[NewState]),
			{reply,{ok,State#state.hallNumber,State#state.hallGameType},NewState};
		[_|_] ->
			{reply,{error,regotoHall},State}%%重复登陆了
	end;
	

%%gen_server回调:1002,进入象棋大厅,参数说明:进入帐号和大厅编号back:{reply,ok,State}
handle_call({gotoXQHall,[Account,_XQHallNumber]},_From,State) ->
	#state{playerCount = PlayerCount,outRoomPlayer = OutRoomPlayer,outRoomPlayerList = OutRoomPlayerList} = State,
	case [X || X <- OutRoomPlayerList, X == Account] of
		[] -> 
			io:format("hall old state is:~p~n",[State]),
			NewState = State#state{playerCount = PlayerCount + 1,outRoomPlayer = OutRoomPlayer +1,outRoomPlayerList = [Account|OutRoomPlayerList]},
			io:format("hall new state is:~p~n",[NewState]),
			{reply,{ok,State#state.hallNumber,State#state.hallGameType},NewState};
		[_|_] ->
			{reply,{error,regotoHall},State}%%重复登陆了
	end;

%%gen_server回调:1003,进入跳棋大厅,参数说明:进入帐号和大厅编号back:{reply,ok,State}
handle_call({gotoTQHall,[Account,_TQHallNumber]},_From,State) ->
	#state{playerCount = PlayerCount,outRoomPlayer = OutRoomPlayer,outRoomPlayerList = OutRoomPlayerList} = State,
	case [X || X <- OutRoomPlayerList, X == Account] of
		[] -> 
			io:format("hall old state is:~p~n",[State]),
			NewState = State#state{playerCount = PlayerCount + 1,outRoomPlayer = OutRoomPlayer +1,outRoomPlayerList = [Account|OutRoomPlayerList]},
			io:format("hall new state is:~p~n",[NewState]),
			{reply,{ok,State#state.hallNumber,State#state.hallGameType},NewState};
		[_|_] ->
			{reply,{error,regotoHall},State}%%重复登陆了
	end;
%%gen_server回调:1004,退出三张牌大厅,参数说明:退出帐号和大厅编号back:{reply,ok,State}
handle_call({exitSZPHall,[Account,_SZPHallNumber]},_From,State) ->
	OutOfRoomAccount = [X || X <- State#state.outRoomPlayerList,X == Account],
	case OutOfRoomAccount of
		[Account|_] -> 
			NewOutRoomUser = [X || X <- State#state.outRoomPlayerList,X /= Account],
			OldUserCount = State#state.playerCount,
			NewState = State#state{playerCount = OldUserCount,outRoomPlayerList = NewOutRoomUser},
			{reply,{ok,State#state.hallNumber},NewState};
		[] ->%%没有发现大厅中有当前玩家,
			{reply,{ok,userNotInHall},State}
	end;
		
	

%%gen_server回调:1005,退出象棋大厅,参数说明:退出帐号和大厅编号back:{reply,ok,State}
handle_call({exitXQHall,[Account,XQHallNumber]},_From,State) ->
	{reply,ok,State};

%%gen_server回调:1006,退出跳棋大厅,参数说明:退出帐号和大厅编号back:{reply,ok,State}
handle_call({exitTQHall,[Account,TQHallNumber]},_From,State) ->
	{reply,ok,State};
%%其他未知同步调用
handle_call(_Request,_From,State) ->
	Reply = ok,
	{reply,Reply,State}.



%% =====================================================================
%% 外部使用异步cast调用的接口
%% =====================================================================


%%外部调用接口:1007,收到大厅玩家发来消息,参数说明:发消息帐号,消息类型,消息,大厅编号back:{reply,ok,State}
msgFromPlay(Pid,[Account,MsgType,Msg,HallNumber]) ->
	gen_server:cast(Pid,{msgFromPlay,[Account,MsgType,Msg,HallNumber]}).

%%外部调用接口:1008,收到系统发来消息,参数说明:系统消息编号,消息类型,消息,大厅编号back:{reply,ok,State}
msgFromSystem(Pid,[SystemNumber,MsgType,Msg,HallNumber]) ->
	gen_server:cast(Pid,{msgFromSystem,[SystemNumber,MsgType,Msg,HallNumber]}).

%%外部调用接口:1009,向大厅分发消息,参数说明:消息类型，消息,发消息帐号back:{reply,ok,State}
msgToHall(Pid,[MsgType,Msg,FromAccount]) ->
	gen_server:cast(Pid,{msgToHall,[MsgType,Msg,FromAccount]}).

%%外部调用接口:1010,向大厅分发系统消息,参数说明:消息类型,消息,系统消息编号back:{reply,ok,State}
systemMsgToHall(Pid,[MsgType,Msg,FromSystemNumber]) ->
	gen_server:cast(Pid,{systemMsgToHall,[MsgType,Msg,FromSystemNumber]}).

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


%%gen_server回调:1007,收到大厅玩家发来消息,参数说明:发消息帐号,消息类型,消息,大厅编号back:{reply,ok,State}
handle_cast({msgFromPlay,[Account,MsgType,Msg,HallNumber]},State) ->
	{reply,ok,State};

%%gen_server回调:1008,收到系统发来消息,参数说明:系统消息编号,消息类型,消息,大厅编号back:{reply,ok,State}
handle_cast({msgFromSystem,[SystemNumber,MsgType,Msg,HallNumber]},State) ->
	{reply,ok,State};

%%gen_server回调:1009,向大厅分发消息,参数说明:消息类型，消息,发消息帐号back:{reply,ok,State}
handle_cast({msgToHall,[MsgType,Msg,FromAccount]},State) ->
	{reply,ok,State};

%%gen_server回调:1010,向大厅分发系统消息,参数说明:消息类型,消息,系统消息编号back:{reply,ok,State}
handle_cast({systemMsgToHall,[MsgType,Msg,FromSystemNumber]},State) ->
	{reply,ok,State};

%%通用方法：更新socket连接
handle_cast({replace,HallNumber},State) ->
	{noreply,State#state{hallNumber = HallNumber},0};

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
