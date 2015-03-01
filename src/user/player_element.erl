-module(player_element).
-include("server_config.hrl").
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%% ===========================================================
%% API functions
%% ===========================================================
-export([
        start_link/2,
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
        useGem/2,
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
%%玩家连接，帐号也就是邮箱，玩家所在大厅，玩家所在房间，随机生成的sission,当前游戏类型,playerType = "1",默认为游戏玩家非观看者,"0"表示玩家为观看者，"2"表示玩家未进入游戏房间,但已进入游戏大厅,"3"表示玩家未进入游戏大厅和房间
%%nowUserType,当前玩家游戏状态，1，正在进行游戏，2,玩家未准备，3,玩家已准备,4,玩家逃跑中,5,游戏结束正在结算中
-record(state, {socket,account,playerHall = null,playerRoom = null,uuidRandom,nowGameType,playerType = "3",nowUserType = "2",
                userID,userName,userPassWord,userUDID,userEmail,sex,loginDate,phoneNumber,lastLoginDate,loginTimes,friendTable,gameTable,userCoin = 0,userVip = 0,userGem,
                szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,canUsedCoin = 0,firstLoginSZP,firstLoginXQ,firstLoginTQ,loginSZPCount,loginXQCount,loginTQCount,
                winCount_szp,loseCount_szp,allCount_szp,winCoin_szp,loseCoin_szp,game100Count_szp,game1000_szp,game10000_szp,gameUserCount_szp,gameUserMax_szp,gameLockCoin_szp,lastGameNumber_szp,
                winCount_xq,loseCount_xq,allCount_xq,winCoin_xq,loseCoin_xq,game100Count_xq,game1000_xq,game10000_xq,gameUserCount_xq,gameUserMax_xq,gameLockCoin_xq,lastGameNumber_xq,
                winCount_tq,loseCount_tq,allCount_tq,winCoin_tq,loseCoin_tq,game100Count_tq,game1000_tq,game10000_tq,gameUserCount_tq,gameUserMax_tq,gameLockCoin_tq,lastGameNumber_tq
                }).


%%玩家各个游戏数据，三张牌游戏数据szpdata，象棋游戏数据xqdata，跳棋游戏数据:tqdata
%%游戏数据结构
%%{userID,winCount,loseCount,allCount,winCoin,loseCoin,game100Count,game1000,game10000,gameUserCount,gameUserMax,gameLockCoin,lastGameNumber}
%%{用户ID,胜利数，失败数，所有游戏次数，赢得金币数，输去金币数，100金币次数，1000游戏次数，10000游戏次数，游戏玩家个数，最大玩家个数，游戏锁定金币数，上次保存的游戏数据}
%%-record(gamedata,{szpdata = {0},xqdata = {0},tqdata = {0}}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
start_link(Socket,Account) ->
    gen_server:start_link(?MODULE, [Socket,Account], []).


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
init([Socket,Account]) ->
    case Account of 
        firstLogin -> {ok,0};
        Account -> {ok, #state{socket = Socket,account = Account},0}
    end.
            


%% ====================================================================
%% 外部访问进程的call调用方法
%% ====================================================================
%%外部调用接口:4,玩家请求进入三张牌游戏大厅,参数说明:游戏大厅编号back:{reply,ok,State}
gotoSZPHall(Pid,[Account,SZPHallNumber]) ->
gen_server:call(Pid,{gotoSZPHall,[Account,SZPHallNumber]}).

%%外部调用接口:5,玩家请求进入当前大厅中的某个游戏房间,参数说明:游戏房音编号back:{reply,ok,State}PlayerType,玩家是观看者时为0,游戏玩家为1
gotoRoom(Pid,[Account,RoomNumber,PlayerType]) ->
gen_server:call(Pid,{gotoRoom,[Account,RoomNumber,PlayerType]}).

%%外部调用接口:6,房间主设置房间信息，包括踢人，设置房间人数上限，房间名，观看者人数等,参数说明:游戏房间编号，修改房间参数类型，所要修改的数据back:{reply,ok,State}
changeRoom(Pid,[RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount]) ->
gen_server:call(Pid,{changeRoom,[RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount]}).

%%外部调用接口:7,玩家准备游戏，等待游戏开始,参数说明:游戏所在房间编号back:{reply,ok,State}
readyGame(Pid,[RoomNumber,Account,UserType]) ->
gen_server:call(Pid,{readyGame,[RoomNumber,Account,UserType]}).

%%外部调用接口:8,玩家下注，或者玩家走棋,参数说明:下注数量或走棋的棋子和坐标back:{reply,ok,State}
goGame(Pid,[Account,GameData]) ->
gen_server:call(Pid,{goGame,[Account,GameData]}).

%%外部调用接口:9,玩家请求悔棋,参数说明:请求悔棋的内容back:{reply,ok,State}
takingBack(Pid,[Account,Msg]) ->
gen_server:call(Pid,{takingBack,[Account,Msg]}).

%%外部调用接口:10,玩家回复是否同意悔棋,参数说明:是否同意悔棋的结果back:{reply,ok,State}
okForTakingBack(Pid,[Account,BackMsg]) ->
gen_server:call(Pid,{okForTakingBack,[Account,BackMsg]}).

%%外部调用接口:11,游戏进行中，玩家逃跑,参数说明:逃跑时的游戏数据back:{reply,ok,State}
escaping(Pid,[Account,RoomNumber]) ->
	gen_server:call(Pid,{escaping,[Account,RoomNumber]}).

%%外部调用接口:12,一局游戏结束，或者玩家取消准备,参数说明:玩家取消准备或游戏结束相关内容back:{noreply,State}
noReady(Pid,[Account,RoomNumber]) ->
	gen_server:call(Pid,{noReady,[Account,RoomNumber]}).

%%外部调用接口:13,玩家请求进入象棋游戏大厅,参数说明:象棋大厅编号back:{reply,ok,State}
gotoXQHall(Pid,[Account,XQHallNumber]) ->
gen_server:call(Pid,{gotoXQHall,[Account,XQHallNumber]}).

%%外部调用接口:14,玩家请求进入跳棋游戏大厅,参数说明:跳棋大厅编号back:{reply,ok,State}
gotoTQHall(Pid,[Account,TQHallNumber]) ->
gen_server:call(Pid,{gotoTQHall,[Account,TQHallNumber]}).

%%外部调用接口:15,游戏结束，玩家从游戏房间退出到游戏大厅,参数说明:退出的房间编号，进入的大厅编号back:{reply,ok,State}
exitRoom(Pid,[Account,RoomNumber,HallNumber]) ->
gen_server:call(Pid,{exitRoom,[Account,RoomNumber,HallNumber]}).

%%外部调用接口:16,玩家退出游戏大厅到过游戏主界面,参数说明:退出的大厅编号back:{reply,ok,State}
exitHall(Pid,[Account,HallNumber]) ->
gen_server:call(Pid,{exitHall,[Account,HallNumber]}).

%%外部调用接口:999,玩家注册帐号,参数说明:连接Socket接口，帐号，密码back:{reply,ok,State}
createAccount(Pid,[Socket,Email,UserPaw]) ->
gen_server:call(Pid,{createAccount,[Socket,Email,UserPaw]}).

%%外部调用接口:998,设备码首次登陆游戏,参数说明:连接Socket接口，UDID设备码back:{reply,ok,State}
createUDID(Pid,[Socket,UdidName]) ->
gen_server:call(Pid,{createUDID,[Socket,UdidName]}).

%%外部调用接口:997,使用帐号登陆游戏,参数说明:连接Socket接口，帐号，密码back:{reply,ok,State}
loginTest(Pid,[Name,PassWord,Socket]) ->
gen_server:call(Pid,{loginTest,[Name,PassWord,Socket]}).

%%外部调用接口:996,修改帐号密码,参数说明:帐号，旧密码，新密码back:{reply,ok,State}
changePaw(Pid,[Email,OldPaw,NewPaw]) ->
gen_server:call(Pid,{changePaw,[Email,OldPaw,NewPaw]}).

%%外部调用接口:995,修改玩家昵称,参数说明:帐号，密码，新呢称back:{reply,ok,State}
changeName(Pid,[Email,ServerKey,NewName]) ->
gen_server:call(Pid,{changeName,[Email,ServerKey,NewName]}).

%%外部调用接口:994,设置手机号码,参数说明:电话号码back:{reply,ok,State}
setPhone(Pid,[Account,PhoneNumber]) ->
gen_server:call(Pid,{setPhone,[Account,PhoneNumber]}).

%%外部调用接口:993,玩家充值宝石,参数说明:充值宝石数量，充值校检码back:{reply,ok,State}
deltaGem(Pid,[GemNumber,DeltaKey]) ->
gen_server:call(Pid,{deltaGem,[GemNumber,DeltaKey]}).

%%外部调用接口:992,玩家使用宝石兑换游戏道具,参数说明:物品类型，物品数量back:{reply,ok,State}
useGem(Pid,[GoodType,GoodNumber]) ->
gen_server:call(Pid,{useGem,[GoodType,GoodNumber]}).


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
%%gen_server回调:4,玩家请求进入三张牌游戏大厅,参数说明:游戏大厅编号back:{reply,ok,State}
handle_call({gotoSZPHall,[Account,SZPHallNumber]},_From,State) ->
	case hall_cache:gotoSZPHall([Account,SZPHallNumber], SZPHallNumber) of
		{ok,SZPHallNumber,GameType} -> 
			CanUsedCoin = State#state.userCoin,
			NewState = State#state{playerHall = SZPHallNumber,nowGameType = GameType,playerType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,canUsedCoin = CanUsedCoin},
			{reply,{ok,SZPHallNumber,State#state.playerRoom},NewState};
		{ok,NumberTmp,GameTypeTmp} -> 
			io:format("hallnumberError Number:(~p),GameType:(~p)~n",[NumberTmp,GameTypeTmp]),
			{reply,{error,hallNumberError},State};
		{error,regotoHall} -> {reply,{error,regotoHall},State};
		_Any -> 
			{reply,{error,gotoSZPUnknowError},State}
	end;

%%gen_server回调:5,玩家请求进入当前大厅中的某个游戏房间,参数说明:游戏房音编号back:{reply,ok,State},PlayerType为1时表示进入为游戏玩家，为0表示进入的是观看者
handle_call({gotoRoom,[Account,RoomNumber,PlayerType]},_From,State) ->
	case State#state.nowUserType of
				"4" -> {reply,{error,escapingRoom},State};
				_   -> 
					case room_cache:gotoRoom([Account,RoomNumber,{PlayerType,State#state.userCoin,State#state.userVip}], RoomNumber) of
						{ok,RoomNumber} ->
							NewState = State#state{playerRoom = RoomNumber,playerType = PlayerType},
							{reply,{ok,RoomNumber},NewState};
						{ok,_TmpRoom} -> 
							{reply,{ok,roomNumberError},State};%%房间号错误
						{error,regotoRoom} -> 
							{reply,{ok,regotoRoom},State};%%重复进入房间
						{error,roomfull} -> {reply,{ok,roomfull},State};%%房间已满
						{error,noViewPlayer} -> {reply,{ok,noViewPlayer},State};%%房间不允许观看者
						{error,noCoin} -> {reply,{error,noCoin},State};%%游戏币不足
						_Any -> 
							{reply,{error,regotoRoomUnkonwError},State}
					end
	end;
	

%%gen_server回调:6,vip设置房间信息，包括踢人，设置房间人数上限，房间名，观看者人数等,参数说明:游戏房间编号，修改房间参数类型，所要修改的数据back:{reply,ok,State}
handle_call({changeRoom,[RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount]},_From,State) ->
	case State#state.userVip > 0 of
		true -> 
			case room_cache:msgFromSystem(changeRoomMsg,[RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount],vip, RoomAccount) of
				{ok,RoomAccount} -> {reply,{ok,changeRoomMsg},State};
				{ok,kickUser} -> {reply,{ok,kickUser},State};
				{ok,_TmpRoom} -> {reply,{error,roomNumberError},State};%%房间号错误
				{error,kickUserError} -> {reply,{error,kickUserError},State};
				_Any -> {reply,{error,chageRoomUnkonwError},State}
			end;
		_ -> {reply,{error,userNotVip},State}
	end;
	

%%gen_server回调:7,玩家准备游戏，等待游戏开始,参数说明:游戏所在房间编号back:{reply,ok,State}
handle_call({readyGame,[RoomNumber,Account,UserType]},_From,State) ->
	case room_cache:playGameReady([Account,RoomNumber,UserType],RoomNumber) of
		{ok,UserType} ->
			NewState = State#state{nowUserType = UserType},
			{reply,{ok,UserType},NewState};
		{error,userNotInRoom} ->
			NewState = State#state{nowUserType = "2"},
			{reply,{error,userNotInRoom},NewState};
		{error,_} ->
			{reply,{error,setUserTypeErro},State}
	end;
	

%%gen_server回调:8,玩家下注，或者玩家走棋,参数说明:下注数量或走棋的棋子和坐标back:{reply,ok,State}
handle_call({goGame,[Account,GameData]},_From,State) ->
	RoomNumber = State#state.playerRoom,
	case room_cache:playGameInRoom([Account,RoomNumber,GameData],RoomNumber) of
		{ok,Account,UseCoin} ->
			NewLockCoin = State#state.szpLockCoin + list_to_integer(UseCoin),%%玩家在三张牌中下注金币数,玩家当前游戏币数量为,用户所有游戏币减去已下注金币
			NewState = State#state{szpLockCoin = NewLockCoin},
			{reply,{ok,Account},NewState};
		{ok,Account} ->
			{reply,{ok,Account},State}
	end;

%%gen_server回调:9,玩家请求悔棋,参数说明:请求悔棋的内容back:{reply,ok,State}
handle_call({takingBack,[Account,Msg]},_From,State) ->
	RoomNumber = State#state.playerRoom,
	case room_cache:playGameHuiQi([Account,RoomNumber,Msg], RoomNumber) of
		{ok,Account} ->{reply,{ok,Account},State};
		{error,noBackRoom} -> {reply,{error,noBackRoom},State}
	end;
	

%%gen_server回调:10,玩家回复是否同意悔棋,参数说明:是否同意悔棋的结果back:{reply,ok,State}
handle_call({okForTakingBack,[Account,BackMsg]},_From,State) ->
	RoomNumber = State#state.playerRoom,
	case room_cache:playGameHuiQiOK([Account,RoomNumber,BackMsg], RoomNumber) of
		{ok,Account} -> {reply,{ok,Account},State};
		{ok,X} -> {reply,{ok,X},State};
		{error,_} -> {reply,{error,unKnowHuiQi},State}	
	end;
	
%%gen_server回调:11,游戏进行中，玩家逃跑,参数说明:逃跑时的游戏数据back:{reply,ok,State}
handle_call({escaping,[Account,RoomNumber]},_From,State) ->
	case State#state.playerRoom of
		RoomNumber -> 
			HallNumber = State#state.playerHall,
			case room_cache:playGameEscaping([Account,RoomNumber,HallNumber], RoomNumber) of
				{ok,HallNumber,NowCoin} -> 
					NewState = State#state{playerRoom = null,userCoin = NowCoin,szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,nowUserType = "4"},
					{reply,{ok,RoomNumber},NewState};
				{ok,_,_} ->
					{reply,{error,hallNumberError},State};
				{error,userNotInRoom} ->
					{reply,{error,userNotInRoom},State};
				{error,_} ->
					{reply,{error,unKnowEscaping},State}
			end;
		_Any ->
			{reply,{error,roomNumberError},State}
	end;
				
	

%%gen_server回调:12,一局游戏结束，或者玩家取消准备,参数说明:玩家取消准备或游戏结束相关内容back:{noreply,State}
handle_call({noReady,[Account,RoomNumber,GameRes]},_From,State) ->
	RoomNumberX = State#state.playerRoom,
	case room_cache:playGameEnd([Account,RoomNumber,GameRes],RoomNumber) of
		{ok,RoomNumber} ->%%正常退出游戏
			{reply,{ok,RoomNumberX},State};
		{error,gameResDataError} ->%%客户端数据格式错误
			{reply,{error,gameResDataError},State};
		{error,userNotInRoom} ->%%用户不在游戏房间中
					{reply,{error,userNotInRoom},State};
		{error,_} -> {reply,{ok,gameEndError},State}
	end;

%%gen_server回调:13,玩家请求进入象棋游戏大厅,参数说明:象棋大厅编号back:{reply,ok,State}
handle_call({gotoXQHall,[Account,XQHallNumber]},_From,State) ->
	case hall_cache:gotoXQHall([Account,XQHallNumber], XQHallNumber) of
		{ok,XQHallNumber,GameType} -> 
			CanUsedCoin = State#state.userCoin,
			NewState = State#state{playerHall = XQHallNumber,playerType = "2",nowGameType = GameType, szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,canUsedCoin = CanUsedCoin},
			{reply,{ok,XQHallNumber,State#state.playerRoom},NewState};
		{ok,NumberTmp,GameTypeTmp} -> 
			io:format("hallnumberError Number:(~p),GameType:(~p)~n",[NumberTmp,GameTypeTmp]),
			{reply,{error,hallNumberError},State};
		{error,regotoHall} -> {reply,{error,regotoHall},State};
		_Any -> 
			{reply,{error,gotoSZPUnknowError},State}
	end;

%%gen_server回调:14,玩家请求进入跳棋游戏大厅,参数说明:跳棋大厅编号back:{reply,ok,State}
handle_call({gotoTQHall,[Account,TQHallNumber]},_From,State) ->
	case hall_cache:gotoXQHall([Account,TQHallNumber], TQHallNumber) of
		{ok,TQHallNumber,GameType} -> 
			CanUsedCoin = State#state.userCoin,
			NewState = State#state{playerHall = TQHallNumber,playerType = "2",nowGameType = GameType, szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,canUsedCoin = CanUsedCoin},
			{reply,{ok,TQHallNumber,State#state.playerRoom},NewState};
		{ok,NumberTmp,GameTypeTmp} -> 
			io:format("hallnumberError Number:(~p),GameType:(~p)~n",[NumberTmp,GameTypeTmp]),
			{reply,{error,hallNumberError},State};
		{error,regotoHall} -> {reply,{error,regotoHall},State};
		_Any -> 
			{reply,{error,gotoSZPUnknowError},State}
	end;

%%gen_server回调:15,游戏结束，玩家从游戏房间退出到游戏大厅,参数说明:退出的房间编号，进入的大厅编号back:{reply,ok,State}
handle_call({exitRoom,[Account,RoomNumber,HallNumber]},_From,State) ->
	TRoomNumber = State#state.playerRoom,
	case TRoomNumber of
		null -> {reply,{error,userNotInRoom},State};
		RoomNumber -> %%nowUserType,当前玩家游戏状态，1，正在进行游戏，2,玩家未准备，3,玩家已准备,4,玩家逃跑中,5,游戏结束正在结算中
			UserNowType = State#state.nowUserType,
			case UserNowType of
				"1" -> {reply,{error,gamePlaying},State};
				"4" -> {reply,{error,taopao},State};
				"5" -> {reply,{error,gamePlaying},State};
				_Any ->
					case room_cache:exitRoom([Account,RoomNumber,HallNumber], RoomNumber) of
						{ok,RoomNumber,HallNumber,UserCoin} ->%%正常退出成功
							NewState = State#state{playerRoom = null,playerType = "2",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,canUsedCoin = UserCoin,userCoin = UserCoin},
							{reply,{ok,RoomNumber,HallNumber},NewState};
						{ok,RoomNumber,HallNumber} ->%%玩家只是游戏房间的观看者
							NewState = State#state{playerRoom = null,playerType = "2",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0},
							{reply,{ok,RoomNumber,HallNumber},NewState};
						{ok,OterRoom,OterHall} -> {reply,{ok,OterRoom,OterHall},State};
						{error,gamePlaying} -> {reply,{error,gamePlaying},State};
						{error,taopao} -> {reply,{error,taopao},State};
						{error,_} -> {reply,{error,unKnowExitRoom},State}
					end
			end;
		OterRoomNumber -> {reply,{ok,OterRoomNumber,HallNumber},State}
	end;
	
	

%%gen_server回调:16,玩家退出游戏大厅到过游戏主界面,参数说明:退出的大厅编号back:{reply,ok,State}
handle_call({exitHall,[Account,HallNumber]},_From,State) ->
	TRoomNumber = State#state.playerRoom,
	case TRoomNumber of
		null ->%%玩家不游戏房间中，可以正常退出游戏大厅
			HallGameType = State#state.nowGameType,
			case HallGameType of
				?SZP_GAMETYPE -> 
					case hall_cache:exitSZPHall([Account,HallNumber], HallNumber) of
						{ok,HallNumber} ->
							NewState = State#state{playerHall = null,playerType = "3",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0},
							{reply,{ok,State#state.playerHall},NewState};
						{ok,OtherHallNumber} -> {reply,{ok,OtherHallNumber},State};
						{error,userNotInHall} -> {reply,{error,userNotInHall},State};
						_Any -> {reply,{error,unKnowExitHall},State}
					end;
				?XQ_GAMETYPE ->
					case hall_cache:exitXQHall([Account,HallNumber], HallNumber) of
						{ok,HallNumber} ->
							NewState = State#state{playerHall = null,playerType = "3",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0},
							{reply,{ok,State#state.playerHall},NewState};
						{ok,OtherHallNumber} -> {reply,{ok,OtherHallNumber},State};
						_Any -> {reply,{error,unKnowExitHall},State}
					end;
				?TQ_GAMETYPE ->
					case hall_cache:exitTQHall([Account,HallNumber], HallNumber) of
						{ok,HallNumber} ->
							NewState = State#state{playerHall = null,playerType = "3",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0},
							{reply,{ok,State#state.playerHall},NewState};
						{ok,OtherHallNumber} -> {reply,{ok,OtherHallNumber},State};
						_Any -> {reply,{error,unKnowExitHall},State}
					end
			end;
		_Any ->%%玩家还在游戏房间中
			UserNowType = State#state.nowUserType,
			case UserNowType of
				"1" -> {reply,{error,gamePlaying},State};
				"4" -> {reply,{error,taopao},State};
				"5" -> {reply,{error,gamePlaying},State};
				_Any ->
					case room_cache:exitRoom([Account,TRoomNumber,HallNumber], TRoomNumber) of
						{ok,_RoomNumber,HallNumber,UserCoin} ->%%正常退出成功
							NewState = State#state{playerHall = null,playerRoom = null,playerType = "3",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0,canUsedCoin = UserCoin,userCoin = UserCoin},
							{reply,{ok,HallNumber},NewState};
						{ok,_RoomNumber,HallNumber} ->%%玩家只是游戏房间的观看者
							NewState = State#state{playerHall = null,playerRoom = null,playerType = "3",nowGameType = "2", szpLockCoin = 0,xqLockCoin = 0,tqLockCoin = 0},
							{reply,{ok,HallNumber},NewState};
						{ok,OterRoom,OterHall} -> {reply,{ok,OterRoom,OterHall},State};
						{error,gamePlaying} -> {reply,{error,gamePlaying},State};
						{error,taopao} -> {reply,{error,taopao},State};
						{error,_} -> {reply,{error,unKnowExitRoom},State}
					end
			end
	end;
			

%%gen_server回调:999,玩家注册帐号,参数说明:连接Socket接口，帐号，密码back:{reply,ok,State}
handle_call({createAccount,[Socket,Email,UserPaw]},_From,State) ->
	{reply,ok,State};

%%gen_server回调:998,设备码首次登陆游戏,参数说明:连接Socket接口，UDID设备码back:{reply,ok,State}
handle_call({createUDID,[Socket,UdidName]},_From,State) ->
	{reply,ok,State};

%%gen_server回调:997,使用帐号登陆游戏,参数说明:连接Socket接口，帐号，密码back:{reply,ok,State}
handle_call({loginTest,[Name,PassWord,Socket]},_From,State) ->
	case db_Tool:getPlayerData(name, {login,[Name,PassWord]}) of
		{ok,login} -> 
			io:format("state is:~p ~n",[State]),
			{reply,{ok,login},State#state{socket = Socket,account = Name}};
		LoginErro -> 
			io:format("error is:~p state is:~p ~n",[LoginErro,State]),
			{reply,LoginErro,State#state{account = undefined}}
	end;
	
%%gen_server回调:996,修改帐号密码,参数说明:帐号，旧密码，新密码back:{reply,ok,State}
handle_call({changePaw,[Account,OldPaw,NewPaw]},_From,State) ->
	case db_Tool:updataPlayerData(name, {changePaw,[Account,OldPaw,NewPaw]}) of
		{ok,changePaw} -> 
			io:format("state is:~p ~n",[State]),
			{reply,{ok,changePaw},State};
		ChangePawErro -> {reply,ChangePawErro,State}
	end;

%%gen_server回调:995,修改玩家昵称,参数说明:帐号，密码，新呢称back:{reply,ok,State},这里的PassWord是当前用户帐号的支付安全码
handle_call({changeName,[Email,PassWord,NewName]},_From,State) ->
	case db_Tool:updataPlayerData(name, {changeName,[Email,PassWord,NewName]}) of
		{ok,changeName} -> 
			io:format("state is:~p ~n",[State]),
			{reply,{ok,changeName},State};
		ChangePawErro -> {reply,ChangePawErro,State}
	end;

%%gen_server回调:994,设置手机号码,参数说明:电话号码back:{reply,ok,State}
handle_call({setPhone,[Account,PhoneNumber]},_From,State) ->
	case db_Tool:updataPlayerData(name, {setPhone,[Account,PhoneNumber]}) of
		{ok,setPhone} -> 
			io:format("state is:~p ~n",[State]),
			{reply,{ok,setPhone},State};
		Erro -> {reply,Erro,State}
	end;

%%gen_server回调:993,玩家充值宝石,参数说明:充值宝石数量，充值校检码back:{reply,ok,State}
handle_call({deltaGem,[GemNumber,DeltaKey]},_From,State) ->
	{reply,ok,State};

%%gen_server回调:992,玩家使用宝石兑换游戏道具,参数说明:物品类型，物品数量back:{reply,ok,State}
handle_call({useGem,[GoodType,GoodNumber]},_From,State) ->
	{reply,ok,State};

%%其他未知同步调用
handle_call(_Request,_From,State) ->
	Reply = ok,
	{reply,Reply,State}.



%% =====================================================================
%% 外部使用异步cast调用的接口
%% =====================================================================
%%外部调用接口:101,玩家收到游戏房间其他玩家发来的消息,参数说明:发消息的玩家帐号back:{noreply,State}
fromRoomMsg(Pid,[FromRoomNumber,FromAccount,MsgType,Msg]) ->
	gen_server:cast(Pid,{fromRoomMsg,[FromRoomNumber,FromAccount,MsgType,Msg]}).

%%外部调用接口:102,玩家收到游戏大厅其他玩家发来的消息,参数说明:好友帐号，好友名，消息类型，消息内容back:{noreply,State}
fromHallMsg(Pid,[FromHallNumber,FromAccount,MsgType,Msg]) ->
	gen_server:cast(Pid,{fromHallMsg,[FromHallNumber,FromAccount,MsgType,Msg]}).

%%外部调用接口:103,玩家收到其他在线玩家发来的消息,参数说明:发消息玩家帐号，发消息玩家名，语音类型，语音内容back:{noreply,State}
fromPlayerMsg(Pid,[FromAccount,MsgType,Msg]) ->
	gen_server:cast(Pid,{fromPlayerMsg,[FromAccount,MsgType,Msg]}).

%%外部调用接口:104,玩家收到好友发来的消息,参数说明:好友帐号，好友名，语音类型，语音内容back:{noreply,State}
fromFriendMsg(Pid,[FromFriend,MsgType,Msg]) ->
	gen_server:cast(Pid,{fromFriendMsg,[FromFriend,MsgType,Msg]}).

%%外部调用接口:105,玩家收到系统发来的消息,参数说明:游戏大厅玩家帐号，玩家名，消息类型，消息内容back:{noreply,State}
formSystemMsg(Pid,[MsgType,Msg]) ->
	gen_server:cast(Pid,{formSystemMsg,[MsgType,Msg]}).

%%外部调用接口:201,向游戏房间发送消息,消息会被房间进程分发,参数说明:目标玩家帐号，消息类型，消息内容back:{noreply,State}
toRoomMsg(Pid,[Account,ToRoomNumber,MsgType,Msg]) ->
	gen_server:cast(Pid,{toRoomMsg,[Account,ToRoomNumber,MsgType,Msg]}).

%%外部调用接口:202,向游戏大厅发送消息，消息会被大厅进程分发,参数说明:目标好友帐号，消息类型，消息内容back:{noreply,State}
toHallMsg(Pid,[Account,ToHallNumber,MsgType,Msg]) ->
	gen_server:cast(Pid,{toHallMsg,[Account,ToHallNumber,MsgType,Msg]}).

%%外部调用接口:203,向其他在线玩家发送消息，也就是密聊其他玩家,参数说明:目标玩家帐号，语音类型，语音内容back:{noreply,State}
toPlayerMsg(Pid,[Account,ToAccount,MsgType,Msg]) ->
	gen_server:cast(Pid,{toPlayerMsg,[Account,ToAccount,MsgType,Msg]}).

%%外部调用接口:204,向好友发送消息，如果好友离线消息会被存入好友的数据库,参数说明:目标好友帐号，语音类型，语音内容back:{noreply,State}
toFriendMsg(Pid,[Account,ToFriend,MsgType,Msg]) ->
	gen_server:cast(Pid,{toFriendMsg,[Account,ToFriend,MsgType,Msg]}).

%%外部调用接口:205,向游戏服务器发送系统消息，本消息可实现各种功能,参数说明:当前游戏房间编号号，语音类型，语音内容back:{noreply,State}
toSystemMsg(Pid,[Account,MsgType,Msg]) ->
	gen_server:cast(Pid,{toSystemMsg,[Account,MsgType,Msg]}).

%%外部调用接口:301,有玩家请求加自已来好友,参数说明:其他请求加自已好友的玩家帐号,玩家信息,请求表白back:{noreply,State}
formAskFriend(Pid,[AskAccount,PlayerMsg,Msg]) ->
	gen_server:cast(Pid,{formAskFriend,[AskAccount,PlayerMsg,Msg]}).

%%外部调用接口:302,自已的请求好友被通过或被拒绝,参数说明:自已请求的目标玩家帐号,对方同意或者拒绝的理由back:{noreply,State}
fromOtherAsk(Pid,[AskAccount,AskMsg]) ->
	gen_server:cast(Pid,{fromOtherAsk,[AskAccount,AskMsg]}).

%%外部调用接口:303,有玩家请求加入公会,参数说明:其他请求加入公会的玩家帐号,玩家信息,请求表白back:{noreply,State}
fromAskGongHui(Pid,[AskAccount,PlayerMsg,Msg]) ->
	gen_server:cast(Pid,{fromAskGongHui,[AskAccount,PlayerMsg,Msg]}).

%%外部调用接口:304,自已的请求加入公会得到答复,参数说明:自已请求的目标公会编号,对方同意或者拒绝的理由back:{noreply,State}
fromGongHuiAsk(Pid,[GongHuiNumber,AskMsg]) ->
	gen_server:cast(Pid,{fromGongHuiAsk,[GongHuiNumber,AskMsg]}).

%%外部调用接口:401,请求加某个玩家为好友,参数说明:自已帐号，请求的目标帐号,请求表白back:{noreply,State}
toAskFriend(Pid,[Account,AccountAsk,Msg]) ->
	gen_server:cast(Pid,{toAskFriend,[Account,AccountAsk,Msg]}).

%%外部调用接口:402,返回自已是否同意加对方为好友,参数说明:自已的帐号，拒绝的玩家帐号，自已拒绝的理由back:{noreply,State}
toAskOther(Pid,[Account,OtherAccount,Msg]) ->
	gen_server:cast(Pid,{toAskOther,[Account,OtherAccount,Msg]}).

%%外部调用接口:403,请求加入目标公会,参数说明:自已的帐号，目标公会编号，请求表白back:{noreply,State}
toAskGongHui(Pid,[Account,GongHuiNumber,Msg]) ->
	gen_server:cast(Pid,{toAskGongHui,[Account,GongHuiNumber,Msg]}).

%%外部调用接口:404,公会会长返回是否同意对方加入公会,参数说明:公会会长帐号，拒绝的帐号，拒绝原因back:{noreply,State}
toAskOtherGongHui(Pid,[Account,AccountAsk,Msg]) ->
	gen_server:cast(Pid,{toAskOtherGongHui,[Account,AccountAsk,Msg]}).

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
%%gen_server回调:101,玩家收到游戏房间其他玩家发来的消息,参数说明:发消息的玩家帐号back:{noreply,State}
handle_cast({fromRoomMsg,[FromRoomNumber,FromAccount,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:102,玩家收到游戏大厅其他玩家发来的消息,参数说明:好友帐号，好友名，消息类型，消息内容back:{noreply,State}
handle_cast({fromHallMsg,[FromHallNumber,FromAccount,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:103,玩家收到其他在线玩家发来的消息,参数说明:发消息玩家帐号，发消息玩家名，语音类型，语音内容back:{noreply,State}
handle_cast({fromPlayerMsg,[FromAccount,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:104,玩家收到好友发来的消息,参数说明:好友帐号，好友名，语音类型，语音内容back:{noreply,State}
handle_cast({fromFriendMsg,[FromFriend,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:105,玩家收到系统发来的消息,参数说明:游戏大厅玩家帐号，玩家名，消息类型，消息内容back:{noreply,State}
handle_cast({formSystemMsg,[MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:201,向游戏房间发送消息,消息会被房间进程分发,参数说明:目标玩家帐号，消息类型，消息内容back:{noreply,State}
handle_cast({toRoomMsg,[Account,ToRoomNumber,MsgType,Msg]},State) ->
	case room_cache:msgToRoom([Account,ToRoomNumber,MsgType,Msg], ToRoomNumber) of
		{ok,allUser} -> {noreply,State};
		{error,allUser} -> 
			Socket = State#state.socket,
			B = "201;46",
			io:format("msg server back:~p~n", [B]),
			RecData = [<<0,0,0,8>>,<<0,6>>,list_to_binary([B])],
			gen_tcp:send(Socket, iolist_to_binary([RecData])),
			io:format("server back bin:~p~n", [iolist_to_binary([RecData])]),
			{noreply,State}
	end;
	

%%gen_server回调:202,向游戏大厅发送消息，消息会被大厅进程分发,参数说明:目标好友帐号，消息类型，消息内容back:{noreply,State}
handle_cast({toHallMsg,[Account,ToHallNumber,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:203,向其他在线玩家发送消息，也就是密聊其他玩家,参数说明:目标玩家帐号，语音类型，语音内容back:{noreply,State}
handle_cast({toPlayerMsg,[Account,ToAccount,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:204,向好友发送消息，如果好友离线消息会被存入好友的数据库,参数说明:目标好友帐号，语音类型，语音内容back:{noreply,State}
handle_cast({toFriendMsg,[Account,ToFriend,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:205,向游戏服务器发送系统消息，本消息可实现各种功能,参数说明:当前游戏房间编号号，语音类型，语音内容back:{noreply,State}
handle_cast({toSystemMsg,[Account,MsgType,Msg]},State) ->
	{noreply,State};

%%gen_server回调:301,有玩家请求加自已来好友,参数说明:其他请求加自已好友的玩家帐号,玩家信息,请求表白back:{noreply,State}
handle_cast({formAskFriend,[AskAccount,PlayerMsg,Msg]},State) ->
	{noreply,State};

%%gen_server回调:302,自已的请求好友被通过或被拒绝,参数说明:自已请求的目标玩家帐号,对方同意或者拒绝的理由back:{noreply,State}
handle_cast({fromOtherAsk,[AskAccount,AskMsg]},State) ->
	{noreply,State};

%%gen_server回调:303,有玩家请求加入公会,参数说明:其他请求加入公会的玩家帐号,玩家信息,请求表白back:{noreply,State}
handle_cast({fromAskGongHui,[AskAccount,PlayerMsg,Msg]},State) ->
	{noreply,State};

%%gen_server回调:304,自已的请求加入公会得到答复,参数说明:自已请求的目标公会编号,对方同意或者拒绝的理由back:{noreply,State}
handle_cast({fromGongHuiAsk,[GongHuiNumber,AskMsg]},State) ->
	{noreply,State};

%%gen_server回调:401,请求加某个玩家为好友,参数说明:自已帐号，请求的目标帐号,请求表白back:{noreply,State}
handle_cast({toAskFriend,[Account,AccountAsk,Msg]},State) ->
	{noreply,State};

%%gen_server回调:402,返回自已是否同意加对方为好友,参数说明:自已的帐号，拒绝的玩家帐号，自已拒绝的理由back:{noreply,State}
handle_cast({toAskOther,[Account,OtherAccount,Msg]},State) ->
	{noreply,State};

%%gen_server回调:403,请求加入目标公会,参数说明:自已的帐号，目标公会编号，请求表白back:{noreply,State}
handle_cast({toAskGongHui,[Account,GongHuiNumber,Msg]},State) ->
	{noreply,State};

%%gen_server回调:404,公会会长返回是否同意对方加入公会,参数说明:公会会长帐号，拒绝的帐号，拒绝原因back:{noreply,State}
handle_cast({toAskOtherGongHui,[Account,AccountAsk,Msg]},State) ->
	{noreply,State};

%%通用方法：更新socket连接
handle_cast({replace,Socket},State) ->
	{noreply,State#state{socket = Socket},0};

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
