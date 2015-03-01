%% @author woodcol
%% @doc @todo Add description to login_server.


-module(login_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 server_loop/2
		]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {port, lsock, request_count = 0}).

-record(lgcount,{loginCount = 0}).

start_link(Port) ->
	 gen_server:start_link({local,?MODULE},?MODULE, [Port], []).

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
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary,{packet,0},{reuseaddr,true},{active,once}]),
	spawn(?MODULE, server_loop, [LSock, 0]),
    {ok, #state{port = Port, lsock = LSock},0}.
%% 	process_flag(trap_exit, true),
%% 	io:format("~p starting,~p~n",[?MODULE,self()]),
%% 	{ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
%% 	register(echoserver, spawn(?MODULE, server_loop, [LSock, 0])),
%% 	{ok, #state{port = Port, lsock = LSock},0}.

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
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


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
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================


handle_info({tcp, Socket, RawData}, State) ->
	io:format("tcp get data:~p~n",[RawData]),
    %%getClient(Socket, RawData),
    RequestCount = State#state.request_count, %%这里可以记录访问时间，如果多常时间没有访问，就断开连接
    {noreply, State#state{request_count = RequestCount + 1}};

handle_info(timeout, #state{lsock = LSock} = State) ->
   %% {ok, Sock} = gen_tcp:accept(LSock),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    clientClosed(Socket),
    RequestCount = State#state.request_count, %%这里可以记录访问时间，如果多常时间没有访问，就断开连接
    {noreply, State#state{request_count = RequestCount + 1}};

handle_info(Info, State) ->
    {noreply, State}.
%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

server_loop(ListenSocket, Count) ->  
    {ok,Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, server_loop, [ListenSocket, 0]),
	loop(Socket).
loop(Socket) ->
	receive
		{tcp,Socket,Bin} ->
			%%B = term_to_binary({binaries,"helo",useful}),
			{_,GetData} = split_binary(Bin,6),
			io:format("received:~p~n", [GetData]),
			AA = binary_to_list(GetData),
			io:format("received:~p~n", [AA]),
			STR = string:tokens(AA, ";"),
			io:format("received:~p~n", [STR]),
			getClient(Socket,STR),
			inet:setopts(Socket, [{active,once}]),
			loop(Socket);
		{tcp_closed,Socket} ->
			io:format("[~p] tcp_closed~n",[Socket]);
		{tcp_error,Socket,Reason} ->
			io:format("[~p] tcp_error:~p ~n", [Socket,Reason])
	end.
getClient(Socket,RawData) ->
	[H|T] = RawData,
	case H of
		%%玩家注册游戏,使用OpenUDID和用户名绑定
	"999" -> io:format("get data 999 is:~p~n", [T]),
			 [Name,Pw|_] = T,
			 Res = player_cache:createAccount([Socket,Name,Pw], [Name,Socket]),
			 io:format("create player back:~p~n",[Res]);
	"998" -> io:format("get data 998 is:~p~n", [T]);%%玩家使用OpenUDID登陆游戏,如果登陆游戏正常，则创建一个玩家进程
	"997" -> 
		io:format("get data 997 is:~p~n", [T]),
		[Name,Pw|_] = T,
		player_cache:loginTest([Name,Pw,Socket], [Name,Socket]);%%玩家正常登陆游戏,如果登陆游戏正常，则创建一个玩家帐号绑定的进程，服务器返回游戏大厅名字列表
	"996" -> io:format("get data 996 is:~p~n", [T]),%%玩家修改游戏密码changePaw([Email,OldPaw,NewPaw],Other)
			[Account,OldPaw,NewName|_] = T,
			player_cache:changePaw([Account,OldPaw,NewName],[Account,Socket]);
	"995" -> io:format("get data 995 is:~p~n", [T]),%%玩家修改用户名
			[Account,ServerPaw,NewPaw|_] = T,
			player_cache:changeName([Account,ServerPaw,NewPaw],[Account,Socket]);
	"994" -> io:format("get data 994 is:~p~n", [T]),%%设置手机号码
			[Account,PhoneNumber|_] = T,
			player_cache:setPhone([Account,PhoneNumber],[Account,Socket]);
	"993" -> io:format("get data 994 is:~p~n", [T]);%%玩家充值宝石
	"992" -> io:format("get data 994 is:~p~n", [T]);%%玩家消费宝石
	%%游戏相关
	"4" -> io:format("get data 4 is:~p~n", [T]),%%玩家进入三张牌游戏房间，获取三张牌游戏数据
			[Account,SZPHallNumber|_] = T,
			player_cache:gotoSZPHall([Account,SZPHallNumber],[Account,Socket]);
	"5" -> io:format("get data 5 is:~p~n", [T]),%%玩家进入游戏房间
			[Account,RoomNumber,PlayerType|_] = T,
			player_cache:gotoRoom([Account,RoomNumber,PlayerType],[Account,Socket]);
	"6" -> io:format("get data 6 is:~p~n", [T]),%%vip踢人，或者更改房间信息,只有VIP玩家才可以更改游戏房间信息
			[RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount|_] = T,
			player_cache:changeRoom([RoomAccount,Account,ChangeName,MaxUser,MaxView,RoomPaw,KickAccount],[Account,Socket]);
	"7" -> io:format("get data 7 is:~p~n", [T]),%%当前游戏已准备，等待其他玩家准备以及服务器发送同一桌玩家信息及游戏开始命令
			[RoomNumber,Account,UserType|_] = T,
		    player_cache:readyGame([RoomNumber,Account,UserType],[Account,Socket]);
	"8" -> io:format("get data 8 is:~p~n", [T]),%%三张牌玩家下注(下注为0表示pass),象棋时玩家走棋,跳棋时玩家走棋
			case T of
				[Account,"szp",GameCoin|_] ->
						player_cache:goGame([Account,[GameCoin]],[Account,Socket]);
				[Account,"xq",OldX,OldY,NewX,NewY|_] ->
						player_cache:goGame([Account,[OldX,OldY,NewX,NewY]],[Account,Socket]);
				[Account,"tq",OldX,OldY,NewX,NewY|_] ->
						player_cache:goGame([Account,[OldX,OldY,NewX,NewY]],[Account,Socket])
			end;
				
	"9" -> io:format("get data 4 is:~p~n", [T]),%%象棋在对方还没有走之前，要求悔棋,需要对方玩家同意
			[Account,Msg|_] = T,
		    player_cache:takingBack([Account,Msg],[Account,Socket]);
	"10" -> io:format("get data 4 is:~p~n", [T]),%%同意对方悔棋,或者不同意
			[Account,BackMsg|_] = T,
		    player_cache:okForTakingBack([Account,BackMsg],[Account,Socket]);
	"11" -> io:format("get data 4 is:~p~n", [T]),%%玩家在未结束游戏时请求退出当前游戏，即逃跑
			[Account,RoomNumber|_] = T,
		    player_cache:escaping([Account,RoomNumber],[Account,Socket]);
	"12" -> io:format("get data 4 is:~p~n", [T]),%%玩家正常退出当前游戏，或者取消准备,如果是取消准备，这里的GameRes是“e“,如果这里的结果是"1",表示玩家赢得相应游戏币，如果结果是"0",表示玩家输丢相应游戏币
			[Account,RoomNumber,GameRes|_] = T,
		    player_cache:noReady([Account,RoomNumber,GameRes],[Account,Socket]);
	"13" -> io:format("get data 4 is:~p~n", [T]),%%玩家进入象棋游戏
			[Account,HallNumber|_] = T,
			player_cache:gotoXQHall([Account,HallNumber],[Account,Socket]);
	"14" -> io:format("get data 4 is:~p~n", [T]),%%玩家进入跳棋游戏
			[Account,HallNumber|_] = T,
			player_cache:gotoTQHall([Account,HallNumber],[Account,Socket]);
	"15" -> io:format("get data 4 is:~p~n", [T]),%%游戏结束,玩家退出游戏房间,玩家从房间退出到大厅
			[Account,FromRoomNumber,ToHallNumber|_] = T,
			player_cache:exitRoom([Account,FromRoomNumber,ToHallNumber],[Account,Socket]);
	"16" -> io:format("get data 16 is:~p~n",[T]),%%玩家退出游戏大厅
			[Account,FromHallNumber|_] = T,
			player_cache:exitHall([Account,FromHallNumber],[Account,Socket]);

	%%消息接收相关
	"101" -> io:format("get data 4 is:~p~n", [T]);%%玩家收到房间中其他玩家发来消息
	"102" -> io:format("get data 4 is:~p~n", [T]);%%玩家收到大厅中其他玩家发来消息
	"103" -> io:format("get data 4 is:~p~n", [T]);%%玩家收到其他在线玩家发来消息
	"104" -> io:format("get data 4 is:~p~n", [T]);%%玩家收到好友发来消息
	"105" -> io:format("get data 4 is:~p~n", [T]);%%玩家收到来自游戏房间，或大厅，或ＧＭ或服务器发来的系统消息

	%%消息发送相关
	"201" -> io:format("get data 4 is:~p~n", [T]),%%游戏中玩家向游戏房间发送消息
			[Account,RoomNumber,MsgType,Msg|_] = T,
			player_cache:toRoomMsg([Account,RoomNumber,MsgType,Msg],[Account,Socket]);
	"202" -> io:format("get data 4 is:~p~n", [T]);%%游戏中玩家向游戏大厅发送消息
	"203" -> io:format("get data 4 is:~p~n", [T]);%%游戏中玩家向其他游戏玩家发送消息
	"204" -> io:format("get data 4 is:~p~n", [T]);%%游戏中玩家向好友发送消息
	"205" -> io:format("get data 4 is:~p~n", [T]);%%玩家向游戏房间，或游戏大厅，或GM，或系统服务器发送消息

	%%好友与公会系统，消息将由用户进程主动发送
	"301" -> io:format("get data 4 is:~p~n", [T]);%%有玩家请求加好友
	"302" -> io:format("get data 4 is:~p~n", [T]);%%其他玩家同意了自已的好友请求
	"303" -> io:format("get data 4 is:~p~n", [T]);%%有人请求加入公会
	"304" -> io:format("get data 4 is:~p~n", [T]);%%公会会长同意了自已的加入请求
	"305" -> io:format("get data 4 is:~p~n", [T]);%%玩家收到公会其他人发来的消息

	%%好友与公会申请
	"401" -> io:format("get data 4 is:~p~n", [T]);%%请求填加某个帐号或用户为自已的好友
	"402" -> io:format("get data 4 is:~p~n", [T]);%%是否同意申请人成为好友
	"403" -> io:format("get data 4 is:~p~n", [T]);%%请求加入某公会
	"404" -> io:format("get data 4 is:~p~n", [T]);%%是否同意入会
	"405" -> io:format("get data 4 is:~p~n", [T])%%向公会发送消息
	end.
 

%%服务器自已推送的信息
sendClient([H|T]) ->
	case H of
		"101" -> io:format("get data 101 is:~p~n", [T]);%%当前游戏中某个玩家发来文本消息发送给当前玩家
		"102" -> io:format("get data 101 is:~p~n", [T]);%%好友发来文本消息
		"103" -> io:format("get data 101 is:~p~n", [T]);%%给其他玩家发送语音消息
		"104" -> io:format("get data 101 is:~p~n", [T]);%%好友发来语音消息
		"105" -> io:format("get data 101 is:~p~n", [T]);%%当前游戏大厅其他玩家发来消息
		"106" -> io:format("get data 101 is:~p~n", [T])%%服务器发来系统消息
	end.

clientClosed(Socket) ->
	io:format("client closed from:~p~n",[Socket]).