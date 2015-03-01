%% @author woodcol
%% @doc @todo Add description to db_Tool.


-module(db_Tool).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 initPool/1,
		 getPlayerData/2,
		 getLog/3,
		 savePlayerData/2,
		 updataPlayerData/2,
		 createPlayerData/2,
		 bondingPlayer/2
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================


%%初始化连接池
initPool(Count) ->
	PoolId = 'test',
	ConnArg = [{driver, mysql},
           {database, "test"},
           {host, "localhost"},
           {user, "root"},
           {password, "7654321"},
           {poolsize, Count},
           {default_pool, true}],
	db_api:add_pool(PoolId, ConnArg),
	Res = db_api:execute_sql("select version()").



%%通过MySQL中保存的UDID读取玩家数据
getPlayerData(udid,UDID) ->
	ok;

%%通过MySQL中保存的帐号名读取玩家数据
getPlayerData(name,NameData) ->
	case NameData of
		{login,[Name,PassWord]} -> tableRead(userdb,{login,[Name,PassWord]})%%登陆
	end.

%%通过日志日期间隔读取一定数量的日志
getLog(DateStart,DateEnd,Count)->
	ok.


%%通过UDID号保存用户数据
savePlayerData(udid,UDIDData) ->
	ok;
%%通过玩家帐号保存用户数据
savePlayerData(name,NameData) ->
	ok.


%%通过玩家UDID保存用户数据
updataPlayerData(udid,UDIDData) ->
	ok;
%%通过玩家帐号更新用户数据
updataPlayerData(name,NameData) ->
	case NameData of
		{changePaw,[Account,OldPaw,NewPaw]} -> tableupdate(userdb,{changePaw,[Account,OldPaw,NewPaw]});%%修改密码
		{changeName,[Email,PassWord,NewName]} -> tableupdate(userdb,{changeName,[Email,PassWord,NewName]});%%修改用户呢称
		{setPhone,[Account,PhoneNumber]} -> tableupdate(userdb,{setPhone,[Account,PhoneNumber]})%%设置用户手机号码
	end.



%%通过玩家设备UDID创建用户
createPlayerData(udid,UDIDData)->
	ok;
%%通过玩家帐号创建用户
createPlayerData(name,NameData)->
	case NameData of
		[UserName,UserPaw] -> 
			tableInsert(userdb,{createUser,[UserName,UserPaw]});
		[UdidName] -> tableInsert(userdb,{createUser,[UdidName,UdidName]})
	end.

%%在数据据表中添加新行
tableInsert(userdb,Data) ->
	case Data of
		{createUser,[UserName,UserPaw]} -> 
			io:format("user name:~p,user password:~p", [UserName,UserPaw]),
			Sql = "INSERT INTO `test`.`game_user` (`user_Name`, `user_Password`) VALUES (" ++ "'" ++ UserName ++ "','" ++ UserPaw ++ "')",
			db_api:execute_sql(Sql)
	end;
tableInsert(heavegame,Data) ->
	ok;
tableInsert(szp_data,Data) ->
	ok;
tableInsert(xq_data,Data) ->
	ok;
tableInsert(tq_data,Data) ->
	ok;
tableInsert(szp_gamesave,Data) ->
	ok;
tableInsert(xq_gamesave,Data) ->
	ok;
tableInsert(tq_gamesave,Data) ->
	ok.

%%删除MySQL数据表项
tabledelete(userdb,Data) ->
	ok;
tabledelete(heavegame,Data) ->
	ok;
tabledelete(szp_data,Data) ->
	ok;
tabledelete(xq_data,Data) ->
	ok;
tabledelete(tq_data,Data) ->
	ok;
tabledelete(szp_gamesave,Data) ->
	ok;
tabledelete(xq_gamesave,Data) ->
	ok;
tabledelete(tq_gamesave,Data) ->
	ok.

%%修改MySQL数据表项
tableupdate(userdb,Data) ->
	case Data of
		{changePaw,[Account,OldPaw,NewPaw]} -> %%修改用户密码
			io:format("changePaw -> user name:~p oldpaw:~p newpaw:~p ~n", [Account,OldPaw,NewPaw]),
			%%update 'Table_Name' from 'COLUMNS' WHERE 'COLUMN_NAME'='字段名'
			%%UPDATE `test`.`game_user` SET `user_Password`='7654324' WHERE `user_Name`='wood' and `user_Password`='7654322';
			Sql = "UPDATE `test`.`game_user` SET `user_Password`='"++ NewPaw ++"' WHERE `user_email`= '" ++ Account ++ "' and `user_Password` = '"++OldPaw++"'",
			PassMysql = db_api:execute_sql(Sql),
			io:format("update password:~p with name:~p ~n", [PassMysql,Account]),
			case PassMysql of
				{ok,1}->{ok,changePaw};
				{ok,0} -> {error,oldPawError};%%旧密码错误,或者新密码与旧密码相同
				{ok,[]} -> {error,loginName}; %%用户名不存在
				{error,_} -> {error,mysqlerror} %%数据库访问错误
			end;
		{changeName,[Email,PassWord,NewName]} ->
			io:format("changeName -> user Account:~p serverKey:~p newName:~p ~n", [Email,PassWord,NewName]),
			%%update 'Table_Name' from 'COLUMNS' WHERE 'COLUMN_NAME'='字段名'
			%%UPDATE `test`.`game_user` SET `user_Password`='7654324' WHERE `user_Name`='wood' and `user_Password`='7654322';
			Sql = "UPDATE `test`.`game_user` SET `user_Name`='"++ NewName ++"' WHERE `user_email`= '" ++ Email ++ "' and `user_serverPaw` = '"++PassWord++"'",
			PassMysql = db_api:execute_sql(Sql),
			io:format("update password:~p with account:~p ~n", [PassMysql,Email]),
			case PassMysql of
				{ok,1}->{ok,changeName};
				{ok,0} -> {error,nameError};%%支付密码错误,或者新昵称与旧昵称相同
				{ok,[]} -> {error,loginName}; %%用户名不存在
				{error,_} -> {error,mysqlerror} %%数据库访问错误
			end;
		{setPhone,[Account,PhoneNumber]} ->
			io:format("changeName -> user Account:~p setPhone:~p ~n", [Account,PhoneNumber]),
			%%update 'Table_Name' from 'COLUMNS' WHERE 'COLUMN_NAME'='字段名'
			%%update `test`.`game_user` SET `phoneNumber`='13590289064' WHERE  `user_email`= 'aaa@woodcol.com' and (`phoneNumber` is null or `phoneNumber` = '0');;
			Sql = "UPDATE `test`.`game_user` SET `phoneNumber`='"++ PhoneNumber ++"' WHERE  `user_email`= '" ++ Account ++ "' and (`phoneNumber` is null or `phoneNumber` = '0')",
			PassMysql = db_api:execute_sql(Sql),
			io:format("update password:~p with account:~p ~n", [PassMysql,Account]),
			case PassMysql of
				{ok,1}->{ok,setPhone};
				{ok,0} -> {error,phoneError};%%已经设置过手机号
				{ok,[]} -> {error,loginName}; %%用户名不存在
				{error,_} -> {error,mysqlerror} %%数据库访问错误
			end
	end;
tableupdate(heavegame,Data) ->
	ok;
tableupdate(szp_data,Data) ->
	ok;
tableupdate(xq_data,Data) ->
	ok;
tableupdate(tq_date,Data) ->
	ok;
tableupdate(szp_gamesave,Data) ->
	ok;
tableupdate(xq_gamesave,Data) ->
	ok;
tableupdate(tq_gamesave,Data) ->
	ok.

%%查找MySQL数据表项
tableRead(userdb,Data) ->
	case Data of
		{login,[Name,PassWord]} ->
			io:format("login -> user name:~p,user password:~p", [Name,PassWord]),
			%%select `TABLE_NAME` from `COLUMNS` where `COLUMN_NAME`='字段名'
			Sql = "select `user_Password` from `test`.`game_user`  where `user_email`='" ++ Name ++"'",
			PassMysql = db_api:execute_sql(Sql),
			io:format("select password:~p with name:~p ~n", [PassMysql,Name]),
			case PassMysql of
				{ok,[[PassWord]]} -> {ok,login};
				{ok,[[_]]} -> {error,loginPassword};%%密码错误
				{ok,[]} -> {error,loginName};	%%用户名不存在
				{error,_} -> {error,mysqlerror}  %%数据库访问错误
			end
	end;
tableRead(heavegame,Data) ->
	ok;
tableRead(szp_data,Data) ->
	ok;
tableRead(xq_data,Data) ->
	ok;
tableRead(tq_data,Data) ->
	ok;
tableRead(szp_gamesave,Data) ->
	ok;
tableRead(xq_gamesave,Data) ->
	ok;
tableRead(tq_gamesave,Data) ->
	ok.




%%绑定玩家帐号和UDID
bondingPlayer(Name,UDID) ->
	ok.
