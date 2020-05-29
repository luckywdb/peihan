%%%-------------------------------------------------------------------
%%% @author wangdaobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%     加载,热更配置
%%%     读取, 更改配置
%%% @end
%%% Created : 20. 五月 2020 20:09
%%%-------------------------------------------------------------------
-module(peihan_cfg).
-author("wangdaobin").

-behaviour(gen_server).

-include("file_info.hrl").

%% API
-export([start_link/0, get_value/2, set_value/2, get_values/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    peihan_lib:init_ets_table(?TABLE_CFG, [{keypos, 1} | ?TABLE_OPTION]),   %% 初始化,存储cfg文件最后写入时间ets表
    peihan_lib:update(?CFG_PATHS, ?TABLE_CFG, fun reload_cfg/1),      %% 在gen_server 初始化同时初始化cfg相关est表
    peihan_log:info(?MODULE, init, [], start_ok, ?LINE),
    erlang:send_after(?TIME, ?SERVER, reload_cfg),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% 获取值
handle_call({get_value, TableName, Key}, _From, State) ->
    Reply = case ets:lookup(TableName, Key) of
                [] ->
                    undefine;
                [Value] ->
                    Value;
                Error ->
                    peihan_log:warn(?MODULE, handle_call,
                        [{get_value, TableName, Key}, State, Error], get_value_error, ?LINE),
                    undefine
            end,
    {reply, Reply, State};
%% 获取表中所有值
handle_call({get_values, TableName}, _From, State) ->
    
    {reply, ets:tab2list(TableName), State};
%% 设置值
handle_call({set_value, TableName, KeyValue}, _From, State) ->
    {reply, ets:insert(TableName, KeyValue), State};
handle_call(_Request, _From, State) ->
    {reply, on_handel, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(reload_cfg, State) ->
    peihan_lib:update(?CFG_PATHS, ?TABLE_CFG, fun reload_cfg/1),
    erlang:send_after(?TIME, ?SERVER, reload_cfg),
    {noreply, State};    %% 设置超时时间, 循环
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, State) ->
    peihan_log:error(?MODULE, terminate, [Reason, State], terminate, [], [], ?LINE),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%      根据配置的键获取值
%% @end
%%--------------------------------------------------------------------
get_value(TableName, Key) ->
    gen_server:call(?SERVER, {get_value, TableName, Key}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%      获取表中所有值
%% @end
%%--------------------------------------------------------------------
get_values(TableName) ->
    gen_server:call(?SERVER, {get_values, TableName}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%%      根据配置的键获取值
%% @end
%%--------------------------------------------------------------------
set_value(TableName, KeyValue) ->
    gen_server:call(?SERVER, {set_value, TableName, KeyValue}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 遍历指定路径下所有的cfg文件, 更新并从新加载配置文件
%% @end
%%--------------------------------------------------------------------
reload_cfg(FileName) ->
    case file:consult(FileName) of
        {ok, Data} ->
            lists:foreach(fun
                              ({TableName, KeyValues, Option}) ->
                                  case ets:info(TableName) of
                                      undefined ->      %% 没有这张表
                                          ets:new(TableName, [Option | ?TABLE_OPTION]);     %% 新建ets表
                                      _ ->
                                          ets:delete_all_objects(TableName) %% 已经存在, 将表里面的数据都删除
                                  end,
                                  lists:foreach(fun
                                                    (KeyValue) ->
                                                        ets:insert(TableName, KeyValue)
                                                end, KeyValues)
                          end, Data);
        Error ->
            peihan_log:warn(?MODULE, reload_cfg, [FileName, Error], consult_error, ?LINE)
    end.