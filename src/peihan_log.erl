%%%-------------------------------------------------------------------
%%% @author wangdaobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 五月 2020 22:05
%%%-------------------------------------------------------------------
-module(peihan_log).
-author("wangdaobin").

-behaviour(gen_server).

%% API
-export([start_link/0, info/5, warn/5, error/7]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("log.hrl").
-record(state, {file_io = []}).

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

%%--------------------------------------------------------------------
%% @doc
%% 一般日志
%%
%% @end
%%--------------------------------------------------------------------
info(Mod, Fun, Args, Desc, Line) ->
    gen_server:cast(?SERVER, {info, Mod, Fun, Args, Desc, Line, peihan_lib:get_format_time_now()}).

%%--------------------------------------------------------------------
%% @doc
%% 警告日志
%%
%% @end
%%--------------------------------------------------------------------
warn(Mod, Fun, Args, Desc, Line) ->
    gen_server:cast(?SERVER, {warn, Mod, Fun, Args, Desc, Line, peihan_lib:get_format_time_now()}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 报错日志
%%
%% @end
%%--------------------------------------------------------------------
error(Mod, Fun, Args, Desc, E1, E2, Line) ->
    gen_server:cast(?SERVER, {error, Mod, Fun, Args, Desc, E1, E2, Line, peihan_lib:get_format_time_now()}),
    ok.

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
    init_log_path(),
    FileIo = init_log(),
    erlang:send_after(?TIME_OUT, ?SERVER, new_log),
    {ok, #state{file_io = FileIo}}.

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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
%% 一般日志
handle_cast({?INFO, Mod, Fun, Args, Desc, Line, Time}, State) ->
    case lists:keyfind(?INFO, 1, State#state.file_io) of
        {_, IoDevice} ->
            write(IoDevice, Mod, Fun, Args, Desc, Line, Time);
        Error ->
            io:format("find file io error: ~p~n", [Error])
    end,
    {noreply, State};
%% 警告日志
handle_cast({?WARN, Mod, Fun, Args, Desc, Line, Time}, State) ->
    case lists:keyfind(?WARN, 1, State#state.file_io) of
        {_, IoDevice} ->
            write(IoDevice, Mod, Fun, Args, Desc, Line, Time);
        Error ->
            io:format("find file io error: ~p~n", [Error])
    end,
    {noreply, State};
%% 错误日志
handle_cast({?ERROR, Mod, Fun, Args, Desc, E1, E2, Line, Time}, State) ->
    case lists:keyfind(?ERROR, 1, State#state.file_io) of
        {_, IoDevice} ->
            write(IoDevice, Mod, Fun, Args, Desc, E1, E2, Line, Time);
        Error ->
            io:format("find file io error: ~p~n", [Error])
    end,
    {noreply, State};
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
%% 指定时间重新创建日志文件io
handle_info(new_log, State) ->
    NewFileIo = new_log(State#state.file_io),
    erlang:send_after(?TIME_OUT, ?SERVER, new_log),
    {noreply, State#state{file_io = NewFileIo}};
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
terminate(_Reason, _State) ->
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 初始化日志路径,
%%
%% @end
%%--------------------------------------------------------------------
init_log_path() ->
    case file:del_dir(?LOG_PATH) of
        {error, eexist} -> %% 已有文件夹,且文件夹不是空的
            ok;
        {error, enoent} -> %% 文件夹不存在
            file:make_dir(?LOG_PATH);
        Error ->
            throw(Error)
    end.
%%--------------------------------------------------------------------
%% @doc
%% 初始化,
%%
%% @end
%%--------------------------------------------------------------------
init_log() ->
    FormatTimeNow = peihan_lib:get_format_time_now(),
    lists:foldl(fun
                    (Type, Acc) ->
                        FileName = lists:concat([?LOG_PATH, Type, "-", FormatTimeNow]),
                        case file:open(FileName, ?OPEN_FILE_OPTION) of
                            {ok, IoDevice} ->
                                [{Type, IoDevice} | Acc];
                            Error ->
                                throw(Error)
                        end
                end, [], [?INFO, ?WARN, ?ERROR]).


%%--------------------------------------------------------------------
%% @doc
%%  新建日志文件, 先关闭老的文件io
%%
%% @end
%%--------------------------------------------------------------------
new_log(OldFileIo) ->
    lists:foreach(fun
                      ({Type, IoDevice}) ->
                          case file:close(IoDevice) of
                              ok ->
                                  ok;
                              Error ->
                                  warn(?MODULE, new_log, [OldFileIo, Type], Error, ?LINE)
                          end
                  end, OldFileIo),
    init_log().

%%--------------------------------------------------------------------
%% @doc
%%  写日志
%%
%% @end
%%--------------------------------------------------------------------
write(IoDevice, Mod, Fun, Args, Desc, Line, Time) ->
    io:format(IoDevice, "time: ~p; module: ~p; Function: ~p; args: ~p; descrip: ~p; line: ~p~n",
        [Time, Mod, Fun, Args, Desc, Line]).
write(IoDevice, Mod, Fun, Args, Desc, E1, E2, Line, Time) ->
    io:format(IoDevice, "time: ~p; module: ~p; Function: ~p; args: ~p; descrip: ~p; line: ~p; e1: ~p; e2: ~p~n",
        [Time, Mod, Fun, Args, Desc, Line, E1, E2]).
    
