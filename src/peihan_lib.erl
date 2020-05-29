%%%-------------------------------------------------------------------
%%% @author wangdaobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 五月 2020 20:13
%%%-------------------------------------------------------------------
-module(peihan_lib).
-author("wangdaobin").

-include("file_info.hrl").

%% API
-export([init_ets_table/2, check_reload/3, insert/3, update/3, get_format_time_now/0]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 初始化ets表
%%
%% @end
%%--------------------------------------------------------------------
init_ets_table(TableName, TableOption) when is_atom(TableName) ->
    ets:new(TableName, TableOption);
init_ets_table(TableNames, TableOption) when is_list(TableNames) ->
    lists:foreach(
        fun(TableName) ->
            ets:new(TableName, TableOption)
        end, TableNames).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 检查是否需要热更新
%%  true: 热更| 更新ets表
%%  false: 不更新
%% @end
%%--------------------------------------------------------------------
check_reload(TableName, FileRootName, NewMTime) when is_list(FileRootName) ->
    check_reload(TableName, list_to_atom(FileRootName), NewMTime);
check_reload(TableName, FileRootName, NewMTime) ->
    case ets:lookup(TableName, FileRootName) of
        [] ->
            true;
        [{FileRootName, MTime}] ->
            NewMTime > MTime;
        Error ->
            peihan_log:warn(?MODULE, check_reload,
                [TableName, FileRootName, NewMTime, Error], lookup_error, ?LINE)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 检查是否需要热更新
%%  true: 热更| 更新ets表
%%  false: 不更新
%% @end
%%--------------------------------------------------------------------
insert(TableName, FileRootName, NewMTime) when is_list(FileRootName) ->
    insert(TableName, list_to_atom(FileRootName), NewMTime);
insert(TableName, FileRootName, NewMTime) ->
    ets:insert(TableName, {FileRootName, NewMTime}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%      遍历指定路径中所有文件, 检查并更新其中有更改的文件
%% @end
%%--------------------------------------------------------------------
update(Paths, TableName, ReloadFun) ->
%% 遍历所有路径
    lists:foreach(
        fun(Path) ->
            case file:list_dir(Path) of
                {ok, FileNames} ->      %% 获得路径所有文件的文件名
                    %% 遍历所有文件名
                    lists:foreach(
                        fun(FileName1) ->
                            FileName = Path ++ FileName1,
                            Suffix = filename:extension(FileName),
                            case lists:member(Suffix, ?SUFFIX) of
                                true ->
                                    case filelib:last_modified(FileName) of
                                        0 ->%% 没有正确获得文件最新的M时间
                                            peihan_log:warn(?MODULE, update,
                                                [Paths, TableName, ReloadFun, Path ++ FileName],
                                                last_modified_error, ?LINE);
                                        LastMTime ->
                                            case peihan_lib:check_reload(TableName, FileName, LastMTime) of
                                                true ->
                                                    peihan_lib:insert(TableName, FileName, LastMTime),
                                                    ReloadFun(FileName);
                                                _ ->
                                                    ignore
                                            end
                                    end;
                                _ ->
                                    ignore
                            end
                        end,
                        FileNames
                    );
                Error ->
                    peihan_log:warn(?MODULE, update, [Paths, TableName, ReloadFun, Error], list_dir_error, ?LINE)
            end
        end,
        Paths).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 获取格式化的时间(当前时间),
%% return:
%%      "2020-1-22_10:11:23"
%% @end
%%--------------------------------------------------------------------
get_format_time_now() ->
    {{Y, M, D}, {H, M1, S}} = calendar:local_time(),
    lists:concat([integer_to_list(Y), "-", integer_to_list(M), "-", integer_to_list(D), "_",
        integer_to_list(H), ":", integer_to_list(M1), ":", integer_to_list(S)]).