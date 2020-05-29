%%%-------------------------------------------------------------------
%%% @author wangdaobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 五月 2020 19:44
%%%-------------------------------------------------------------------
-author("wangdaobin").

%% 日志文件路径前缀
-define(LOG_PATH, "../log/").
%% 日志文件类型
-define(INFO, info).
-define(WARN, warn).
-define(ERROR, error).
%% 文件操作参数
-define(OPEN_FILE_OPTION, [write, append]).

%% 半小时创建一次文档
-define(TIME_OUT, 1800000).