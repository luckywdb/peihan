%%%-------------------------------------------------------------------
%%% @author wangdaobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 五月 2020 19:44
%%%-------------------------------------------------------------------
-module(peihan_json).
-author("wangdaobin").

%% API
-export([]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  将erlang数据转换成json数据格式
%% @end
%%--------------------------------------------------------------------
encode({json, Data}) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  将json数据格式转换成erlang数据格式
%%  {
%%      "folder_exclude_patterns": [".svn", ".git", ".hg", "CVS"],
%%      "index_workers": 0,
%%      "git_diff_target": "index"
%%  }
%% @end
%%--------------------------------------------------------------------
decode(JsonString) -> %% 字符串以 '{' 字符开头
    [Head | Tail] = JsonString,
    case Head =:= 123 andalso lists:last(Tail) =:= 125 of
        true -> %% JsonString 是一个以 { 开头, 以 } 结尾的字符串
            string:split(lists:droplast(Tail), ","),
            ok;
        _ ->
            illegal_json
    end.