%%%-------------------------------------------------------------------
%%% @author wangdaobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 五月 2020 22:25
%%%-------------------------------------------------------------------
-author("wangdaobin").
-record(file_info,
{size :: non_neg_integer() | 'undefined',  % Size of file in bytes.
    type :: 'device' | 'directory' | 'other' | 'regular' | 'symlink'
    | 'undefined',
    access :: 'read' | 'write' | 'read_write' | 'none' | 'undefined',
    atime :: file:date_time() | non_neg_integer() | 'undefined',
    % The local time the file was last read:
    % {{Year, Mon, Day}, {Hour, Min, Sec}}.
    % atime, ctime, mtime may also be unix epochs()
    mtime :: file:date_time() | non_neg_integer() | 'undefined',
    % The local time the file was last written.
    ctime :: file:date_time() | non_neg_integer() | 'undefined',
    % The interpretation of this time field
    % is dependent on operating system.
    % On Unix it is the last time the file
    % or the inode was changed.  On Windows,
    % it is the creation time.
    mode :: non_neg_integer() | 'undefined',
    % File permissions.  On Windows,
    % the owner permissions will be
    % duplicated for group and user.
    links :: non_neg_integer() | 'undefined',
    % Number of links to the file (1 if the
    % filesystem doesn't support links).
    major_device :: non_neg_integer() | 'undefined',
    % Identifies the file system (Unix),
    % or the drive number (A: = 0, B: = 1)
    % (Windows).
    %% The following are Unix specific.
    %% They are set to zero on other operating systems.
    minor_device :: non_neg_integer() | 'undefined',
    % Only valid for devices.
    inode :: non_neg_integer() | 'undefined',  % Inode number for file.
    uid :: non_neg_integer() | 'undefined',   % User id for owner.
    gid :: non_neg_integer() | 'undefined'}). % Group id for owner.

-define(TIME, 30000).           %% 循环定时器间隔时间, 30s
-define(WORK_PATHS, ["./"]).    %% 工作路径, 所有存放'.beam'文件
-define(CFG_PATHS, ["../cfg/"]). %% 存放cfg文件的路径
-define(FILE_BEAM, ".beam").    %% beam 二进制文件
-define(FILE_CFG, ".cfg").      %% cfg 配置文件
-define(SUFFIX, [?FILE_CFG, ?FILE_BEAM]).
-define(TABLE_BEAM, tab_beam).  %% 暂存beam文件的ets表
-define(TABLE_CFG, tab_cfg).    %% 暂存cfg配置文件的ets表
%% 新建ets表参数, 用表名注册, set存储方式, 访问权限public, 并发读写关闭 这里不指定键的位置
-define(TABLE_OPTION,
    [named_table, set, public,
        {write_concurrency, false},
        {read_concurrency, false}]).