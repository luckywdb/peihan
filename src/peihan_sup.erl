%%%-------------------------------------------------------------------
%% @doc peihan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(peihan_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD_SPECS, [
    {peihan_reload, worker},
    {peihan_tcp, worker},
    {peihan_cfg, worker},
    {peihan_log, worker}]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
        intensity => 0,
        period => 1},
    ChildSpecs =
        lists:foldl(fun
                        ({Mod, Type}, Acc) ->
                            [#{id => Mod,
                                start => {Mod, start_link, []},
                                restart => transient,
                                shutdown => brutal_kill,
                                type => Type,
                                modules => [Mod]} | Acc]
                    end, [], ?CHILD_SPECS),
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
