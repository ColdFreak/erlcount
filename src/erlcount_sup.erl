-module(erlcount_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% @doc スーパーバイザーはerlcount_dispatchを監視する
init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    {ok, {{one_for_one, MaxRestart, MaxTime},
          [{dispatch,
            {erlcount_dispatch, start_link, []},
            transient, %%　非正常に終了する場合は再起動する
            60000,
            worker,
            [erlcount_dispatch]}]}}.
