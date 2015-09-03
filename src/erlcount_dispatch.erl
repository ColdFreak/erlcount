-module(erlcount_dispatch).
-behaviour(gen_fsm).

%% スーパーバイザー用のstart_link
%% ppoolの呼び出し元complete/4
-export([start_link/0, complete/4]).
-export([init/1]).

-define(POOL, erlcount).

%% @doc gen_fsmのデータはどのようになるのだろう。
%% 非同期で処理を行い、常にppool:run_async/2を呼び出したい
%% ファイルをキューにため終わったかどうかを知る方法がないため
%% 個々のワーカーに状態を使う
%% 状態のデータ
-record(data, {regex=[], refs=[]}).




