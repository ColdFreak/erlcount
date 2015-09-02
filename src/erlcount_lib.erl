-module(erlcount_lib).
-export([find_erl/1]).
-include_lib("kernel/include/file.hrl").


%% @doc すべての`.erl`ファイルを探し出して
%% キューに入れる
find_erl(Directory) ->
    find_erl(Directory, queue:new()).

%% @doc まず渡された名前はディレクトリなのか、
%% 一般ファイルなのかを判断する
%% Priate
%% Dispatches based on file type
find_erl(Name, Queue) ->
    {ok, F=#file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
        directory   -> handle_directory(Name, Queue);
        regular     -> handle_regular_file(Name, Queue);
        _Other      -> dequeue_and_run(Queue)
    end.




handle_directory(Dir, Queue) ->
    %% list_dirはリストを返す
    case file:list_dir(Dir) of
        {ok, []} ->
            dequeue_and_run(Queue);
        {ok, Files} ->
            dequeue_and_run(enqueue_many(Dir, Files, Queue))
    end.

%% @doc 渡された`Path`は一個しかないが
%% `Files`はファイルのリスト,その中にディレクトリもあれば、
%% 普通のファイルもある.もちろんシンボリックリンクもあるでしょう。
%% file_erl関数の中の`_Other`ってやつ
%% 一個のディレクトリ下のファイルのリストの対して、
%% foldlを使って、全部キューに入れる
enqueue_many(Path, Files, Queue) ->
    F = fun(File, Q) -> queue:in(filename:join(Path, File), Q) end,
    lists:foldl(F, Queue, Files).

%% @doc 再帰的にキューの中のものを取り出して、探していく
dequeue_and_run(Queue) ->
    case queue:out(Queue) of
        {empty, _}                  -> done;
        {{value, File}, NewQueue}   -> find_erl(File, NewQueue)
    end.

%% @doc 出てきたファイルの拡張子は`.erl`なのかを判断する
handle_regular_file(Name, Queue) ->
    case file:extension(Name) of
        ".erl" ->
            {continue, Name, fun() -> dequeue_and_run(Queue) end};
        _NonErl ->
            dequeue_and_run(Queue)
    end.



