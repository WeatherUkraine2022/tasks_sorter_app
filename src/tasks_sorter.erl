-module('tasks_sorter').
-author('Oleg Strogan').

-export([order_tasks/1, basic_test/0, simple_server/0, start_simple_server/0, stop_simple_server/0]).


start_simple_server()->
 io:format("To submit a job run command ~njob_server ! 'job'~n where 'job' is a task list in the agreed format (see example)~n "),
 io:format("To stop the serer and exit application run command ~njob_server ! stop ~n"),
 register(job_server, spawn_link(?MODULE, simple_server, [])),
{ok,self()}.

stop_simple_server() ->
 job_server ! stop.


simple_server() ->
  receive
    stop ->  io:format("Exiting application~n"), application:stop(tasks_sorter);
    Input ->
      io:format("Job input is:~n~n"),
      io:put_chars(Input),
      TList = decode_extract_tasks(Input),
      TMap = tasks_to_map(TList),
      {_,OrderedList} =  order_tasks_names(TMap),
      {Rebuilt, Script} = rebuild_tasks(TMap, OrderedList),
      io:format("~n~nJob example, tasks ordered:~n~n"),
      io:put_chars(Rebuilt),
      io:format("~n~njob shell commands:~n~n"),
      io:put_chars(Script),

      io:format("~n saving job to shell script 'ascript.sh'~n"),
      {ok, FS} = file:open("ascript.sh", [raw,write,binary]),

      file:write(FS,Script),
      file:close(FS),
      simple_server()
  end.


-spec basic_test() -> {ok, pid} | term().

basic_test() ->
  Input = job_input_example(),
  io:format("Job input example:~n~n"),
  io:put_chars(Input),
  TList = decode_extract_tasks(Input),
  TMap = tasks_to_map(TList),
  {_,OrderedList} =  order_tasks_names(TMap),
  {Rebuilt, Script} = rebuild_tasks(TMap, OrderedList),
  io:format("~n~nJob example, tasks ordered:~n~n"),
  io:put_chars(Rebuilt),
  io:format("~n~njob shell commands:~n~n"),
  io:put_chars(Script),

  io:format("~n saving job to shell script 'ascript.sh'~n"),
  {ok, FS} = file:open("ascript.sh", [raw,write,binary]),

  file:write(FS,Script),
  file:close(FS),
  {ok, self()}.


-spec order_tasks(InputJobJSON :: binary()) -> {OutputJobJSON :: binary(), ShellScript :: binary()}.

order_tasks(Input) ->
  TList = decode_extract_tasks(Input),
  TMap = tasks_to_map(TList),
  {_,OrderedList} =  order_tasks_names(TMap),
  rebuild_tasks(TMap, OrderedList).


rebuild_tasks(TM,RList) ->
  {Tasks, Commands} = lists:foldl(
    fun(Name, {TList,CList}) -> {ok, #{<<"command">> := C} = _M} = maps:find(Name, TM),
      {[#{<<"name">> => Name, <<"command">> => C}| TList], [C| CList]}
    end,
    {[],[]},
    RList
  ),
  Shell = <<"#!/usr/bin/env bash">>,
  Lst = [[C,$\n] || C <- [Shell | Commands]],
  Script = iolist_to_binary(Lst),
  Json = jsx:encode(#{<<"tasks">> => Tasks}),
  {Json, Script}.


order_tasks_names(TaskMap) -> maps:fold(
  fun(N,#{<<"requires">> :=Deps} = ATask,{TM,TList}) ->

    {TM1,TList1} = add_dep_tasks(TM, TList, Deps, TaskMap), %first, add required tasks, recursively

    case maps:find(N, TM1) of
      {ok,_M} -> {TM1, TList1}; %do not add the task if it has been added in previous steps
      error -> {maps:put(N, ATask, TM1), [N|TList1]} % add the task itself
    end

  end,
  {#{},[]},
  TaskMap
).


add_dep_tasks(TM,TList, [], _TaskMap) -> {TM, TList};

add_dep_tasks(TM,TList, [D | DTail], TaskMap) ->
  case maps:find(D, TM) of
    {ok,_M} -> add_dep_tasks(TM,TList, DTail,TaskMap);
    error ->
      NextTask = maps:get(D, TaskMap),
      TMUpd = maps:put(D, NextTask, TM),
      add_dep_tasks(TMUpd, [D | TList], DTail, TaskMap)
  end.


decode_extract_tasks(TasksInputExample) ->
  #{<<"tasks">> := TL} =  jsx:decode(TasksInputExample),
  TL.

tasks_to_map(TaskList) ->
  lists:foldl(fun(ATask,M) ->
    {N, ATaskM} = one_task_map(ATask), maps:put(N,ATaskM, M) end,
    #{},
    TaskList
  ).

one_task_map(#{<<"command">> := C, <<"name">> := N, <<"requires">> := R}) ->
  {N, #{<<"command">> => C, <<"requires">> => R, done => false}};

one_task_map(#{<<"command">> := C, <<"name">> := N}) ->
  {N, #{<<"command">> => C, <<"requires">> => [], done => false}}.



% Input example
job_input_example() -> <<"{
\"tasks\": [
{
\"name\": \"task-1\",
\"command\": \"touch /tmp/file1\"
},
{
\"name\": \"task-2\",
\"command\":\"cat /tmp/file1\",
\"requires\":[
\"task-3\"
]
},
{
\"name\": \"task-3\",
\"command\": \"echo 'Hello World!' > /tmp/file1\",
\"requires\":[
\"task-1\"
]
},
{
\"name\": \"task-4\",
\"command\": \"rm /tmp/file1\",
\"requires\":[
\"task-2\",
\"task-3\"
]
}
]
}">> .
