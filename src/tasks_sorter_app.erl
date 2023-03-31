-module('tasks_sorter_app').
-behaviour(application).

-export([start/2, stop/1]).
  start(_StartType, _StartArgs) ->
    tasks_sorter_sup:start_link().
  stop(_State) ->
  ok.
