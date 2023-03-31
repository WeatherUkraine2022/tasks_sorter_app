-module(tasks_sorter_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  Args = [],
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->

  Flags = #{strategy => one_for_one, intensity => 2, period => 5},
  Children = [#{id => my_worker, start => {tasks_sorter, start_simple_server, []}, restart => permanent}],

  {ok, {Flags, Children}}.
