-module(tasks_sorter_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_job/1]).

process_job(Input) -> gen_server:call(?MODULE,Input).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call(Request, _From, State) ->
  Reply = tasks_sorter:order_job(Request),
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> 
  %Do cleanup if needed
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
