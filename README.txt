Contrived HTTP job processing service implementation.

Basic implementation in form of module:function.
(Can be extended to server/rest-webserver/socket-webserver)
Uses jsx to decode/encode JSON data.


main function is:

-spec tasks_sorter:order_tasks(InputJobJSON :: binary()) -> {OrderedOutputJobJSON :: binary(), ShellScript :: binary()}.

to compile/launch run:

$ rebar3 shell


then in Erlang shell
 for testing, run:

1> eunit:test(tasks_sorter_test).

2> tasks_sorter:basic_test().


**In this basic implementation, there is no input validation (like testing input possibly containing circular dependencies in 'required tasks')
