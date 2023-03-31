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

To start job_server run:

3> application:start(tasks_sorter).

To order a  jobs, run:

4> tasks_sorter_server:process_job(YourJob).

where YourJob may be like in this call:

5>tasks_sorter_server:process_job(<<"{ \"tasks\": [{ \"name\": \"task-1\", \"command\": \"touch /tmp/file1\"},
{\"name\": \"task-2\",\"command\":\"cat /tmp/file1\",\"requires\":[\"task-3\"]},
{\"name\": \"task-3\",\"command\": \"echo 'Hello World!' > /tmp/file1\",\"requires\":[\"task-1\"]},
{\"name\": \"task-4\",\"command\": \"rm /tmp/file1\",\"requires\":[\"task-2\",\"task-3\"]}]}">>).



**In this basic implementation, there is no input validation (like testing input possibly containing circular dependencies in 'required tasks')
