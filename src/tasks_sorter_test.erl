-module(tasks_sorter_test).

-include_lib("eunit/include/eunit.hrl").


sorting_test() ->
  ?assert(tasks_sorter:order_tasks(example_input1()) =:= {example_output1_json(),example_output1_shell_script()}).



example_input1() ->
<<"{
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


example_output1_json() ->
  <<"{\"tasks\":[{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\"},{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\"},{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\"}]}">>.
example_output1_shell_script() ->
  <<"#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1\n">>.
