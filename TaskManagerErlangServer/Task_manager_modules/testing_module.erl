-module(testing_module).
-import(query_module,[create_board_db/1]).
- import(listener_module,[listener_loop/4]).
- import(utility_module,[isolate_element/2]).
%% API
-export([board_test/0, get_boards/0, load_tasks/0, client_test/2, start/0, call_me/1]).

board_test() ->
  Board =  {"1", "Board_test"},
  create_board_db(Board).

get_boards() ->
  query_module:load_boards_db().

load_tasks() ->
  query_module:load_tasks_db({"A","CaccaAAA"}).

client_test(Title, PID_server) ->
  io:format("Client sending data!~n"),
  PID_server ! {create_board, {"Board:Down secondary"}, primary, self()},
% io:format("------------------Send data to SERVERS~n"),
% PID_server ! {create_stage, {"Stage1", Title}, primary, self()},

  PID_server ! {create_task,  {"Task: Down secondary","2021-10-10", 2}, primary, self()}.
%PID_server ! {update_task,  {2,1}, primary, self()}.

start() ->
  odbc:start(),
  Board = {"0", "Magicboard"},
  query_module:create_board_db(Board),
  Task = {"1", "Magicboard", "task di prova", "2021-12-12", 0, "Task", "Salume Magico", "Management"},
  query_module:create_task_db(Task),
  %Task_up = {"20", "Magicboard", "Task", 125},
  %query_module: update_task_db(Task_up),
  Task_rec= {"Magicboard", "task di prova", "2021-12-12", 44, "2-Task", "Salume Magico", "Management", "8080"},
  query_module:create_task_recovery(Task_rec),
  query_module:load_tasks_db({"0", "Magicboard"}).
  %PID_secondary = spawn(listener_module,listener_loop,[]),
  %PID_primay = spawn(listener_module,listener_loop,[[PID_secondary]]),
  %spawn(?MODULE,client_test, "A", [[PID_primay]]).
  %odbc:start(),
  %Params = {"0", "C"},
  %Params = [{ 1, "ASDRUBale","2021-10-10", 1062, "50000"}],
  %query_module:update_task_db(Params),

  %odbc:param_query(Ref_to_db, "INSERT INTO tasks (task_id, task_description, expiration_date, stage_id, last_update_time) VALUES (?, ?, ?, ?, ?)",
  %  [{sql_integer,
  %    isolate_element(Params, 1)},
  %    {{sql_varchar, 255},
  %      isolate_element(Params, 2)},
  %    {{sql_varchar, 20},
  %      isolate_element(Params, 3)},
  %    {sql_integer,
  %      isolate_element(Params, 4)},
  %    {{sql_varchar, 255},
  %      isolate_element(Params, 5)}
  %  ]).

call_me(List) ->
  List.
