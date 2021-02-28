-module(testing_module).
-import(query_module,[create_board_db/1]).
- import(listener_module,[listener_loop/4]).
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
  PID_server ! {create_board, {"SONO UN TITOLONE"}, primary, self()},
% io:format("------------------Send data to SERVERS~n"),
% PID_server ! {create_stage, {"Stage1", Title}, primary, self()},

  PID_server ! {create_task,  {"OCCHI DI DATTOO AHDSADHJASD","2021-10-10", 802}, primary, self()}.
%PID_server ! {update_task,  {2,1}, primary, self()}.

start() ->
  %PID_secondary = spawn(listener_module,listener_loop,[]),
  %PID_primay = spawn(listener_module,listener_loop,[[PID_secondary]]),
  %spawn(?MODULE,client_test, "A", [[PID_primay]]).
  odbc:start(),
  query_module:load_boards_recovery("0").
  %Magi = ["A", "B", "C"],
  %call_me(Magi).

call_me(List) ->
  List.
