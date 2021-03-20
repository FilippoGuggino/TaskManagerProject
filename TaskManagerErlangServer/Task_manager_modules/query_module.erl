-module(query_module).

%% API
-export([check_host_recovery_registered/1,insert_host_recovery/2,create_board_db/1,create_task_db/1, load_boards_db/0,
  load_tasks_db/1, update_task_db/1, load_boards_recovery/1, load_tasks_recovery/1, delete_host_from_recovery/1, create_task_recovery/1, load_last_opid/0]).



check_host_recovery_registered(Host_pid) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Data} = odbc:param_query(Ref_to_db, "SELECT operation_id FROM recovery_hosts WHERE host=?",
    [{{sql_varchar,255}, [atom_to_list(node(Host_pid))]}
    ]),
  odbc:disconnect(Ref_to_db),
  Data.

insert_host_recovery(Host_pid, Params) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "INSERT INTO recovery_hosts (host, operation_id) VALUES (?,?)",
    [{{sql_varchar, 255},
      [atom_to_list(node(Host_pid))]},
      {sql_integer,
        [element(1,Params)]}
    ]),
  odbc:disconnect(Ref_to_db).

%This creates the board and the 4 default stages
create_board_db(Params) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  %CREATE BOARDS
  {updated, _} = odbc:param_query(Ref_to_db, "INSERT INTO boards (board_title, operation_id) VALUES (?,?)
                               ON DUPLICATE KEY UPDATE operation_id = ? ",
    [{{sql_varchar, 255},
      [element(2, Params)]},
      {sql_integer,
        [element(1, Params)]},
      {sql_integer,
        [element(1, Params)]}
    ]),
  io:format("DB INFO: update_boards query ok~n"),
  odbc:disconnect(Ref_to_db).

%Params = {Board title, Description, expiration date, stage id, title, creator, type}
create_task_db(Params)->
  io:format("~p~n", [Params]),
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "INSERT INTO tasks (board_title, task_description, expiration_date,
                                                stage_id, task_title, author,  type, operation_id)
                                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                                ON DUPLICATE KEY UPDATE operation_id=?",
    [{{sql_varchar, 255},
        [element(2, Params)]},
      {{sql_varchar, 255},
        [element(3, Params)]},
      {{sql_varchar, 255},
        [element(4, Params)]},
      {sql_integer,
        [element(5, Params)]},
      {{sql_varchar, 255},
        [element(6, Params)]},
      {{sql_varchar, 255},
        [element(7, Params)]},
      {{sql_varchar, 255},
        [element(8, Params)]},
      {sql_integer,
        [element(1, Params)]},
      {sql_integer,
        [element(1, Params)]}
    ]),
  io:format("DB INFO: create_task query ok~n"),
  odbc:disconnect(Ref_to_db).

load_boards_db() ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Boards} = odbc:sql_query(Ref_to_db, "SELECT board_title FROM boards"),
  odbc:disconnect(Ref_to_db),
  Boards.

load_tasks_db( Params) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Tasks} = odbc:param_query(Ref_to_db, "SELECT *
                                                      FROM  tasks
                                                      WHERE board_title = ? ",
    [{{sql_varchar, 255},
      [element(2, Params)]}
    ]),
  odbc:disconnect(Ref_to_db),
  Tasks.

%{Board title, task title, new stage id }
update_task_db(Params) ->
  io:format("ciao: ~p~n", [Params]),
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id = ?, operation_id =?  WHERE task_title = ? AND board_title = ?",
    [{sql_integer,
      [element(4, Params)]},
      {sql_integer,
        [element(1, Params)]},
      {{sql_varchar, 255},
        [element(3, Params)]},
      {{sql_varchar, 255},
        [element(2, Params)]}
    ]),
  io:format("DB INFO: update_task query ok~n"),
  odbc:disconnect(Ref_to_db).

load_boards_recovery(Time) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected,_, Boards}=odbc:param_query(Ref_to_db, "SELECT * FROM boards WHERE operation_id >= ?",
    [{sql_integer,
      [Time]}
    ]),
  odbc:disconnect(Ref_to_db),
  Boards.



load_tasks_recovery(Time) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Tasks}=odbc:param_query(Ref_to_db, "SELECT * FROM tasks
                                                  WHERE operation_id >= ? ",
    [{sql_integer,
      [Time]}
    ]),
  odbc:disconnect(Ref_to_db),
  Tasks.

delete_host_from_recovery(Process_id) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "DELETE FROM recovery_hosts WHERE host=?",
    [{{sql_varchar,255},
      [atom_to_list(node(Process_id))]}
    ]),
  odbc:disconnect(Ref_to_db).

create_task_recovery(Params)->
   {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "INSERT INTO tasks (board_title, task_description, expiration_date,
                                                stage_id, task_title, author,  type, operation_id)
                                VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    [{{sql_varchar, 255},
      [element(1, Params)]},
      {{sql_varchar, 255},
        [element(2, Params)]},
      {{sql_varchar, 255},
        [element(3, Params)]},
      {sql_integer,
        [element(4, Params)]},
      {{sql_varchar, 255},
        [element(5, Params)]},
      {{sql_varchar, 255},
        [element(6, Params)]},
      {{sql_varchar, 255},
        [element(7, Params)]},
      {sql_integer,
        [element(8, Params)]}
    ]),
  odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id=?, operation_id=? WHERE task_title=? AND board_title=?",
                     [{sql_integer,
                               [element(4, Params)]},
                       {sql_integer,
                               [element(8, Params)]},
                        {{sql_varchar, 255},
                               [element(5, Params)]},
                        {{sql_varchar, 255},
                               [element(1, Params)]}
                    ]),
   odbc:disconnect(Ref_to_db).


load_last_opid() ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Opid} = odbc:sql_query(Ref_to_db, "select max(operation_id)
                             from (select operation_id
                                   from boards
                                   union all
                                   select operation_id
                                   from tasks) AS Optable "),
  if
    Opid == [{null}] ->
      Up_opid = 0 ;
    true ->
      %Opid format [{Number}]
      Up_opid = element(1,lists:nth(1,Opid))
  end,
  io:format("up_opid: ~p~n", [Up_opid]),
  odbc:disconnect(Ref_to_db),
  Up_opid.

delete_data_from_opid(Opid) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "DELETE FROM boards WHERE operation_id>?",
    [{sql_integer,
      [Opid]}
    ]),
  odbc:param_query(Ref_to_db, "DELETE FROM tasks WHERE operation_id>?",
    [{sql_integer,
      [Opid]}
    ]),
  odbc:disconnect(Ref_to_db).
