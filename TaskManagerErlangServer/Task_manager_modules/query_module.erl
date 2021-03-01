-module(query_module).

%% API
-export([check_host_recovery_registered/1,insert_host_recovery/2,create_board_db/1,create_task_db/1, load_boards_db/0,
  load_tasks_db/1, update_task_db/1, load_boards_recovery/1, load_tasks_recovery/1, delete_host_from_recovery/1]).



check_host_recovery_registered(Host_pid) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Data} = odbc:param_query(Ref_to_db, "SELECT down_time FROM recovery_hosts WHERE host=?",
    [{{sql_varchar,255}, [atom_to_list(node(Host_pid))]}
    ]),
  odbc:disconnect(Ref_to_db),
  Data.

insert_host_recovery(Host_pid, Params) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "INSERT INTO recovery_hosts (host, down_time) VALUES (?,?)",
    [{{sql_varchar, 255},
      [atom_to_list(node(Host_pid))]},
      {{sql_varchar, 255},
        [element(1,Params)]}
    ]),
  odbc:disconnect(Ref_to_db).

%This creates the board and the 4 default stages
create_board_db(Params) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  %CREATE BOARDS
  {Info, _} = odbc:param_query(Ref_to_db, "INSERT INTO boards (board_title, last_update_time) VALUES (?,?)",
    [{{sql_varchar, 255},
      [element(2, Params)]},
      {{sql_varchar, 255},
        [element(1, Params)]}
    ]),
  if
    Info /= error ->
      %CREATE ASSOCIATE STAGES
      List_of_stages = ["BACKLOG", "DOING", "QUALITY CHECK","DONE"],
      odbc:param_query(Ref_to_db, "INSERT INTO stages (stage_title, board_title, last_update_time) VALUES (?, ?, ?)",
        [{{sql_varchar, 255},
          List_of_stages},
          {{sql_varchar, 255},
            [element(2, Params) ||  X <- List_of_stages]},
          {{sql_varchar, 255},
            [element(1, Params) ||  X <- List_of_stages]}
        ]),
      io:format("DB INFO: create_board query ok~n")
  end,
  odbc:disconnect(Ref_to_db).

create_task_db(Params)->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "INSERT INTO tasks (task_description, expiration_date, stage_id, last_update_time) VALUES (?, ?, ?, ?)",
    [{{sql_varchar, 255},
      [element(2, Params)]},
      {{sql_varchar, 20},
        [element(3, Params)]},
      {sql_integer,
        [element(4, Params)]},
      {{sql_varchar, 255},
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
                                                      FROM stages JOIN tasks ON stages.stage_id = tasks.stage_id
                                                      WHERE stages.board_title = ? ",
    [{{sql_varchar, 255},
      [element(2, Params)]}
    ]),
  odbc:disconnect(Ref_to_db),
  Tasks.

update_task_db(Params) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id = ?, last_update_time=?  WHERE task_id = ?",
    [{sql_integer,
      [element(2, Params)]},
      {{sql_varchar, 255},
        [element(1, Params)]},
      {sql_integer,
        [element(3, Params)]}
    ]),
  io:format("DB INFO: update_task query ok~n"),
  odbc:disconnect(Ref_to_db).

load_boards_recovery(Time) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected,_, Boards}=odbc:param_query(Ref_to_db, "SELECT * FROM boards WHERE last_update_time >= ?",
    [{{sql_varchar,255},
      [Time]}
    ]),
  odbc:disconnect(Ref_to_db),
  Boards.



load_tasks_recovery(Time) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  {selected, _, Tasks}=odbc:param_query(Ref_to_db, "SELECT * FROM tasks
                                                  WHERE last_update_time >= ? ",
    [{{sql_varchar,255},
      [Time]}
    ]),
  odbc:disconnect(Ref_to_db),
  Tasks.

delete_host_from_recovery(Process_id) ->
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
  odbc:param_query(Ref_to_db, "DELETE FROM host WHERE host=?",
    [{{sql_varchar,255},
      [atom_to_list(node(Process_id))]}
    ]),
  odbc:disconnect(Ref_to_db).
