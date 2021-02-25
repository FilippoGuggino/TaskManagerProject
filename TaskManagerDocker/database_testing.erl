%%%-------------------------------------------------------------------
%%% @author Riccardo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. feb 2021 14:45
%%%-------------------------------------------------------------------
-module(database_testing).
-author("Riccardo").

%% API
-export([start/0]).

start() ->
  Params = "New_table",
  odbc:start(),

  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=taskorganizer;user=root;password=;", []),
  io:format("db_mager_loop_started~n"),
  odbc:param_query(Ref_to_db, "INSERT INTO boards (board_title) VALUES (?)",
    [{{sql_varchar, 255}, [Params]}]),
  io:format("create_board query ok~n").