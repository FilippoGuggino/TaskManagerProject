%%%-------------------------------------------------------------------
%%% @author Riccardo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. feb 2021 22:32
%%%-------------------------------------------------------------------
-module(listener_responder).
-author("Riccardo").

%% API
-export([start/1, listener_loop/1, db_manager_loop/5, client_test/2, send_and_wait/4, test/0, testante/0, test_db/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Client code - For testing purposes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client_test(Title, PID_server)->
  PID_server ! {create_board, "Hello", primary, self()},
  %PID_server ! {create_stage, ["Stage2",Title], self()},
  PID_server ! {create_stage, {"Stage1",Title}, primary, self()},
  PID_server ! {create_task, {"This task is nice","2021-10-10", 1}, primary, self()},
  PID_server ! {update_task, {2,1}, primary, self()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     SERVER CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() -> 
	io:format("ciao").

testante() ->
	io:format("testante").
	
	
%The start method starts the listener_loop and client_test
% input: List_of_host - secondary servers ID
start([List_of_hosts])->
  io:format("Starting"),
  io:format("AAA"),
  PID_server = spawn(?MODULE, listener_loop,[ [List_of_hosts] ]),
  spawn(?MODULE, client_test, ["Ciao",PID_server]).

% The listener_loop is a generic lister
% Every time a message is received in the specified format
% A process db_manager_loop is spawned
listener_loop([List_of_hosts])->
  receive
    { Operation ,Params, Primary_info, From} ->
      spawn(?MODULE, db_manager_loop, [From, Operation, Params, Primary_info, [List_of_hosts]]),
      listener_loop([List_of_hosts])
  end.
  
test_db() ->
	odbc:start(),
	  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
	  io:format("I am here").
% The db_manager_loop
% 1- create a connection to MySQL database
% 2- Execute the query
% 3- If it is the primary server
%   + Send the data to other hosts and waits their ACK
%    If it is a secondary
%   + Send and ack to the primary
db_manager_loop(From, Operation, Params, Primary_info, [H|T])->
  %Insert-Update data
  odbc:start(),
  {ok,Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=taskorganizer;user=root;password=;", []),
  io:format("I am here"),
  case Operation of
    create_board ->
      %odbc:param_query(Ref_to_db, "INSERT INTO boards VALUES (?)")
      odbc:param_query(Ref_to_db, "INSERT INTO boards (board_title) VALUES (?)",
        [{{sql_varchar, 255}, [Params]}]),
      io:format("Query ok");

    create_stage ->
      odbc:param_query(Ref_to_db, "INSERT INTO stages (stage_title, board_title) VALUES (?, ?)",
        [{{sql_varchar, 255},
          [element(1,Params)]},
          {{sql_varchar, 255},
            [element(2,Params)]}
        ]),
      io:format("Query ok");

    create_task ->
      odbc:param_query(Ref_to_db, "INSERT INTO tasks (task_description, expiration_date, stage_id) VALUES (?, ?, ?)",
        [{{sql_varchar, 255},
          [element(1,Params)]},
          {{sql_varchar,20},
            [element(2,Params)]},
          {sql_integer,
            [element(3,Params)]}
        ]),
      io:format("Query ok");

    update_task ->
      odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id = ? WHERE task_id = ?",
        [{sql_integer,
          [element(1,Params)]},
          {sql_integer,
            [element(2,Params)]}
        ]),
      io:format("Query ok");

    true ->
      % UNEXPECTED MESSAGE TYPE
      io:format("Unexpected message format: Kill this process")
      %
  end.

send_ack_to_primary(From, Params) ->
  From ! {ack, Params, self()}.


send_and_wait(_, Params, [List_of_hosts], []) -> receive_acks(Params,[List_of_hosts], []);
send_and_wait(Operation, Params, [List_of_hosts], [H,T])->
  H! {Operation, Params, secondary, self()},
  send_and_wait(Operation, Params, [List_of_hosts], T).


receive_acks(Params, [List_of_hosts], [List_of_responders])->
  receive
    {ack, Params_received, Remote_ID}->
      if
        %check if this was an expected ack
        Params == Params_received ->
            [Remote_ID|List_of_responders],
          %check if the responders list is the same as the sender list
            if [List_of_hosts] /= [List_of_responders] ->
              receive_acks(Params, [List_of_hosts], [List_of_responders])
            end;
        %The Params received was not for this process
        true ->
          receive_acks(Params, [List_of_hosts], [List_of_responders])
      end
  after 5 ->
        %this routine will store the data in case of
        %an host goes down
        % TODO host_recovery_routine()
        io:format("CALL host_recovery_routine()")
  end,
  %must be defined - sends the new operation to the queue (Guggino)
  % TODO send_response_to_client().
  io:format("CALL send_response_to_client()").

