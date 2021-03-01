-module(listener_module).

-import(lists, [delete/2]).
-import(query_module, [check_host_recovery_regisered/1, insert_host_recovery/2, create_board_db/1, create_task_db/1, load_boards_db/0, load_tasks_db/1, update_task_db/1]).
-import(election_module, [election_handler/1]).
-import(recover_node_module, [host_register_recovery/2, recovery_routine/2]).
-import(utility_module, [isolate_element/2, delete_hosts_from_list/2, get_timestamp_a/0]).
-import(message_sending_module, [server_up_message/2, send_and_wait/4, send_ack_to_primary/3]).
%% API
-export([listener_loop/4, receive_acks/2, db_manager_loop/6]).

% The listener_loop is a generic listener

% List_of_hosts contains all hosts' PIDs (NOTE: the primary does not save its own PID in order
% to avoid sending messages to itself)

% Every time a message is received in the specified format
% A process db_manager_loop is spawned
% Election_reay: false = an election is already being performed, true = otherwise
listener_loop([List_of_hosts], Server_type, Sent_heartbeat, Election_ready) ->
     io:format("~p: List of hosts inside the cluster: ~p~n", [self(), List_of_hosts]),
     receive
          {election_vote, From} ->
               From ! {ack_election_vote, self()},
               New_server_type = Server_type,
               % This case is used in order to NOT start multiple election procedures if multiple election_votes are received
               case Election_ready of
                    true -> election_handler([List_of_hosts]);
                    _ -> ok
               end,
               New_sent_heartbeat = false,
               Updated_list_of_hosts = List_of_hosts,
               New_election_ready = Election_ready;
          {ack_election_vote, From} ->
               io:format("~p: Received ack_election_vote from ~p, my job is done. Waiting for election_victory...~n", [self(), From]),
               New_server_type = Server_type,
               New_sent_heartbeat = false,
               Updated_list_of_hosts = List_of_hosts,
               New_election_ready = Election_ready;
          {election_victory, Received_list_of_hosts, New_server_type, From} ->
               io:format("~p: Server pid: ~p has been elected as the primary~n", [self(), From]),
               case New_server_type of
                    primary ->
                         Updated_list_of_hosts = Received_list_of_hosts;
                    _ ->
                         Updated_list_of_hosts = [From | Received_list_of_hosts]
               end,
               New_sent_heartbeat = false,
               New_election_ready = true;
     % TODO think about synchronization issues
          {heartbeat, From} ->
               io:format("~p: Reply with 'i_am_alive' to: ~p~n", [self(), From]),
               New_server_type = Server_type,
               From ! i_am_alive,
               New_sent_heartbeat = false,
               Updated_list_of_hosts = List_of_hosts,
               New_election_ready = Election_ready;

     %Routine to add a new host in the chain of hosts
          {new_server_up, From} ->
               io:format("~p: New server joined, its PID is: ~p ~n", [self(), From]),
               New_sent_heartbeat = false,
               New_server_type = Server_type,
               % remove empty lists using list comprehension
               Updated_list_of_hosts = List_of_hosts ++ [From],

               From ! {ack_new_server_up, Updated_list_of_hosts, self()},

               %Send to all the other servers active the new host that joined
               server_up_message(Updated_list_of_hosts, List_of_hosts),
               %Recovery Host routine - send it all the data
               recovery_routine(From, List_of_hosts),
               New_election_ready = Election_ready;
     % update_new_server_state(From);
     % TODO update new server database state
          i_am_alive ->
               io:format("~p: Received i_am_alive from primary~n", [self()]),
               New_server_type = Server_type,
               Updated_list_of_hosts = List_of_hosts,
               New_sent_heartbeat = false,
               New_election_ready = Election_ready;
     % This process is killed by the heartbeat_handler
     % The reason why it's killed through a message instead of an exit inside the heartbeat_handler is to
     % let the process finish whatever query it had already started
          quit ->
               New_server_type = Server_type,
               Updated_list_of_hosts = List_of_hosts,
               New_sent_heartbeat = false,
               New_election_ready = Election_ready,
               exit(electing_new_primary);

     % ------------- REQUEST SERVICE MESSAGE -----------------
          {Operation, Params, Primary_info, From} ->
               New_sent_heartbeat = false,
               New_server_type = Server_type,
               New_election_ready = Election_ready,
               case Operation of
                    hosts_to_delete ->
                         io:format("Listener_loop: Host delete request recived~n"),
                         io:format(List_of_hosts),
                         Updated_list_of_hosts = delete_hosts_from_list(List_of_hosts, Params),
                         io:format("Updated list ~n"),
                         io:format(Updated_list_of_hosts);
                    update_list ->
                         io:format("~p: Updating list of hosts: ~p ~n", [self(), [From] ++ Params]),
                         Updated_list_of_hosts = [From] ++ Params;
                    _ ->
                         Updated_list_of_hosts = List_of_hosts,
                         %take care of the request if not an host update
                         spawn(?MODULE, db_manager_loop, [From, Operation, Params, Primary_info, List_of_hosts, self()])
               end;
          _ ->
               io:format("~p: received undefined message ~n", [self()]),
               New_server_type = Server_type,
               Updated_list_of_hosts = List_of_hosts,
               New_sent_heartbeat = false,
               New_election_ready = Election_ready,
               exit(undefined_message)
     after 3000 ->
          case Election_ready of
               false ->
                    New_server_type = Server_type,
                    New_sent_heartbeat = false,
                    Updated_list_of_hosts = List_of_hosts,
                    New_election_ready = Election_ready;
               _ ->
                    case Server_type of
                         % Try asking the primary if it's Alive
                         secondary when Sent_heartbeat == false ->
                              % Can't use a new process for heartbeat handler because the pid would be different.
                              % all kind of messages: election, ack, etc. would be received from the listener_loop
                              %spawn(?MODULE, heartbeat_handler, [List_of_hosts, self()]),
                              % heartbeat_handler(List_of_hosts, self()),
                              [PID_primary | New_list_of_hosts_secondary] = List_of_hosts,
                              io:format("~p: Sending heaertbeat to ~p~n", [self(), PID_primary]),
                              PID_primary ! {heartbeat, self()},
                              New_server_type = Server_type,
                              New_sent_heartbeat = true,
                              Updated_list_of_hosts = List_of_hosts,
                              New_election_ready = Election_ready;
                         % Primary hasn't responded yet, it's probably down -> start election
                         secondary when Sent_heartbeat == true ->
                              io:format("~p: Primary is down~n", [self()]),
                              [PID_primary | New_list_of_hosts_secondary] = List_of_hosts,
                              New_server_type = Server_type,
                              New_sent_heartbeat = false,
                              Updated_list_of_hosts = List_of_hosts,
                              New_election_ready = false,
                              election_handler([List_of_hosts]);

                         %PID_primary ! {heartbeat, self()};
                         _ ->
                              New_server_type = Server_type,
                              New_sent_heartbeat = false,
                              Updated_list_of_hosts = List_of_hosts,
                              New_election_ready = Election_ready
                    end
          end
     end,
     listener_loop([Updated_list_of_hosts], New_server_type, New_sent_heartbeat, New_election_ready).


create_multiple_boards([],[]) ->
     ok;
create_multiple_boards([H_title|T_title],[H_time|T_time]) ->
     create_board_db({H_time,H_title}),
     create_multiple_boards(T_title, T_time).

broadcast_or_ack(From, Operation, Params, Primary_info, List_of_hosts, Listener_process_id) ->
     case Primary_info of
          primary ->
               send_and_wait(Operation, Params, List_of_hosts, List_of_hosts);
          %Finally send data to client ACK or informations asked
%%               send_update_to_clients(Params),
%%               % Reply web-server with ack
%%               From ! {Ack_operation, Params};
%%
          secondary ->
               send_ack_to_primary(From, Params, Listener_process_id)
     end.

% The db_manager_loop
% 1- create a connection to MySQL database
% 2- Execute the query
% 3- If it is the primary server
%   + Send the data to other hosts and waits their ACK
%    If it is a secondary
%   + Send and ack to the primary
db_manager_loop(From, Operation, Param, Primary_info, List_of_hosts, Listener_process_id) ->
     if
     %The primary must select the timestamp to be stored in the db
          Primary_info == primary ->
               Params = erlang:insert_element(1, Param, integer_to_list(get_timestamp_a()));
          true ->
               Params = Param
     end,

     %io:format(Query_time),

     %GET CURRENT TIMESTAMP TO PASS ALL THE OTHER HOSTS
     case Operation of
          create_board ->
               io:format("ho chiamato create_board~n"),
               create_board_db(Params),
               broadcast_or_ack(From, Operation, Params, Primary_info, List_of_hosts, Listener_process_id);

          %MULTIPLE BOARDS UPDATE
          create_boards ->
               io:format("ho chiamato create_boards~n"),
               List_of_boards_title = isolate_element(Params, 1),
               List_of_last_update_time = isolate_element(Params, 2),
               create_multiple_boards(List_of_boards_title, List_of_last_update_time),
               broadcast_or_ack(From, Operation, Params, Primary_info, List_of_hosts, Listener_process_id),
               io:format("create_board query ok~n"),
               io:format("SYNC: create_boards query ok~n");


          create_task ->
               create_task_db(Params),
               broadcast_or_ack(From, Operation, Params, Primary_info, List_of_hosts, Listener_process_id);

          create_tasks ->
               odbc:start(),
               {ok, Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
               odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id=?, last_update_time=? WHERE task_id=?",
                    [{sql_integer,
                         isolate_element(Params, 4)},
                         {{sql_varchar, 255},
                              isolate_element(Params, 5)},
                         {sql_integer,
                              isolate_element(Params, 1)}
                    ]),

               odbc:param_query(Ref_to_db, "INSERT INTO tasks (task_id, task_description, expiration_date, stage_id, last_update_time) VALUES (?, ?, ?, ?, ?)",
                    [{sql_integer,
                         isolate_element(Params, 1)},
                         {{sql_varchar, 255},
                              isolate_element(Params, 2)},
                         {{sql_varchar, 20},
                              isolate_element(Params, 3)},
                         {sql_integer,
                              isolate_element(Params, 4)},
                         {{sql_varchar, 255},
                              isolate_element(Params, 5)}
                    ]),
               %io:format("SYNC: create_tasks query ok~n"),
               %odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id=?, last_update_time=? WHERE task_id=?",
               %     [{sql_integer,
               %          isolate_element(Params, 4)},
               %          {{sql_varchar, 255},
               %               isolate_element(Params, 5)},
               %          {sql_integer,
               %               isolate_element(Params, 1)}
               %     ]),
               io:format("SYNC: create_tasks query ok~n"),
               broadcast_or_ack(From, Operation, Params, Primary_info, List_of_hosts, Listener_process_id),
               odbc:disconnect(Ref_to_db);

          update_task ->
               update_task_db(Params);


          %TODO: Define new Message to get data
          load_boards ->
               Data = load_boards_db();

          load_tasks ->
               Data = load_tasks_db(Params);

          true ->
               % UNEXPECTED MESSAGE TYPE
               io:format("Unexpected message format: Kill this process~n")
          %
     end.


%%% Receive ack routine ensure that all other secondary hosts have
%%% received the correct parameters
%%% IF not starts the host recovery routine
receive_acks(_, []) ->
     %SEND REPONSE TO CLIENT WHO ASKED DATA
     io:format("All host have responded~n");
receive_acks(Params, List_of_hosts) ->
     receive
          {ack, Params_received, Remote_ID} ->
               %If we have a match we received the expected params
               if Params =:= Params_received ->
                    io:format("Primary: ACK received~n"),
                    %Delete the host who responded from the list of hosts -> PID OF LISTENER LOOP
                    Remaining_list_of_hosts = delete(Remote_ID, [List_of_hosts]),
                    %DELETE THIS BELOW
                    %Remaining_list_of_hosts = List_of_hosts,
                    %io:format("~w~n", [Remote_ID]),
                    %io:format("~w~n", [List_of_hosts]),
                    %Call untill no host is in the list
                    receive_acks(Params, Remaining_list_of_hosts)
               end
     after 10000 ->
          io:format("Entered host recovery routine ~n"),
          host_register_recovery([List_of_hosts], Params),
          whereis(listener_loop_process) ! {hosts_to_delete, {List_of_hosts}, primary, self()},
          %SEND the response to client after recovery data are stored
          receive_acks(Params, [])
     end.
