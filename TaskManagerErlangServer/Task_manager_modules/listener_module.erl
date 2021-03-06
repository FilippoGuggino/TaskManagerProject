% When I wrote this, only God and I understood what I was doing
% Now, God only knows
-module(listener_module).

-import(lists, [delete/2]).
-import(query_module, [create_task_recovery/1, check_host_recovery_regisered/1, insert_host_recovery/2, create_board_db/1, create_task_db/1, load_boards_db/0, load_tasks_db/1, update_task_db/1, delete_host_from_recovery/1]).
-import(election_module, [election_handler/1]).
-import(recover_node_module, [host_register_recovery/2, recovery_routine/2]).
-import(utility_module, [isolate_element/2, delete_hosts_from_list/2, get_timestamp_a/0]).
-import(message_sending_module, [server_up_message/2, send_and_wait/4, send_ack_to_primary/3, send_data_to_client/2]).
%% API
-export([listener_loop/5, receive_acks/2, db_manager_loop/7]).

-include_lib("amqp_client/include/amqp_client.hrl").


update_operation_id(Operation, Params, Primary_info, Operation_id) ->
     if
          ((Primary_info == primary) and ((Operation == create_board) or (Operation == create_task) or (Operation == update_task))) ->
               Update_operation_id = Operation_id + 1;
          ((Primary_info == secondary) and ((Operation == create_board) or (Operation == create_task) or (Operation == update_task))) ->
               Update_operation_id = element(1, Params);
          true ->
               Update_operation_id = Operation_id
     end,
     %io:format("Sono qui ahahahahahhahhahhahahahah ~n"),
     %io:format(Update_operation_id),
     Update_operation_id.


% The listener_loop is a generic listener

% List_of_hosts contains all hosts' PIDs (NOTE: the primary does not save its own PID in order
% to avoid sending messages to itself)

% Every time a message is received in the specified format
% A process db_manager_loop is spawned
% Election_reay: false = an election is already being performed, true = otherwise
listener_loop([List_of_hosts], Server_type, Sent_heartbeat, Election_ready, Operation_id) ->
     io:format("~p: List of hosts inside the cluster: ~p~n", [self(), List_of_hosts]),
     receive
          {election_vote, From} ->
               Updated_operation_id = Operation_id,
               From ! {ack, {election_vote, From}},
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
               Updated_operation_id = Operation_id,
               io:format("~p: Received ack_election_vote from ~p, my job is done. Waiting for election_victory...~n", [self(), From]),
               New_server_type = Server_type,
               New_sent_heartbeat = false,
               Updated_list_of_hosts = List_of_hosts,
               New_election_ready = Election_ready;
          % due to relative speed, a server can receive an election victory even if it wasn't even aware that the primary was down
          % in this case, the election victory message is slightly different
          {election_victory, Received_list_of_hosts, New_server_type, From, Thread_ack_pid} ->
               Thread_ack_pid ! {ack, {election_victory, Received_list_of_hosts, New_server_type, From}},
               Updated_operation_id = Operation_id,
               io:format("~p: Server pid: ~p has been elected as the primary~n", [self(), From]),
               case New_server_type of
                    primary ->
                         io:format("~p: I am the new primary! ~n", [self()]),
                         % Start connection with RabbitMQ
                         application:ensure_started(amqp_client),
                         {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
                         {ok, Channel} = amqp_connection:open_channel(Connection),
               
                         % Update message inside rabbitmq indicating PID of the primary
                         Get = #'basic.get'{queue = <<"primary_queue">>},
                         {#'basic.get_ok'{delivery_tag = Tag}, Content} = amqp_channel:call(Channel, Get),
                         %#amqp_msg{payload = Payload} = Content,
                         amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
               
                         Payload = term_to_binary(self()),
               
                         Routing_key = <<"pr imary_pid">>,
                         Exchange_name = <<"topics_boards">>,
                         Publish = #'basic.publish'{exchange = Exchange_name, routing_key = Routing_key},
                         amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
               
                         %% Close the channel
                         amqp_channel:close(Channel),
               
                         %% Close the connection
                         amqp_connection:close(Connection),
                         Updated_list_of_hosts = Received_list_of_hosts;
                    _ ->
                         Updated_list_of_hosts = [From | Received_list_of_hosts]
               end,
               New_sent_heartbeat = false,
               New_election_ready = true;
          
          % self message sent insiede election handler
          {election_victory, Received_list_of_hosts, New_server_type, From} ->
               % Note: self-message doesn't need to send an ack
               Updated_operation_id = Operation_id,
               io:format("~p: Server pid: ~p has been elected as the primary~n", [self(), From]),
               case New_server_type of
                    primary ->
                         io:format("~p: I am the new primary! ~n", [self()]),
                         % Start connection with RabbitMQ
                         application:ensure_started(amqp_client),
                         {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
                         {ok, Channel} = amqp_connection:open_channel(Connection),
     
                         % Update message inside rabbitmq indicating PID of the primary
                         Get = #'basic.get'{queue = <<"primary_queue">>},
                         {#'basic.get_ok'{delivery_tag = Tag}, Content} = amqp_channel:call(Channel, Get),
                         %#amqp_msg{payload = Payload} = Content,
                         amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
     
                         Payload = term_to_binary(self()),
                         
                         Routing_key = <<"primary_pid">>,
                         Exchange_name = <<"topics_boards">>,
                         Publish = #'basic.publish'{exchange = Exchange_name, routing_key = Routing_key},
                         amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
     
                         %% Close the channel
                         amqp_channel:close(Channel),

                         %% Close the connection
                         amqp_connection:close(Connection),
                         Updated_list_of_hosts = Received_list_of_hosts;
                    _ ->
                         Updated_list_of_hosts = [From | Received_list_of_hosts]
               end,
               New_sent_heartbeat = false,
               New_election_ready = true;
     % TODO think about synchronization issues
          {heartbeat, From} ->
               Updated_operation_id = Operation_id,
               io:format("~p: Reply with 'i_am_alive' to: ~p~n", [self(), From]),
               New_server_type = Server_type,
               From ! i_am_alive,
               New_sent_heartbeat = false,
               Updated_list_of_hosts = List_of_hosts,
               New_election_ready = Election_ready;

     %Routine to add a new host in the chain of hosts
          {new_server_up, From} ->
               Updated_operation_id = Operation_id,
               io:format("~p: New server joined, its PID is: ~p ~n", [self(), From]),
               New_sent_heartbeat = false,
               New_server_type = Server_type,
               % remove empty lists using list comprehension
               Updated_list_of_hosts = List_of_hosts ++ [From],

               From ! {ack_new_server_up, Updated_list_of_hosts, self()},

               %Send to all the other servers active the new host that joined
               server_up_message(Updated_list_of_hosts, List_of_hosts),
               %Recovery Host routine - send it all the data
               recovery_routine(From, Operation_id),
               New_election_ready = Election_ready;
     % update_new_server_state(From);
     % TODO update new server database state
          i_am_alive ->
               Updated_operation_id = Operation_id,
               io:format("~p: Received i_am_alive from primary~n", [self()]),
               New_server_type = Server_type,
               Updated_list_of_hosts = List_of_hosts,
               New_sent_heartbeat = false,
               New_election_ready = Election_ready;
     % This process is killed by the heartbeat_handler
     % The reason why it's killed through a message instead of an exit inside the heartbeat_handler is to
     % let the process finish whatever query it had already started
          quit ->
               Updated_operation_id = Operation_id,
               New_server_type = Server_type,
               Updated_list_of_hosts = List_of_hosts,
               New_sent_heartbeat = false,
               New_election_ready = Election_ready,
               exit(electing_new_primary);



     % ------------------------ REQUEST SERVICE MESSAGE ---------------------------------------------
          {operation_id, OpID} ->
          	    New_sent_heartbeat = false,
               New_server_type = Server_type,
               New_election_ready = Election_ready,
               Updated_list_of_hosts = List_of_hosts,
               Updated_operation_id = OpID;

          {Operation, Params, Primary_info, From} ->
               io:format("~p: Received ~p operation from ~p~n", [self(), Operation, From]),
               New_sent_heartbeat = false,
               New_server_type = Server_type,
               New_election_ready = Election_ready,

               case Operation of
                    hosts_to_delete ->
                    	   Updated_operation_id = Operation_id,
                         io:format("Listener_loop: Host delete request recived~n"),
                         %The params are in the format {[List_of_hosts_no_reponse], Params_query}
                         host_register_recovery(element(1,Params), element(2,Params)),
                         Updated_list_of_hosts = List_of_hosts -- element(1,Params),
                         if
                              Primary_info == primary ->
                                   message_sending_module:send_broadcast(hosts_to_delete, Params,  Updated_list_of_hosts,  Updated_list_of_hosts);
                              true ->
                                   ok
                         end,

                         io:format("Updated list ~n");
                    update_list ->
                    	 Updated_operation_id = Operation_id,
                         io:format("~p: Updating list of hosts: ~p ~n", [self(), [From] ++ Params]),
                         Updated_list_of_hosts = [From] ++ Params,
                         How_many_hosts = Updated_list_of_hosts -- List_of_hosts,
                         if
                              length(How_many_hosts) == 1 ->
                                   delete_host_from_recovery(lists:nth(1,How_many_hosts));
                              true ->
                                   ok
                         end;

                    _ ->
                         Updated_list_of_hosts = List_of_hosts,
                         Updated_operation_id = update_operation_id(Operation,Params, Primary_info, Operation_id),
                         spawn(?MODULE, db_manager_loop, [From, Operation, Params, Primary_info, List_of_hosts, self(), Updated_operation_id])
               end;
          _ ->
               Updated_operation_id = Operation_id,
               io:format("~p: received undefined message ~n", [self()]),
               New_server_type = Server_type,
               Updated_list_of_hosts = List_of_hosts,
               New_sent_heartbeat = false,
               New_election_ready = Election_ready,
               exit(undefined_message)
     % ----------------------------------------------------------------------------------------------------------------------------------------------------
     after 3000 ->
          Updated_operation_id = Operation_id,
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

                              %Store when the primary failed
                              insert_host_recovery(PID_primary, {Operation_id}),

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
     listener_loop([Updated_list_of_hosts], New_server_type, New_sent_heartbeat, New_election_ready, Updated_operation_id).

create_multiple_boards([],[]) ->
     ok;
create_multiple_boards([H_title|T_title],[H_time|T_time]) ->
     create_board_db({H_time,H_title}),
     create_multiple_boards(T_title, T_time).

create_multilpe_tasks([])->
     tesks_ok;
create_multilpe_tasks([H | T])->
     create_task_recovery(H),
     create_multilpe_tasks(T).

get_head([H | _]) ->
     H.

broadcast_or_ack(From, Operation, Params, Primary_info, List_of_hosts, Listener_process_id) ->
     case Primary_info of
          primary ->
               send_and_wait(Operation, Params, List_of_hosts, List_of_hosts),
               case Operation of
                    create_task ->
                         From! {ack_create_task, Params},
                         utility_module:send_update_to_rabbitmq(ack_create_task, Params, element(2, Params));
                    update_task ->
                         From! {ack_update_task, Params},
                         utility_module:send_update_to_rabbitmq(ack_update_task, Params, element(2, Params));
                    _ ->
                         From! {ack_create_board, Params}
               end;
          %Finally send data to client ACK or informations asked
%%               send_update_to_clients(Params),
%%               % Reply web-server with ack
%%               From ! {Ack_operation, Params};
%%
          secondary ->
               io:format("~p: Sending ack to primary~n", [self()]),
               send_ack_to_primary(From, Params, Listener_process_id)
     end.

broadcast(Operation, Params, Primary_info, List_of_hosts)->
     case Primary_info of
          primary ->
               io:format("~p: Primary broadcasting the message", [self()]),
               send_and_wait(Operation, Params, List_of_hosts, List_of_hosts);
          _ ->
               ok
     end.

ack(From, Operation, Params, Primary_info, Listener_process_id) ->
     case Primary_info of
          primary ->
               io:format("~p: Sending ack to rabbitMQ and Client~n", [self()]),
               case Operation of
                    create_task ->
                         From! {ack_create_task, Params},
                         utility_module:send_update_to_rabbitmq(ack_create_task, Params, element(2, Params));
                    update_task ->
                         From! {ack_update_task, Params},
                         utility_module:send_update_to_rabbitmq(ack_update_task, Params, element(2, Params));
                    _ ->
                         From! {ack_create_board, Params}
               end;
          secondary ->
               io:format("~p: Sending ack to primary~n", [self()]),
               send_ack_to_primary(From, Params, Listener_process_id)
     end.

% The db_manager_loop
% 1- create a connection to MySQL database
% 2- Execute the query
% 3- If it is the primary server
%   + Send the data to other hosts and waits their ACK
%    If it is a secondary
%   + Send and ack to the primary
db_manager_loop(From, Operation, Param, Primary_info, List_of_hosts, Listener_process_id, Operation_id) ->
     if
     %The primary must select the timestamp to be stored in the db
          Primary_info == primary ->
               Params = erlang:insert_element(1, Param, Operation_id);
          true ->
               Params = Param
     end,

     %io:format(Query_time),

     %GET CURRENT TIMESTAMP TO PASS ALL THE OTHER HOSTS
     case Operation of
          create_board ->
               broadcast(Operation, Params, Primary_info, List_of_hosts),
               create_board_db(Params),
               ack(From, Operation, Params, Primary_info, Listener_process_id);

          %MULTIPLE BOARDS UPDATE
          create_boards ->
               io:format("ho chiamato create_boards~n"),
               List_of_boards_title = isolate_element(Params, 1),
               List_of_last_update_time = isolate_element(Params, 2),

               broadcast(Operation, Params, Primary_info, List_of_hosts),
               create_multiple_boards(List_of_boards_title, List_of_last_update_time),
               ack(From, Operation, Params, Primary_info, Listener_process_id),

               io:format("create_board query ok~n"),
               io:format("SYNC: create_boards query ok~n");


          create_task ->
               io:format("~p: Received create_task ~n", [self()]),
               broadcast(Operation, Params, Primary_info, List_of_hosts),
               create_task_db(Params),
               ack(From, Operation, Params, Primary_info, Listener_process_id);

          create_tasks ->
               broadcast(Operation, Params, Primary_info, List_of_hosts),
               create_multilpe_tasks(Params),
               io:format("SYNC: create_tasks query ok~n"),
               ack(From, Operation, Params, Primary_info, Listener_process_id);

          update_task ->
               io:format("~p: Received update_task ~n", [self()]),
               broadcast(Operation, Params, Primary_info, List_of_hosts),
               update_task_db(Params),
               ack(From, Operation, Params, Primary_info, Listener_process_id);
               % TODO send 'ack_update_task' back + send same ack message to rabbitmq


          %TODO: Define new Message to get data
          load_boards ->
               Data = load_boards_db(),
               send_data_to_client(From, Data);

          load_tasks ->
               io:format("~p: Received load_tasks for board=~p ~n", [self(), Params]),
               Data = load_tasks_db(Params),
               send_data_to_client(From, Data);

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
                    Remaining_list_of_hosts = delete(Remote_ID, List_of_hosts),
                    %DELETE THIS BELOW
                    %Remaining_list_of_hosts = List_of_hosts,
                    %io:format("~w~n", [Remote_ID]),
                    %io:format("~w~n", [List_of_hosts]),
                    %Call untill no host is in the list
                    receive_acks(Params, Remaining_list_of_hosts)
               end
     after 1000 ->
          io:format("Entered host recovery routine ~n"),
          io:format("~p: FORRRRRRRRR  RRRR RRRR =~p ~n", [self(), Params]),
          %host_register_recovery(List_of_hosts, Params),
          whereis(listener_loop_process) ! {hosts_to_delete, {List_of_hosts, Params}, primary, self()},
          %SEND the response to client after recovery data are stored
          receive_acks(Params, [])
     end.