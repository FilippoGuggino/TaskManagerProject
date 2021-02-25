%%%-------------------------------------------------------------------
%%% @author Riccardo Xefraj, Filippo Guggino, Leonardo Cecchelli
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. feb 2021 22:32
%%%-------------------------------------------------------------------
-module(listener_responder).
-author("Riccardo Xefraj, Filippo Guggino, Leonardo Cecchelli").
-import(lists, [delete/2]).
%% API
-export([start/0, init/2, db_manager_loop/6, election_handler/1, listener_loop/4, client_test/2, send_and_wait/4, server_up_message/2, receive_acks/2]).

% Retrieve all RabbitMQ records
-include_lib("amqp_client/include/amqp_client.hrl").
% TODO maybe implement all listener loop with a record in order to avoid creating a new variable in every branch
-record(listener_loop_params, {server_type = secondary, sent_heartbeat = false, election_ready = true}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Client code - For testing purposes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client_test(Title, PID_server) ->
     io:format("Client sending data!~n"),
     PID_server ! {create_board, {"New_Board_con_timestamp"}, primary, self()}.
%io:format("------------------Send data to SERVERS~n"),
%PID_server ! {create_stage, {"Stage1",Title}, primary, self()}.
%PID_server ! {create_task,  {"This task is nice","2021-10-10", 1}, primary, self()},
%PID_server ! {update_task,  {2,1}, primary, self()}.

-spec get_timestamp_a() -> integer().
get_timestamp_a() ->
     {Mega, Sec, Micro} = os:timestamp(),
     (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     SERVER CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%The start method starts the listener_loop and client_test
% input: List_of_host - secondary servers ID
start() ->
     % TEST UTILITY FUNCTIONS
%%     List = [1, 2,3,4,5],
%%     io:format("~p~n", [index_of(List, 3)]),
%%     Index = index_of(List, 3),
%%     Sublist = lists:sublist(List, 2, 5),
%%     io:format("~p~n", [Sublist]),


     io:format("Starting~n"),
     %PID_secondary = spawn(?MODULE, listener_loop, [["A"]]),
     %PID_secondary = spawn(?MODULE, get_timestamp_a, []),
     %io:format(pid_to_list(PID_secondary)),
     PID_primary = spawn('erlang-server@172.18.0.162', ?MODULE, init, [[], primary]),
%%     timer:sleep(1000),
%%     % PID_primary = spawn(?MODULE, init, [primary]),
%%     PID_secondary = spawn(?MODULE, init, [[PID_primary], secondary]),
%%     timer:sleep(1000),
%%     PID_secondary3 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%     timer:sleep(1000),
%%     PID_secondary2 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%     timer:sleep(1000),
%%     PID_secondary4 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%     % register(primary_process, PID_primary),
%%     timer:sleep(2000),
%%     exit(PID_primary, testing_election).
     spawn(?MODULE, client_test, ["Ciao", PID_primary]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(List_of_hosts, Server_type) ->
     case Server_type of
          % This host is the primary
          primary ->
               io:format("~p: sono il primario~n", [self()]),
               % Start connection with RabbitMQ
%%               application:ensure_started(amqp_client),
%%               {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
%%               {ok, Channel} = amqp_connection:open_channel(Connection),
%%
%%               % Create new Exchange, used to dispatch updates to "interested" clients
%%               Exchange = #'exchange.declare'{exchange = <<"topics_boards">>,
%%                    type = <<"topic">>},
%%               #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
%%
%%               %% Close the channel
%%               amqp_channel:close(Channel),
%%               %% Close the connection
%%               amqp_connection:close(Connection),
               listener_loop([[]], primary, false, true);
          % This host is a Secondary
          secondary ->
               io:format("~p: sono un secondario~n", [self()]),
               % TODO send new_server_up to primary
               [PID_primary | List_without_primary] = List_of_hosts,
               PID_primary ! {new_server_up, self()},
               receive
                    {ack_new_server_up, Updated_list_of_hosts, From} ->
                         io:format("~p: received updated list of hosts: ~p ~n", [self(), Updated_list_of_hosts]),
                         Complete_list_of_hosts = [From | Updated_list_of_hosts],
                         listener_loop([Complete_list_of_hosts], secondary, false, true);
                    _ ->
                         io:format("undefined message."),
                         exit(undefined_message)
               after 5000 ->
                    io:format("Critical Failure can't connect to primary."),
                    exit(cant_connect_to_primary)
               end
     end.


% The listener_loop is a generic listener

% List_of_hosts contains all hosts' PIDs (NOTE: the primary does not save its own PID in order
% to avoid sending messages to itself)

% Every time a message is received in the specified format
% A process db_manager_loop is spawned
% Election_reay: false = an election is already being performed, true = otherwise
listener_loop([List_of_hosts], Server_type, Sent_heartbeat, Election_ready) ->
     io:format("~p: List of hosts inside the cluster: ~p~n", [self(), List_of_hosts]),
     receive
          {election_vote, From}->
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
          {election_victory, Received_list_of_hosts, New_server_type,  From} ->
               io:format("~p: Server pid: ~p has been elected as the primary~n",[self(), From]),
               Updated_list_of_hosts = [From | Received_list_of_hosts],
               New_sent_heartbeat = false,
               New_election_ready = true;
               % TODO think about synchronization issues
          {heartbeat, From} ->
               io:format("~p: Reply with 'i_am_alive' to: ~p~n",[self(), From]),
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
               server_up_message([Updated_list_of_hosts], List_of_hosts),
               From ! {ack_new_server_up, Updated_list_of_hosts, self()},
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
          {Operation, Params, Primary_info, From} ->
               New_sent_heartbeat = false,
               New_server_type = Server_type,
               New_election_ready = Election_ready,
               case Operation of
                    hosts_to_delete ->
                         io:format("Listener_loop: Host delete request recived~n"),
                         io:format(List_of_hosts),
                         Updated_list_of_hosts = delete_hosts_from_list(List_of_hosts, element(1, Params)),
                         io:format("Updated list ~n"),
                         io:format(Updated_list_of_hosts);
                    update_list ->
                         io:format("~p: Updating list of hosts: ~p ~n", [self(), [From] ++ Params]),
                         Updated_list_of_hosts = [From] ++ Params;
                    _ ->
                         Updated_list_of_hosts = List_of_hosts,
                         %take care of the request if not an host update
                         spawn(?MODULE, db_manager_loop, [From, Operation, Params, Primary_info, [List_of_hosts], self()])
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

% Implementation of Bully algorithm
%%If P has the highest process ID, it sends a Victory message to all other processes and becomes the new Coordinator. Otherwise, P broadcasts an Election message to all other processes with higher process IDs than itself.
%%If P receives no Answer after sending an Election message, then it broadcasts a Victory message to all other processes and becomes the Coordinator.
%%If P receives an Answer from a process with a higher ID, it sends no further messages for this election and waits for a Victory message. (If there is no Victory message after a period of time, it restarts the process at the beginning.)
%%If P receives an Election message from another process with a lower ID it sends an Answer message back and starts the election process at the beginning, by sending an Election message to higher-numbered processes.
%%If P receives a Coordinator message, it treats the sender as the coordinator.
election_handler([List_of_hosts]) ->
     Index = index_of(List_of_hosts, self()),
     Higher_prio_list_of_hosts = lists:sublist(List_of_hosts, Index-1),
     io:format("~p: List of hosts with higher priority than me ~p~n", [self(), Higher_prio_list_of_hosts]),
     send_generic_message_to_list_of_hosts({election_vote, self()}, Higher_prio_list_of_hosts),
     receive
          {election_victory, Remaining_hosts, secondary,From} ->
               io:format("~p: Received victory_election from ~p ~n", [self(), From]),
               self() ! {election_victory, Remaining_hosts, secondary,From};
          {election_vote, From} ->
               From ! {ack_election_vote, self()},
               election_handler([List_of_hosts]);
          {ack_election_vote, From} ->
               io:format("~p: Received ack_election_vote from ~p, my job is done. Waiting for election_victory...~n", [self(), From]);
          Message ->
               io:format("dosandoasmndas ~p~n", [Message]);
          _ ->
               io:format("~p: received undefined message inside election handler~n", [self()]),
               exit(undefined_message)
     after 1000 ->
          io:format("~p: Either I am the highest priority or all higher priority servers are down. I am going to candidate as Primary~n",[self()]),
          Remaining_hosts = lists:sublist(List_of_hosts, Index+1, length(List_of_hosts)),
          io:format("~p: Remaining hosts in the cluster after election ~p~n",[self(), Remaining_hosts]),
          % Send results of election to all servers inside the cluster
          send_generic_message_to_list_of_hosts({election_victory, Remaining_hosts, secondary, self()}, Remaining_hosts),
          self() ! {election_victory, Remaining_hosts, primary, self()}
     end.


send_generic_message_to_list_of_hosts(_, []) ->
     ok;
%%send_generic_messages(Message, [PID_host]) ->
%%     io:format("Message: ~p     PID: ~p~n", [Message, PID_host]),
%%     PID_host ! Message;
send_generic_message_to_list_of_hosts(Message, [PID_host | Remaining_hosts]) ->
     PID_host ! Message,
     send_generic_message_to_list_of_hosts(Message, Remaining_hosts).

%delete_hosts_from_list(List, _) ->
%     [];
delete_hosts_from_list(List, []) ->
     List;
delete_hosts_from_list(List, [H | T]) ->
     Updated_list = lists:delete(H, List),
     delete_hosts_from_list(Updated_list, T).

% The db_manager_loop
% 1- create a connection to MySQL database
% 2- Execute the query
% 3- If it is the primary server
%   + Send the data to other hosts and waits their ACK
%    If it is a secondary
%   + Send and ack to the primary
db_manager_loop(From, Operation, Param, Primary_info, [List_of_hosts], Listener_process_id) ->
     %Insert-Update data
     %{Mega, Sec, Micro} = now(),
     %Query_time = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
     %{Mega,Sec,Micro} = erlang:now(),
     Ack_operation = undefined,
     if
          Primary_info == primary ->
               Params = erlang:insert_element(1, Param, integer_to_list(get_timestamp_a()));
          true ->
               Params = Param
     end,

     %io:format(Query_time),
     odbc:start(),
     {ok, Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=TaskOrganizer;user=root;", []),
     io:format("db_mager_loop_started~n"),
     %GET CURRENT TIMESTAMP TO PASS ALL THE OTHER HOSTS
     case Operation of
          create_board ->
               %,
               odbc:param_query(Ref_to_db, "INSERT INTO boards (board_title, last_update_time) VALUES (?,?)",
                    [{{sql_varchar, 255},
                         [element(2, Params)]},
                         {{sql_varchar, 255},
                              [element(1, Params)]}
                    ]),
               io:format("create_board query ok~n");

          create_stage ->
               odbc:param_query(Ref_to_db, "INSERT INTO stages (stage_title, board_title, last_update_time) VALUES (?, ?, ?)",
                    [{{sql_varchar, 255},
                         [element(2, Params)]},
                         {{sql_varchar, 255},
                              [element(3, Params)]},
                         {{sql_varchar, 255},
                              [element(1, Params)]}
                    ]),
               io:format("create_stage query ok~n");

          create_task ->
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
               io:format("create_task query ok~n");

          update_task ->
               odbc:param_query(Ref_to_db, "UPDATE tasks SET stage_id = ?, last_update_time=?  WHERE task_id = ?",
                    [{sql_integer,
                         [element(2, Params)]},
                         {{sql_varchar, 255},
                              [element(1, Params)]},
                         {sql_integer,
                              [element(3, Params)]}
                    ]),
               io:format("update_task query ok~n");

          true ->
               % UNEXPECTED MESSAGE TYPE
               io:format("Unexpected message format: Kill this process~n")
          %
     end.
%%     case Server_info of
%%          primary when Ack_operation =/= undefined ->
%%               io:format("I am the PRIMARY"),
%%               send_and_wait(Operation, Params, [List_of_hosts], [List_of_hosts]),
%%               %Finally send data to client ACK or informations asked
%%
%%               send_update_to_clients(Params),
%%               % Reply web-server with ack
%%               From ! {Ack_operation, Params};
%%
%%          secondary ->
%%               send_ack_to_primary(From, Params, Listener_process_id)
%%     end.

send_ack_to_primary(From, Params, Listener_process_id) ->
     io:format("Secondary sending ack to primary!~n"),
     From ! {ack, Params, Listener_process_id}.

send_update_to_clients(Params) ->
     % TODO send update message to all "interested" clients through RabbitMQ

     application:ensure_started(amqp_client),
     {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
     {ok, Channel} = amqp_connection:open_channel(Connection),

     %% Publish a message
     % TODO change payload with correct data
     Payload = <<"foobar">>,
     % TODO check if element is enough or it needs to be changed into a BIN (atom_to_binary)
     Publish = #'basic.publish'{exchange = <<"topics_boards">>, routing_key = element(1, Params)},
     amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

     %% Close the channel
     amqp_channel:close(Channel),
     %% Close the connection
     amqp_connection:close(Connection).


send_ack_to_primary(From, Params) ->
     io:format("Secondary sending ack to primary!~n"),
     From ! {ack, Params, erlang:self()}.


send_and_wait(_, Params, [List_of_hosts], []) ->
     io:format("All data have been sent - Wait for acks~n"),
     receive_acks(Params, [List_of_hosts]);
send_and_wait(Operation, Params, [List_of_hosts], [H | T]) ->
     io:format("Send data to secondary~n"),
     list_to_pid(H) ! {Operation, Params, secondary, erlang:self()},

     send_and_wait(Operation, Params, [List_of_hosts], T).


%%% Receive ack routine ensure that all other secondary hosts have
%%% received the correct parameters
%%% IF not starts the host recovery routine
receive_acks(_, []) ->
     %SEND REPONSE TO CLIENT WHO ASKED DATA
     io:format("All host have responded~n");
receive_acks(Params, [List_of_hosts]) ->
     receive
          {ack, Params_received, Remote_ID} ->
               %If we have a match we received the expected params
               if Params =:= Params_received ->
                    io:format("Primary: ACK received"),
                    %Delete the host who responded from the list of hosts -> PID OF LISTENER LOOP
                    Remaining_list_of_hosts = delete(pid_to_list(Remote_ID), [List_of_hosts]),
                    %Call untill no host is in the list
                    receive_acks(Params, Remaining_list_of_hosts)
               end
     after 5000 ->
          io:format("Entered host recovery routine ~n"),
          host_register_recovery([List_of_hosts], Params),
          whereis(listener_loop_process) ! {hosts_to_delete, {List_of_hosts}, primary, self()},
          %SEND the response to client after recovery data are stored
          receive_acks(Params, [])
     end.


%the host recovery routine
% CHECKS - if after 5 seconds an ack has not been recived
% For every host not in List_of_responders:
%   Check if is present in the table of unavailable host
%       If is present no action is performed
%       If is not present - will insert in the recovery table his hostname and timestamp of faiulure
%       Broadcast to all the other the failure information's
%
host_register_recovery([], _) ->
     ok;
host_register_recovery([H | T], Params) ->
     io:format("Saving hosts how did not respond ~n"),
     odbc:start(),
     {ok, Ref_to_db} = odbc:connect("dsn=test_;server=localhost;database=taskorganizer;user=root;password=;", []),
     {selected, _, Data} = odbc:param_query(Ref_to_db, "SELECT host FROM recovery_hosts WHERE host=?",
          [{{sql_varchar, 255}, [H]}
          ]),
     if
     %If the host is not already in the database -> register it with new timestamp
     %MUST BE THE SAME ONE AS THE QUERY INSERT TIME!!
          Data == [] ->
               io:format("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA~n"),
               odbc:param_query(Ref_to_db, "INSERT INTO recovery_hosts (host, down_time) VALUES (?,?)",
                    [{{sql_varchar, 255},
                         [H]},
                         {{sql_varchar, 255},
                              [element(1, Params)]}
                    ]),
               io:format("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB~n")
     end,
     host_register_recovery(T, Params).

%host_recovery_routine(Process_id) ->
%recover data from all the tables
%Check if there is an entry in the recovery_hosts table
% IF there is -> Send data from all table since that timestamp
% IF there is not -> Send all the data


% This should be called as a process to notify all the hosts
% A new host is available in the network
server_up_message(_, []) ->
     io:format("All hosts has been notified! ~n");
server_up_message([Updated_list_of_hosts], [Host | T]) ->
     io:format("~p: Sent update to ~p ~n", [self(), Host]),
     Host ! {update_list, Updated_list_of_hosts, secondary, self()},
     server_up_message([Updated_list_of_hosts], T).

index_of([], _, _) ->
     -1;
index_of([Value | _], Value, Index) ->
     Index;
index_of([H | T], Value, Index) ->
     index_of(T, Value, Index+1).
index_of(List, Value)->
     index_of(List, Value, 1).