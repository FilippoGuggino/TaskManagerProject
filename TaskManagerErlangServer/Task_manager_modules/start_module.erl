-module(start_module).


%% API
-export([start/0, init/2, start_localhost/0]).
-import(listener_module, [listener_loop/4]).

-include_lib("amqp_client/include/amqp_client.hrl").

start_localhost() ->
     io:format("Starting~n"),
     PID_primary = spawn(?MODULE, init, [[], primary]).

%This is a testing method to try multiple processes
start() ->
     io:format("Starting~n"),
     PID_primary = spawn('erlang-server@172.18.0.162', start_module, init, [[], primary]),
     %unregister(listener_loop_process),
%%  timer:sleep(1000),
%%
%%  %Spawn multiple secondary nodes
     PID_secondary = spawn('erlang-server@172.18.0.163', start_module, init, [[PID_primary], secondary]),
     %timer:sleep(30000),
     %io:format("----------------- SECONDARY DOWN ------------------~n"),
     %exit(PID_secondary, testing_election),

     timer:sleep(10000),
%%  PID_secondary3 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  timer:sleep(1000),
%%  PID_secondary2 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  timer:sleep(1000),
%%  PID_secondary4 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  % register(primary_process, PID_primary),
%%  timer:sleep(2000),
	io:format("----------------- TESTING HOST FAILURE ------------------~n"),
	exit(PID_secondary, testing_election),
	testing_module:client_test("A", PID_primary),
	timer:sleep(50000),
	io:format("------------------- SECONDARY IS UP ------------------~n"),
	spawn('erlang-server@172.18.0.163', start_module, init, [[PID_primary], secondary]).
	%spawn('erlang-server@172.18.0.163', start_module, init, [[PID_primary], secondary]).
%%     exit(PID_primary, testing_election).
% spawn(?MODULE, client_test, ["Ciao", PID_primary]).


%Stating node -
% Primary: will also connect to RabbitMQ
% Secondary: will ask primary for updates on database
% Both of them will call listener loop to receive message
init(List_of_hosts, Server_type) ->
     odbc:start(),
     case Server_type of
          % This host is the primary
          primary ->
          	   register(listener_loop_process, self()),
               io:format("~p: sono il primario~n", [self()]),
               % Start connection with RabbitMQ
               application:ensure_started(amqp_client),
               {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
               {ok, Channel} = amqp_connection:open_channel(Connection),

               Declare = #'queue.declare'{queue = <<"my_queue">>},
               #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),

               Primary_queue_name = <<"primary_queue">>,
               Exchange_name = <<"topics_boards">>,
               % Create new Exchange, used to dispatch updates to "interested" clients
               Exchange = #'exchange.declare'{exchange = Exchange_name,
                    type = <<"topic">>},
               #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
     
               % Create queue containing a message with the PID of the primary node
               % This message will be used from every web-server whenever they can't receive an ack from the erlang-server
               Primary_queue = #'queue.declare'{queue = Primary_queue_name},
               #'queue.declare_ok'{} = amqp_channel:call(Channel, Primary_queue),
     
               Routing_key = <<"primary_pid">>,
               Binding = #'queue.bind'{queue       = Primary_queue_name,
                                        exchange    = Exchange_name,
                                        routing_key = Routing_key},
               #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
     
               % TODO see how this value can be used from java web server
               Payload = term_to_binary(self()),

               Publish = #'basic.publish'{exchange = Exchange_name, routing_key = Routing_key},
               amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
               
               %% Close the channel
               amqp_channel:close(Channel),
               %% Close the connection
               amqp_connection:close(Connection),
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
