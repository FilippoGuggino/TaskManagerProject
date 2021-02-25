-module(start_module).


%% API
-export([start/0, init/2]).
-import(listener_module,[listener_loop/4]).
-import(testing_module, [client_test/2]).

%This is a testing method to try multiple processes
start() ->
  io:format("Starting~n"),
  PID_primary = spawn('erlang-server@172.18.0.162', ?MODULE, init, [[], primary]),
%%  timer:sleep(1000),
%%
%%  %Spawn multiple secondary nodes
%%  PID_secondary = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  timer:sleep(1000),
%%  PID_secondary3 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  timer:sleep(1000),
%%  PID_secondary2 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  timer:sleep(1000),
%%  PID_secondary4 = spawn(?MODULE, init, [[PID_primary], secondary]),
%%  % register(primary_process, PID_primary),
%%  timer:sleep(2000),
%%  testing_module:client_test("A", PID_primary).
  %exit(PID_primary, testing_election)
  spawn(testing_module, client_test, ["Ciao", PID_primary]).


%Stating node -
% Primary: will also connect to RabbitMQ
% Secondary: will ask primary for updates on database
% Both of them will call listener loop to receive message
init(List_of_hosts, Server_type) ->
  odbc:start(),
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