%%%-------------------------------------------------------------------
%%% @author Riccardo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. feb 2021 18:12
%%%-------------------------------------------------------------------
-module(message_sending_module).
-author("Riccardo").

%% API
-export([send_generic_message_to_list_of_hosts/2, send_ack_to_primary/3,send_update_to_clients/1, send_and_wait/4, server_up_message/2]).
-import(listener_module,[receive_acks/2]).
% Retrieve all RabbitMQ records
%-include_lib("amqp_client/include/amqp_client.hrl").

send_generic_message_to_list_of_hosts(_, []) ->
  ok;
%%send_generic_messages(Message, [PID_host]) ->
%%     io:format("Message: ~p     PID: ~p~n", [Message, PID_host]),
%%     PID_host ! Message;
send_generic_message_to_list_of_hosts(Message, [PID_host | Remaining_hosts]) ->
  PID_host ! Message,
  send_generic_message_to_list_of_hosts(Message, Remaining_hosts).

send_ack_to_primary(From, Params, Listener_process_id) ->
  io:format("Secondary sending ack to primary!~n"),
  From ! {ack, Params, Listener_process_id}.

send_update_to_clients(Params) ->
  ok.
  % TODO send update message to all "interested" clients through RabbitMQ

  %application:ensure_started(amqp_client),
  %{ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
  %{ok, Channel} = amqp_connection:open_channel(Connection),

  %% Publish a message
  % TODO change payload with correct data
  %Payload = <<"foobar">>,
  % TODO check if element is enough or it needs to be changed into a BIN (atom_to_binary)
  %Publish = #'basic.publish'{exchange = <<"topics_boards">>, routing_key = element(1, Params)},
  %amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

  %% Close the channel
  %amqp_channel:close(Channel),
  %% Close the connection
  %amqp_connection:close(Connection).


send_and_wait(_, Params, List_of_hosts, []) ->
  io:format("All data have been sent - Wait for acks~n"),
  receive_acks(Params, List_of_hosts);
send_and_wait(Operation, Params, List_of_hosts, [H | T]) ->
  io:format("Send data to secondary~n"),
  io:format("cosoidas: ~p~n", [{Operation, Params, secondary, self()}]),
  H ! {Operation, Params, secondary, self()},
  send_and_wait(Operation, Params, List_of_hosts, T).

% This should be called as a process to notify all the hosts
% A new host is available in the network
server_up_message(_, []) ->
  io:format("All hosts has been notified! ~n");
server_up_message(Updated_list_of_hosts, [Host | T]) ->
  io:format("~p: Sent update to ~p ~n", [self(), Host]),
  Host ! {update_list, Updated_list_of_hosts, secondary, self()},
  server_up_message(Updated_list_of_hosts, T).