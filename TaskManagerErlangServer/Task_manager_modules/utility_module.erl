-module(utility_module).

-include_lib("amqp_client/include/amqp_client.hrl").
%% API
-export([send_update_to_rabbitmq/3, get_timestamp_a/0, delete_hosts_from_list/2, isolate_element/2, index_of/2, index_of/3]).

-spec get_timestamp_a() -> integer().
get_timestamp_a() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).


%Delete a list of host from the available list of hosts
%delete_hosts_from_list(List, []) ->
%  List;
delete_hosts_from_list(List_hosts, List_failure)->
  Updated_list = List_hosts -- List_failure,
  Updated_list.
  %Updated_list = lists:delete(H, [List]),
  %delete_hosts_from_list(Updated_list, T).


%Take element from array of touples [{a,b}, {c,d}]
isolate_element(List, Index_to_isolate) ->
  [element(Index_to_isolate,Elemt) || Elemt <- List].


%Get index of an element
index_of([], _, _) ->
  -1;
index_of([Value | _], Value, Index) ->
  Index;
index_of([H | T], Value, Index) ->
  index_of(T, Value, Index+1).
index_of(List, Value)->
  index_of(List, Value, 1).

send_update_to_rabbitmq(Operation, Params, Board_title) ->
  application:ensure_started(amqp_client),
  {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "172.18.0.160"}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  
  Exchange_name = <<"topics_boards">>,
  
  Payload = term_to_binary({Operation, Params}),
  
  Publish = #'basic.publish'{exchange = Exchange_name, routing_key = Board_title},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
  
  %% Close the channel
  amqp_channel:close(Channel),
  %% Close the connection
  amqp_connection:close(Connection).