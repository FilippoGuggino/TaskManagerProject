-module(election_module).

%% API
-export([election_handler/1]).
-import(message_sending_module, [send_generic_message_to_list_of_hosts/2]).
-import(utility_module, [index_of/2]).

election_handler([List_of_hosts]) ->
     Index = index_of(List_of_hosts, self()),
     Higher_prio_list_of_hosts = lists:sublist(List_of_hosts, Index - 1),
     io:format("~p: List of hosts with higher priority than me ~p~n", [self(), Higher_prio_list_of_hosts]),
     send_generic_message_to_list_of_hosts({election_vote, self()}, Higher_prio_list_of_hosts),
     receive
          {election_victory, Remaining_hosts, secondary, From} ->
               io:format("~p: Received victory_election from ~p ~n", [self(), From]),
               self() ! {election_victory, Remaining_hosts, secondary, From};
          {election_vote, From} ->
               From ! {ack_election_vote, self()},
               election_handler([List_of_hosts]);
          {ack_election_vote, From} ->
               io:format("~p: Received ack_election_vote from ~p, my job is done. Waiting for election_victory...~n", [self(), From]);
          Message ->
               ok,
               io:format("dosandoasmndas ~p~n", [Message]);
          _ ->
               io:format("~p: received undefined message inside election handler~n", [self()]),
               exit(undefined_message)
     after 1000 ->
          io:format("~p: Either I am the highest priority or all higher priority servers are down. I am going to candidate as Primary~n", [self()]),
          Remaining_hosts = lists:sublist(List_of_hosts, Index + 1, length(List_of_hosts)),
          io:format("~p: Remaining hosts in the cluster after election ~p~n", [self(), Remaining_hosts]),
          % Send results of election to all servers inside the cluster
          send_generic_message_to_list_of_hosts({election_victory, Remaining_hosts, secondary, self()}, Remaining_hosts),
          self() ! {election_victory, Remaining_hosts, primary, self()}
     end.