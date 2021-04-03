
-module(election_module).

%% API
-export([election_handler/1, ssend/3]).
-import(message_sending_module,[send_generic_message_to_list_of_hosts/2]).
-import(utility_module, [index_of/2]).

%%start_election([List_of_hosts]) ->
%%     ssend_thread(election_vote, [List_of_hosts], []),
%%     election_handler([List_of_hosts]).
%%
%%election_handler([List_of_hosts]) ->
%%     Index = index_of(List_of_hosts, self()),
%%     Higher_prio_list_of_hosts = lists:sublist(List_of_hosts, Index-1),
%%     io:format("~p: List of hosts with higher priority than me ~p~n", [self(), Higher_prio_list_of_hosts]),
%%     send_generic_message_to_list_of_hosts({election_vote, self()}, Higher_prio_list_of_hosts),
%%
%%     receive
%%          {election_victory, Remaining_hosts, secondary,From} ->
%%               io:format("~p: Received victory_election from ~p ~n", [self(), From]),
%%               self() ! {election_victory, Remaining_hosts, secondary,From};
%%          {election_vote, From} ->
%%               From ! {ack_election_vote, self()},
%%               election_handler([List_of_hosts]);
%%          {ack_election_vote, From} ->
%%               io:format("~p: Received ack_election_vote from ~p, my job is done. Waiting for election_victory...~n", [self(), From]);
%%
%%
%%          Message ->
%%               ok,
%%               io:format("dosandoasmndas ~p~n", [Message]);
%%          _ ->
%%               io:format("~p: received undefined message inside election handler~n", [self()]),
%%               exit(undefined_message)
%%     after 1000 ->
%%          io:format("~p: Either I am the highest priority or all higher priority servers are down. I am going to candidate as Primary~n",[self()]),
%%          Remaining_hosts = lists:sublist(List_of_hosts, Index+1, length(List_of_hosts)),
%%          io:format("~p: Remaining hosts in the cluster after election ~p~n",[self(), Remaining_hosts]),
%%          % Send results of election to all servers inside the cluster
%%          send_generic_message_to_list_of_hosts({election_victory, Remaining_hosts, secondary, self()}, Remaining_hosts),
%%          self() ! {election_victory, Remaining_hosts, primary, self()}
%%     end.
%%
%%
%%
%%
%%
%%ssend(Message, [], [List_of_hosts_received_ack], Election_module_pid) ->
%%     case Message of
%%          election_vote  ->
%%               Ack_atom = ack_election_vote;
%%          election_victory ->
%%               Ack_atom = ack_election_victory;
%%          _ ->
%%               io:format("Incorrect message format in ssend election module~n"),
%%               Ack_atom = undefined
%%     end,
%%     receive
%%          {Ack_atom, From} ->
%%               New_list_of_hosts_received_ack = [List_of_hosts_received_ack | From],
%%               ssend(Message, [], [New_list_of_hosts_received_ack], Election_module_pid);
%%          % Probably some other hosts started a new election instance
%%          {election_vote, From} ->
%%               From ! {ack_election_vote, self()},
%%               election_handler([List_of_hosts]);
%%     after 1000 ->
%%          Election_module_pid ! {online_list_of_hosts, List_of_hosts_received_ack}
%%     end;
%%
%%ssend(Message, [PID | Remaining_list_of_hosts], [], Election_module_pid) ->
%%     PID ! erlang:insert_element(size(Message)+1, Message, self()),
%%     ssend(Message, Remaining_list_of_hosts, [], Election_module_pid).









% TODO
wait_ack(Message) ->
     receive
          {ack, _} ->
               host_online
     after 500 ->
          host_offline
     end.

% true ssend
ssend(Message, Pid, Result_pid) ->
     Formatted_msg = erlang:insert_element(size(Message)+1, Message, self()),
     Pid ! Formatted_msg,
     Result = wait_ack(Message),
     case Result of
          host_online ->
               io:format("Host ~p is online ~n", [Pid]),
               Result_pid ! {host_online, Pid};
          host_offline ->
               io:format("Host ~p is offline ~n", [Pid]),
               Result_pid ! {host_offline, Pid};
          _ ->
               io:format("undefined format~n"),
               {error, undefined_format}
     end.
     

ssend_election_votes([], Num) ->
     wait_results_from_threads([], Num);
ssend_election_votes([Pid | List_of_hosts], Num) ->
     spawn(election_module, ssend, [{election_vote}, Pid, self()]),
     ssend_election_votes(List_of_hosts, Num+1).

ssend_election_victory([], [], 0) ->
     [];
ssend_election_victory([], [_], Num) ->
     wait_results_from_threads([], Num);
ssend_election_victory([Pid | List_of_hosts], Remaining_hosts, Num) ->
     spawn(election_module, ssend, [{election_victory, Remaining_hosts, secondary, self()}, Pid, self()]),
     ssend_election_victory(List_of_hosts, Remaining_hosts, Num+1).

wait_results_from_threads([], 0) ->
     [];
wait_results_from_threads([], Num) ->
     receive
          {host_online, From} ->
               wait_results_from_threads([From], Num - 1);
          {host_offline, From} ->
               wait_results_from_threads([], Num - 1);
          {election_vote, From} ->
               From ! {ack, {election_vote, From}},
               wait_results_from_threads([], Num)
     end;
wait_results_from_threads(Online_hosts, 0) ->
     [Online_hosts];
wait_results_from_threads([Online_hosts], Num) ->
     receive
          {host_online, From} ->
               New_online_hosts = [Online_hosts | From],
               wait_results_from_threads(New_online_hosts, Num - 1);
          {host_offline, From} ->
               wait_results_from_threads(Online_hosts, Num - 1);
          {election_vote, From} ->
               From ! {ack, {election_vote, From}},
               wait_results_from_threads(Online_hosts, Num)
     end.



election_handler([List_of_hosts]) ->
     Index = index_of(List_of_hosts, self()),
     Higher_prio_list_of_hosts = lists:sublist(List_of_hosts, Index-1),
     io:format("~p: List of hosts with higher priority than me ~p~n", [self(), Higher_prio_list_of_hosts]),
     % Result contains list of online hosts with higher priority
     Result = ssend_election_votes(Higher_prio_list_of_hosts, 0),
     case Result of
          % If no other hosts received the message then i am the new primary
          [] ->
               io:format("~p: Either I am the highest priority or all higher priority servers are down. I am going to candidate as Primary~n",[self()]),
               Remaining_hosts = lists:sublist(List_of_hosts, Index+1, length(List_of_hosts)),
               ssend_election_victory(Remaining_hosts, Remaining_hosts, 0),
               io:format("~p: Remaining hosts in the cluster after election ~p~n",[self(), Remaining_hosts]),
               self() ! {election_victory, Remaining_hosts, primary, self()};
          % For sure i am not the new primary, wait election_victory
          [Online_hosts] ->
               receive
                    {election_victory, Remaining_hosts, secondary, From, Thread_ack_pid} ->
                         Thread_ack_pid ! {ack, {election_victory, Remaining_hosts, secondary,From}},
                         io:format("~p: Received victory_election from ~p ~n", [self(), From]),
                         self() ! {election_victory, Remaining_hosts, secondary, From}
               end
     end.
     
     
     
     
     
     
     
     
     
     
     
%%
%%
%%ssend_thread(Message, [Remaining_list_of_hosts], []) ->
%%     spawn(election_module, ssend, [Message, [Remaining_list_of_hosts], [], self()]),
%%     receive
%%          {online_list_of_hosts, List_of_hosts_received_ack} ->
%%               List_of_hosts_received_ack
%%     end.
%%
%%
%%election_handler([List_of_hosts]) ->
%%     Index = index_of(List_of_hosts, self()),
%%     Higher_prio_list_of_hosts = lists:sublist(List_of_hosts, Index-1),
%%     io:format("~p: List of hosts with higher priority than me ~p~n", [self(), Higher_prio_list_of_hosts]),
%%     % Result contains list of online hosts with higher priority
%%     Result = ssend_thread({election_vote}, Higher_prio_list_of_hosts),
%%     case Result of
%%          % If no other hosts received the message then i am the new primary
%%          [] ->
%%               Remaining_hosts = lists:sublist(List_of_hosts, Index+1, length(List_of_hosts)),
%%               ssend_thread({election_victory, Remaining_hosts, secondary}, []),
%%               self() ! {election_victory, Remaining_hosts, primary, self()};
%%          [List_of_hosts_received_ack] ->
%%               % For sure i am not the new primary, wait election_victory
%%               receive
%%                    {election_victory, Remaining_hosts, secondary,From} ->
%%                         From ! {ack_election_victory, self()},
%%                         io:format("~p: Received victory_election from ~p ~n", [self(), From]),
%%                         self() ! {election_victory, Remaining_hosts, secondary,From}
%%               end
%%     end.
%%
%%
%%
%%
%%% NOPE
%%send_election_votes([], []) ->
%%     io:format("~p: Either I am the highest priority or all higher priority servers are down. I am going to candidate as Primary~n",[self()]),
%%     primary;
%%send_election_votes([], [Online_hosts]) ->
%%     io:format("~p: Received ack_election_vote from ~p, my job is done. Waiting for election_victory...~n", [self(), From]),
%%     not_primary;
%%send_election_votes([PID | List_of_hosts], [Online_hosts]) ->
%%     Ret = rpc:call(PID, election_module, election_handler, [[List_of_hosts]]),
%%     case Ret of
%%          {badrpc, Reason} ->
%%               io:format("Error starting election procedure on node : ~p with reason: ~p~n", [PID, Reason]);
%%          _ ->
%%               New_online_hosts = [Online_hosts | PID],
%%               ok
%%     end,
%%     send_election_votes([List_of_hosts]).