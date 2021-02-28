
-module(recover_node_module).


%% API
-export([host_register_recovery/2, recovery_routine/2, server_up_message/2]).
-import(query_module, [load_tasks_recovery/1,check_host_recovery_registered/1, insert_host_recovery/2, load_tasks_db/1, load_boards_db/0, load_boards_recovery/1, delete_host_from_recovery/1]).
-import(message_sending_module,[send_and_wait/4]).
%%%%%%%%%%%%%%%%%%%%%% HOST FAILURE ROUTINE: REGISTRATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECKS - if after 5 seconds an ack has not been recived
% For every host not in List_of_responders:
%   Check if is present in the table of unavailable host
%       If is present no action is performed
%       If is not present - will insert in the recovery table his hostname and timestamp of faiulure
%       Broadcast to all the other the failure information's
%
host_register_recovery([], _) ->
  ok;
host_register_recovery([H|T], Params) ->
  io:format("RECOVERY: Saving hosts who did not respond ~n"),
  Data= check_host_recovery_registered(H),
  if
  %If the host is not already in the database -> register it with new timestamp
  %MUST BE THE SAME ONE AS THE QUERY INSERT TIME
    Data == [] ->
      io:format("~p", [node(H)]),
      insert_host_recovery(H, Params)
  end,
  host_register_recovery(T, Params).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%host_recovery_routine(Process_id) ->
%recover data from all the tables
%Check if there is an entry in the recovery_hosts table
% IF there is -> Send data from all table since that timestamp
% IF there is not -> Send all the data

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NEW SERVER UP BROADCAST MESSAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This should be called as a process to notify all the hosts
% A new host is available in the network
server_up_message(_, []) ->
  io:format("All hosts has been notified! ~n");
server_up_message([Updated_list_of_hosts], [Host | T]) ->
  io:format("~p: Sent update to ~p ~n", [self(), Host]),
  Host ! {update_list, Updated_list_of_hosts, secondary, self()},
  server_up_message([Updated_list_of_hosts], T).

getFirst([H|_]) ->
  H.

recovery_routine(Process_id, List_of_hosts) ->
  io:format("PRIMARY: starting recovery routine for secondary~n"),
  %Process_id ! {ack_new_server_up, List_of_hosts, self()},
  Data_query = check_host_recovery_registered(Process_id),

  %io:format(Data),
  %io:format(utility_module:isolate_element(Data,1)),
  if
    Data_query == [] ->
      %    io:format("------------ HOST FOUND: This host is in the recovery mode ~n"),
      Time = "0";
    true ->
      Data = getFirst(Data_query),
      io:format("------------ HOST FOUND: This host is in the recovery mode ~n"),
      Time = element(1,Data),
      io:format(Time)
  end,
  %Time = "0",
  %GET BOARD AND SEND BOARDS
  Boards = load_boards_recovery(Time),
  %SENDING ALL THE BOARDS
  if
    Boards == [] -> ok;
    true ->
      send_and_wait(create_boards, Boards, [Process_id], [Process_id])
  end,


  %GET STAGES AND SEND STAGES
  %Stages = get_stages_db(Time),
  %SENDING ALL THE STAGES
  %send_and_wait(create_stages, Stages, [Process_id], [Process_id]),

  %GET TASKS AND SEND TASKS
  %utility_module:isolate_element(Time,1)
  Tasks = load_tasks_recovery(Time),
  %SENDING ALL THE TASKS
  if
    Tasks == [] -> ok;
    true ->
      send_and_wait(create_tasks, Tasks, [Process_id], [Process_id])
  end,

  %DELETE THE HOST FROM THE RECOVERY TABLE. (Only if all Data are OK)
  delete_host_from_recovery(Process_id).