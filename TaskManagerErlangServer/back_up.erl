%%%-------------------------------------------------------------------
%%% @author Riccardo
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. feb 2021 17:16
%%%-------------------------------------------------------------------
-module(back_up).
-author("Riccardo").

%% API
-export([]).

receive_acks(Params, [List_of_hosts], []) ->
  receive
    {ack, Params_received, Remote_ID}->
      if
      %check if this was an expected ack
        Params =:= Params_received ->
          io:format("Primary: ACK received"),
          %Delete from the list the host who sent the message
          New_update_list = delete(pid_to_list(Remote_ID), List_of_hosts ),
          if [Remote_ID] /= [List_of_hosts] ->
            receive_acks(Params, [List_of_hosts], [Remote_ID])
          end;
      %The Params received was not for this process
        true ->
          receive_acks(Params, [List_of_hosts], [])
      end
  after 5 ->
    %this routine will store the data in case of
    %an host goes down
    host_register_recovery([List_of_hosts])
  end
;

receive_acks(Params, [List_of_hosts], [List_of_responders])->
  receive
    {ack, Params_received, Remote_ID}->
      if
      %check if this was an expected ack
        Params =:= Params_received ->
          io:format("Primary: ACK received"),
          [Remote_ID|List_of_responders],
          %check if the responders list is the same as the sender list
          if [List_of_hosts] /= [List_of_responders] ->
            receive_acks(Params, [List_of_hosts], [List_of_responders])
          end;
      %The Params received was not for this process
        true ->
          receive_acks(Params, [List_of_hosts], [List_of_responders])
      end
  %after 5 ->
  %this routine will store the data in case of
  %an host goes down
  % Host in List_of_hosts - List_of_responders
  %host_recovery_routine([List_of_hosts],[List_of_responders])
  end.
%must be defined - sends the new operation to the queue (Guggino)
%send_response_to_client().
