%%%-------------------------------------------------------------------
%% @doc testrebar public API
%% @end
%%%-------------------------------------------------------------------

-module(testrebar_app).

-behaviour(application).

-export([start/2, stop/1, main/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start(_StartType, _StartArgs) ->
    testrebar_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

main(_Args) ->
    % Start connection with RabbitMQ
	application:ensure_started(amqp_client),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host="172.18.0.160"}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	
	Exchange = #'exchange.declare'{exchange = <<"topics_boards">>,
						type = <<"topic">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
	
	Queue1 = #'queue.declare'{
	  queue = <<"gatti">>,
	  durable = true
	},
	#'queue.declare_ok'{} = amqp_channel:call(Channel, Queue1),
	
	Queue2 = #'queue.declare'{
	  queue = <<"cani mandorli">>,
	  durable = true
	},
	#'queue.declare_ok'{} = amqp_channel:call(Channel, Queue2),
	
	Queue3 = #'queue.declare'{
	  queue = <<"cani mandorli e gatti">>,
	  durable = true
	},
	#'queue.declare_ok'{} = amqp_channel:call(Channel, Queue3),
	
	Binding1 = #'queue.bind'{queue = <<"gatti">>,
                        exchange = <<"topics_boards">>,
                        routing_key = <<"gatti">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding1),
	
	Binding2 = #'queue.bind'{queue       = <<"cani mandorli">>,
                        exchange    = <<"topics_boards">>,
                        routing_key = <<"cani mandorli">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding2),
	
	Binding3 = #'queue.bind'{queue       = <<"cani mandorli e gatti">>,
                        exchange    = <<"topics_boards">>,
                        routing_key = <<"cani mandorli">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding3),
	
	Binding4 = #'queue.bind'{queue       = <<"cani mandorli e gatti">>,
                        exchange    = <<"topics_boards">>,
                        routing_key = <<"gatti">>},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding4),
	
	%% Publish a message
	Payload = <<"foobar">>,
	Publish = #'basic.publish'{exchange = <<"topics_boards">>, routing_key = <<"cani mandorli">>},
	amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
		
	%% Close the channel
	amqp_channel:close(Channel),
	%% Close the connection
	amqp_connection:close(Connection).    
