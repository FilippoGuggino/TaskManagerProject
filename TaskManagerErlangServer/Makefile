start-localhost:
	$(MAKE) clean
	erlc -I $(shell pwd)/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	erl -name 'erlang-starter@172.18.0.1' -pa _build/default/lib/*/ebin -eval 'start_module:start_localhost().' -noshell -setcookie 'test'

start:
	$(MAKE) clean
	erlc -I $(shell pwd)/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	erl -name 'erlang-starter@172.18.0.1' -pa _build/default/lib/*/ebin -eval 'start_module:start().' -noshell -setcookie 'test'

deploy-single-node:
	$(MAKE) -C ../TaskManagerDocker test-node1
	$(MAKE) start

deploy-multiple-nodes:
	$(MAKE) -C ../TaskManagerDocker test-node1
	$(MAKE) -C ../TaskManagerDocker test-node2
	$(MAKE) start

compile:
	erlc -I /run/media/filippo/dati/workspace/Task_manager

clean:
	rm listener_responder.beam || true