default:
	$(MAKE) clean
	erlc -I $(shell pwd)/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	erl -name 'erlang-starter@172.18.0.1' -pa _build/default/lib/*/ebin -eval 'start_module:start().' -noshell -setcookie 'test'

deploy:
	$(MAKE) -C ../TaskManagerDocker test-node1
	$(MAKE) default

compile:
	erlc -I /run/media/filippo/dati/workspace/Task_manager

clean:
	rm listener_responder.beam || true