build:
	docker network create --subnet=172.18.0.0/16 DistributedSystems || true
	$(MAKE) -C TaskManagerDocker build-node1
	$(MAKE) -C TaskManagerDocker build-node2
	$(MAKE) -C TaskManagerDocker build-node3

	$(MAKE) -C TaskManagerDocker build-webserver-1
	$(MAKE) -C TaskManagerDocker build-webserver-2

deploy:
	$(MAKE) -C TaskManagerDocker start-rabbit
	$(MAKE) -C TaskManagerErlangServer deploy-multiple-nodes

	$(MAKE) -C TaskManagerDocker start-multiple-webserver
	$(MAKE) -C TaskManagerDocker build-haproxy

	$(MAKE) -C TaskManagerErlangServer start-multiple

restart-node2:
	docker start mysql2 || true
	docker exec -ti mysql2 /bin/sh -c 'kill -9 $$(pgrep erl_child_setup) || true'
	docker exec -ti -w /TaskManagerErlangServer mysql2 erlc -I /TaskManagerErlangServer/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	sleep 5
	docker exec -ti -w /TaskManagerErlangServer mysql2 /bin/sh -c "erl -name erlang-server@172.18.0.163 -pa _build/default/lib/*/ebin -eval 'A = rpc:call(list_to_atom(\"erlang-server@172.18.0.162\"),erlang,whereis, [listener_loop_process] ), start_module:init([A], secondary).' -setcookie test"