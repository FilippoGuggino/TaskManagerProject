default:
	$(MAKE) run
	
test-node1:
	docker start mysql1 || true
	
	docker exec -ti mysql1 /bin/sh -c 'kill -9 $$(pgrep erl_child_setup) || true'
	docker exec -ti mysql1 rm -rf /TaskManagerErlangServer/Task_manager_modules
	docker cp ../TaskManagerErlangServer/Task_manager_modules mysql1:/TaskManagerErlangServer/
	
	docker exec -ti -w /TaskManagerErlangServer mysql1 rebar3 compile
	
	docker exec -ti -w /TaskManagerErlangServer mysql1 erlc -I /TaskManagerErlangServer/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	docker exec -dti -w /TaskManagerErlangServer mysql1 /bin/sh -c 'erl -name erlang-server@172.18.0.162 -pa _build/default/lib/*/ebin -setcookie test'

start-rabbit:
	$(MAKE) clean-rabbit
	
	docker run -d --hostname rabbitmq --name rabbitmq --net DistributedSystems --ip 172.18.0.160 -e RABBITMQ_ERLANG_COOKIE=test rabbitmq:3-management

test-node2:
	docker start mysql2 || true

	docker exec -ti mysql2 /bin/sh -c 'kill -9 $$(pgrep erl_child_setup) || true'
	docker exec -ti mysql2 rm -rf /TaskManagerErlangServer/Task_manager_modules
	docker cp ../TaskManagerErlangServer/Task_manager_modules mysql2:/TaskManagerErlangServer/
	docker cp ../TaskManagerErlangServer/rebar.config mysql2:/TaskManagerErlangServer/

	docker exec -ti -w /TaskManagerErlangServer mysql2 rebar3 compile

	docker exec -ti -w /TaskManagerErlangServer mysql2 erlc -I /TaskManagerErlangServer/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	docker exec -dti -w /TaskManagerErlangServer mysql2 /bin/sh -c 'erl -name erlang-server@172.18.0.163 -pa _build/default/lib/*/ebin -setcookie test'
	docker exec -dti mysql2 /bin/sh

clean-rabbit:
	docker stop rabbitmq || true
	docker rm rabbitmq || true

clean-node1:
	docker stop mysql1 || true
	docker rm mysql1 || true
	docker rmi distributed-systems/alpine-mysql1 || true
	rm alpine-mysql1/erlang-src/listener_responder.erl || true
	rm alpine-mysql1/TaskOrganizerDB.sql || true


clean-node2:
	docker stop mysql2 || true
	docker rm mysql2 || true
	docker rmi distributed-systems/alpine-mysql2 || true
	rm alpine-mysql2/erlang-src/listener_responder.erl || true
	rm alpine-mysql2/TaskOrganizerDB.sql || true

clean:
	$(MAKE) clean-node1
	$(MAKE) clean-node2

build-node1:
	$(MAKE) clean-node1
	cp TaskOrganizerDB.sql alpine-mysql1/TaskOrganizerDB.sql
	cp listener_responder.erl alpine-mysql1/erlang-src/listener_responder.erl
	docker build -t distributed-systems/alpine-mysql1 ./alpine-mysql1

	docker run -it --name mysql1 --net DistributedSystems --ip 172.18.0.162 -p 3306:3306 -v $(shell pwd)/alpine-mysql1:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD='password' -e FUN='$(FUN)' distributed-systems/alpine-mysql1

	docker cp odbc.ini mysql1:/etc/odbc.ini
	docker cp odbcinst.ini mysql1:/etc/odbcinst.ini

build-node2:
	$(MAKE) clean-node2
	cp TaskOrganizerDB.sql alpine-mysql2/TaskOrganizerDB.sql
	cp listener_responder.erl alpine-mysql2/erlang-src/listener_responder.erl
	docker build -t distributed-systems/alpine-mysql2 ./alpine-mysql2

	docker run -it --name mysql2 --net DistributedSystems --ip 172.18.0.163 -v $(shell pwd)/alpine-mysql2:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD=password -e TEST=true -e FUN='$(FUN)' distributed-systems/alpine-mysql2

	docker cp odbc.ini mysql2:/etc/odbc.ini
	docker cp odbcinst.ini mysql2:/etc/odbcinst.ini

build:
	$(MAKE) clean
	$(MAKE) build-node1
	$(MAKE) build-node2

deploy-node1:
	$(MAKE) build-node1
	docker run -dit --name mysql1 --net DistributedSystems --ip 172.18.0.162 -v $(pwd)/alpine-mysql1:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD=password distributed-systems/alpine-mysql1

deploy-node2:
	$(MAKE) build-node2
	docker run -dit --name mysql2 --net DistributedSystems --ip 172.18.0.163 -v $(pwd)/alpine-mysql2:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD=password distributed-systems/alpine-mysql2

deploy:
	$(MAKE) deploy-node1
	$(MAKE) deploy-node2