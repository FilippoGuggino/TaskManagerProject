# Erlang servers
test-node1:
	docker start mysql1 || true

	docker exec -ti mysql1 /bin/sh -c 'kill -9 $$(pgrep erl_child_setup) || true'
	docker exec -ti mysql1 rm -rf /TaskManagerErlangServer/Task_manager_modules
	docker cp ../TaskManagerErlangServer/Task_manager_modules mysql1:/TaskManagerErlangServer/

	docker exec -ti -w /TaskManagerErlangServer mysql1 rebar3 compile

	docker exec -ti -w /TaskManagerErlangServer mysql1 erlc -I /TaskManagerErlangServer/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	sleep 3
	docker exec -dti -w /TaskManagerErlangServer mysql1 /bin/sh -c 'erl -name erlang-server@172.18.0.162 -pa _build/default/lib/*/ebin -setcookie test'

test-node2:
	docker start mysql2 || true

	docker exec -ti mysql2 /bin/sh -c 'kill -9 $$(pgrep erl_child_setup) || true'
	docker exec -ti mysql2 rm -rf /TaskManagerErlangServer/Task_manager_modules
	docker cp ../TaskManagerErlangServer/Task_manager_modules mysql2:/TaskManagerErlangServer/
	docker cp ../TaskManagerErlangServer/rebar.config mysql2:/TaskManagerErlangServer/

	docker exec -ti -w /TaskManagerErlangServer mysql2 rebar3 compile

	docker exec -ti -w /TaskManagerErlangServer mysql2 erlc -I /TaskManagerErlangServer/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	sleep 3
	docker exec -dti -w /TaskManagerErlangServer mysql2 /bin/sh -c 'erl -name erlang-server@172.18.0.163 -pa _build/default/lib/*/ebin -setcookie test'

test-node3:
	docker start mysql3 || true

	docker exec -ti mysql3 /bin/sh -c 'kill -9 $$(pgrep erl_child_setup) || true'
	docker exec -ti mysql3 rm -rf /TaskManagerErlangServer/Task_manager_modules
	docker cp ../TaskManagerErlangServer/Task_manager_modules mysql3:/TaskManagerErlangServer/
	docker cp ../TaskManagerErlangServer/rebar.config mysql3:/TaskManagerErlangServer/

	docker exec -ti -w /TaskManagerErlangServer mysql3 rebar3 compile

	docker exec -ti -w /TaskManagerErlangServer mysql3 erlc -I /TaskManagerErlangServer/_build/default/lib/ Task_manager_modules/start_module.erl Task_manager_modules/testing_module.erl Task_manager_modules/utility_module.erl Task_manager_modules/election_module.erl Task_manager_modules/listener_module.erl Task_manager_modules/message_sending_module.erl Task_manager_modules/query_module.erl Task_manager_modules/recover_node_module.erl
	sleep 3
	docker exec -dti -w /TaskManagerErlangServer mysql3 /bin/sh -c 'erl -name erlang-server@172.18.0.164 -pa _build/default/lib/*/ebin -setcookie test'

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

clean-node3:
	docker stop mysql3 || true
	docker rm mysql3 || true
	docker rmi distributed-systems/alpine-mysql3 || true
	rm alpine-mysql3/erlang-src/listener_responder.erl || true
	rm alpine-mysql3/TaskOrganizerDB.sql || true

clean:
	$(MAKE) clean-node1
	$(MAKE) clean-node2
	$(MAKE) clean-node3

build-node1:
	$(MAKE) clean-node1
	chmod +x alpine-mysql1/startup.sh
	cp TaskOrganizerDB.sql alpine-mysql1/TaskOrganizerDB.sql
	cp listener_responder.erl alpine-mysql1/erlang-src/listener_responder.erl
	docker build -t distributed-systems/alpine-mysql1 ./alpine-mysql1

	docker run -it --name mysql1 --net DistributedSystems --ip 172.18.0.162 -p 3306:3306 -v $(shell pwd)/alpine-mysql1:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD='password' -e FUN='$(FUN)' distributed-systems/alpine-mysql1

	docker cp odbc.ini mysql1:/etc/odbc.ini
	docker cp odbcinst.ini mysql1:/etc/odbcinst.ini
	docker cp ../TaskManagerErlangServer mysql1:/
	docker stop mysql1 || true
	docker start mysql1 || true

build-node2:
	$(MAKE) clean-node2
	chmod +x alpine-mysql2/startup.sh
	cp TaskOrganizerDB.sql alpine-mysql2/TaskOrganizerDB.sql
	cp listener_responder.erl alpine-mysql2/erlang-src/listener_responder.erl
	docker build -t distributed-systems/alpine-mysql2 ./alpine-mysql2

	docker run -it --name mysql2 --net DistributedSystems --ip 172.18.0.163 -v $(shell pwd)/alpine-mysql2:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD=password -e TEST=true -e FUN='$(FUN)' distributed-systems/alpine-mysql2

	docker cp odbc.ini mysql2:/etc/odbc.ini
	docker cp odbcinst.ini mysql2:/etc/odbcinst.ini
	docker cp ../TaskManagerErlangServer mysql2:/
	docker stop mysql2 || true
	docker start mysql2 || true

build-node3:
	$(MAKE) clean-node3
	chmod +x alpine-mysql3/startup.sh
	cp TaskOrganizerDB.sql alpine-mysql3/TaskOrganizerDB.sql
	docker build -t distributed-systems/alpine-mysql3 ./alpine-mysql3

	docker run -it --name mysql3 --net DistributedSystems --ip 172.18.0.164 -v $(shell pwd)/alpine-mysql3:/app -e MYSQL_DATABASE=TaskOrganizer -e MYSQL_USER=distributed -e MYSQL_PASSWORD=password -e MYSQL_ROOT_PASSWORD=password -e TEST=true -e FUN='$(FUN)' distributed-systems/alpine-mysql3

	docker cp odbc.ini mysql3:/etc/odbc.ini
	docker cp odbcinst.ini mysql3:/etc/odbcinst.ini
	docker cp ../TaskManagerErlangServer mysql3:/
	docker stop mysql3 || true
	docker start mysql3 || true

build:
	$(MAKE) clean
	$(MAKE) build-node1
	$(MAKE) build-node2
	$(MAKE) build-node3



# RabbitMQ
start-rabbit:
	$(MAKE) clean-rabbit

	docker run -d --hostname rabbitmq --name rabbitmq --net DistributedSystems --ip 172.18.0.160 -e RABBITMQ_ERLANG_COOKIE=test rabbitmq:3-management

clean-rabbit:
	docker stop rabbitmq || true
	docker rm rabbitmq || true


deploy-node1:
	$(MAKE) build-node1
	docker start mysql1

deploy-node2:
	$(MAKE) build-node2
	docker start mysql2

deploy-node3:
	$(MAKE) build-node3
	docker start mysql3

deploy:
	$(MAKE) deploy-node1
	$(MAKE) deploy-node2
	$(MAKE) deploy-node3


# Tomcat Webserver
start-single-webserver:
	$(MAKE) start-webserver-1

start-multiple-webserver:
	$(MAKE) start-webserver-1
	$(MAKE) start-webserver-2

start-webserver-1:
	docker stop tomcat-webserver-1
	# Compile and create packege of new project
	mvn package -f ../TaskOrganizerWebService

	docker cp ../TaskOrganizerWebService/target/TaskOrganizerWebService-1.0-SNAPSHOT.war tomcat-webserver-1:/usr/local/tomcat/webapps

	docker start tomcat-webserver-1

	# Process needed to create an erlang node, theoretically this should start automatically at startup...
	docker exec -dti tomcat-webserver-1 epmd

build-webserver-1:
	$(MAKE) clean-tomcat-webserver-1
	docker build -t distributed-systems/tomcat-webserver-1 ./tomcat-webserver-1
	docker run -dit --name tomcat-webserver-1 --net DistributedSystems --ip 172.18.0.151 -p 8080 -v $(shell pwd)/tomcat-webserver-1:/app  distributed-systems/tomcat-webserver-1
	docker exec -i -w /usr/local/tomcat/ tomcat-webserver-1 /bin/sh -c 'cp -r webapps.dist/* webapps/'
	docker cp ConfigTomcatDocker/context.xml tomcat-webserver-1:/usr/local/tomcat/webapps/manager/META-INF/
	docker cp ConfigTomcatDocker/tomcat-users.xml tomcat-webserver-1:/usr/local/tomcat/conf
	docker stop tomcat-webserver-1 || true

clean-tomcat-webserver-1:
	docker stop tomcat-webserver-1 || true
	docker rm tomcat-webserver-1 || true
	docker rmi tomcat-webserver-1 || true

start-webserver-2:
	docker stop tomcat-webserver-2
	# Compile and create packege of new project
	mvn package -f ../TaskOrganizerWebService

	docker cp ../TaskOrganizerWebService/target/TaskOrganizerWebService-1.0-SNAPSHOT.war tomcat-webserver-2:/usr/local/tomcat/webapps

	docker start tomcat-webserver-2

	# Process needed to create an erlang node, theoretically this should start automatically at startup...
	docker exec -dti tomcat-webserver-2 epmd

build-webserver-2:
	$(MAKE) clean-tomcat-webserver-2
	docker build -t distributed-systems/tomcat-webserver-2 ./tomcat-webserver-2
	docker run -dit --name tomcat-webserver-2 --net DistributedSystems --ip 172.18.0.152 -p 8080 -v $(shell pwd)/tomcat-webserver-2:/app  distributed-systems/tomcat-webserver-2
	docker exec -i -w /usr/local/tomcat/ tomcat-webserver-2 /bin/sh -c 'cp -r webapps.dist/* webapps/'
	docker cp ConfigTomcatDocker/context.xml tomcat-webserver-2:/usr/local/tomcat/webapps/manager/META-INF/
	docker cp ConfigTomcatDocker/tomcat-users.xml tomcat-webserver-2:/usr/local/tomcat/conf
	docker stop tomcat-webserver-2 || true

clean-tomcat-webserver-2:
	docker stop tomcat-webserver-2 || true
	docker rm tomcat-webserver-2 || true
	docker rmi distributed-systems/tomcat-webserver-2 || true


# HAProxy
build-haproxy:
	$(MAKE) clean-haproxy
	docker build -t distributed-systems/haproxy ./haproxy
	docker run -dit --name haproxy --net DistributedSystems --ip 172.18.0.150 --sysctl net.ipv4.ip_unprivileged_port_start=0 distributed-systems/haproxy

clean-haproxy:
	docker stop haproxy || true
	docker rm haproxy || true
	docker rmi distributed-systems/haproxy || true