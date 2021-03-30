build:
	docker network create --subnet=172.18.0.0/16 DistributedSystems || true
	$(MAKE) -C TaskManagerDocker build-node1
	$(MAKE) -C TaskManagerDocker build-node2

	$(MAKE) -C TaskManagerDocker build-webserver-1
	$(MAKE) -C TaskManagerDocker build-webserver-2

deploy:
	$(MAKE) -C TaskManagerDocker start-rabbit
	$(MAKE) -C TaskManagerErlangServer deploy-multiple-nodes

	$(MAKE) -C TaskManagerDocker start-multiple-webserver
	$(MAKE) -C TaskManagerDocker build-haproxy

	$(MAKE) start-multiple
	