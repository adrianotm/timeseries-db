build_and_run_server:
	stack build
	stack exec server

build_server:
	stack build

run_server:
	stack exec server

run_client:
	cd client 
	elm reactor
