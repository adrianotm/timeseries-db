.PHONY: setup_server
setup_server:
	stack setup

.PHONY: build_and_run_server
build_and_run_server:
	stack build
	stack exec server

.PHONY: build_server
build_server:
	stack build

.PHONY: run_server
run_server:
	stack exec server

.PHONY: run_client
run_client:
	cd client && elm reactor

.PHONY: create_static_html
create_static_html:
	cd client && elm make src/Main.elm --output index.html
