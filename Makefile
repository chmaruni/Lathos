.SUFFIXES: .erl .beam .yrl

PARSERS=

ebin/%.beam: src.erl/%.erl
	erlc -o ebin -W $<
	
src.erl/%.erl: src.erl/%.yrl
	erlc -o $@ -W $<
	chmod u-w $@
	
ERLS =	lathos lathos_tests lathos_serve \
		lathos_parse lathos_parse_tests \
		pico_http_server pico_socket_server \
		pico_utils 
BEAMS = ${ERLS:%=ebin/%.beam} 

all: test

clean:
	rm -rf ./ebin
	rm -rf ./doc

compile: parsers
	mkdir -p ebin
	erl -make

parsers: $(PARSERS:%=%.erl)

test: compile
	erl -noshell \
		-pa ebin \
		-s test_suite test \
		-s init stop


run_server: compile
	erl -pa ebin -s lathos_serve start
	
post_test_tuples:
	curl --data-binary '@test_tuple1.post' http://localhost:4999/create_nodes
	@echo
	curl --data-binary '@test_tuple2.post' http://localhost:4999/create_nodes
	@echo