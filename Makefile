

.PHONY: deps

all: deps compile

compile:
	./rebar compile
	make -C java_src

deps:
	./rebar get-deps

clean:
	./rebar clean
	make -C java_src clean

distclean: clean 
	./rebar delete-deps

eunit:
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c apps/riak_jmx/ebin


