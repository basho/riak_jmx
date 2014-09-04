

.PHONY: deps test

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

DIALYZER_APPS = kernel stdlib erts sasl eunit

include tools.mk
