
REBAR=./rebar

.PHONY: all clean run test

all:
	@$(REBAR) prepare-deps

clean:
	@$(REBAR) clean

run:
	@erl -pa ebin -s fraction main -s init stop -noshell

test:
	@$(REBAR) eunit
