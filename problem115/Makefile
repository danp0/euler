
REBAR=rebar

.PHONY: all clean run run0 test

all:
	@$(REBAR) prepare-deps

clean:
	@$(REBAR) clean

run:
	@erl -pa ebin -s blocks main -s init stop -noshell

run0:
	@erl -pa ebin -s blocks0 main -s init stop -noshell

test:
	@$(REBAR) eunit
