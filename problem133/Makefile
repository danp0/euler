
REBAR=./rebar

.PHONY: all clean run test

all:
	@$(REBAR) prepare-deps

clean:
	@$(REBAR) clean

run:
	@erl -pa ebin -s repunit main -s init stop -noshell

test:
	@$(REBAR) eunit


