
REBAR=./rebar

.PHONY: all clean run test

all:
	@$(REBAR) prepare-deps

clean:
	@$(REBAR) clean

run:
	@erl -pa ebin -s ec main -s init stop -noshell

test:
	@rm -rf .eunit
	@mkdir .eunit
	@$(REBAR) eunit
