
ERL=erl
REBAR=rebar

.PHONY: all clean run test

all:
	@$(REBAR) prepare-deps

clean:
	@$(REBAR) clean

run:
	@$(ERL) -pa ebin -s permutations main -s init stop -noshell

test:
	@rm -rf .eunit
	@mkdir .eunit
	@$(REBAR) eunit
