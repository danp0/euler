
ERL=erl
REBAR=rebar

.PHONY: all run test clean

all:
	@$(REBAR) prepare-deps

run: all
	@$(ERL) -pa ebin -s goldbach main -s init stop -noshell

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean


