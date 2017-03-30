
ERL=erl
MKDIR=mkdir
REBAR=./rebar
RM=rm

.PHONY: all clean run test

all:
	@$(REBAR) prepare-deps

clean:
	@$(REBAR) clean

run:
	@$(ERL) -pa ebin -s power main -s init stop -noshell

test:
	@$(RM) -rf .eunit
	@$(MKDIR) .eunit
	@$(REBAR) eunit


