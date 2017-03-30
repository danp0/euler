
REBAR=./rebar

.PHONY: all edoc test run clean build_plt dialyzer app

all:
	@$(REBAR) prepare-deps

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

run: all
	@erl -pa ebin -s intersection main -s init stop -noshell

clean:
	@$(REBAR) clean
