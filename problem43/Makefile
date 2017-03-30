
REBAR=./rebar

.PHONY: all edoc test run clean build_plt dialyzer app

all:
	@$(REBAR) prepare-deps

edoc: all
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

run: all
	erl -pa ebin -s substring main -s init stop -noshell

clean:
	@$(REBAR) clean
