
REBAR=rebar

.PHONY: all edoc test run clean build_plt dialyzer app

all:
	@$(REBAR) compile

run:
	@erl -pa ebin -s pentagon main -s init stop -noshell

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
