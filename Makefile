REBAR := rebar3

all: update doc

update:
	$(REBAR) upgrade

doc:
	$(REBAR) edoc

test:
	$(REBAR) do eunit,dialyzer,xref

clean:
	$(REBAR) clean

.PHONY: all update doc test clean
