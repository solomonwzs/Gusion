# vim: noet:

REBAR=./rebar

all:
	@$(REBAR) get-deps
	@$(REBAR) compile
