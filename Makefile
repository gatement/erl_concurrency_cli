# variables
REBAR = bin/rebar

# targets
compile:
	@$(REBAR) get-deps compile

run: compile
	@erl -name ser01@local.com -pa deps/*/ebin ./ebin -config sys -s erl_concurrency_ser

eunit: compile
	@$(REBAR) eunit

xref:
	@$(REBAR) xref 

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

check-deps:
	@$(REBAR) check-deps

update-deps:
	@$(REBAR) update-deps

shell:
	@$(REBAR) shell

help: 
	@echo "make compile (default)"
	@echo "make run"
	@echo "make eunit"
	@echo "make xref"
	@echo "make doc"
	@echo "make clean"
	@echo "make check-deps"
	@echo "make update-deps"
	@echo "make shell"
	@echo "make help (this)"
