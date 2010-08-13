EC = /usr/local/bin/erlc
ERL = /usr/local/bin/erl
ECFLAGS = -Dtest +debug_info

%.beam: %.erl test_%.hrl
	$(EC) $(ECFLAGS) -W $<
#%.erl: test_%.hrl
#	$(EC) $(ECFLAGS) -W $<


MODS = ap appreg emkuu ltree message messageparser msgflow queuemanager receiver sender store subscriptions uri meta counter

.PHONY: all
all: test

appregdtl: 
	$(ERL) -noinput -eval 'emkuu:compiletmpl(), erlang:halt()'

compile: ${MODS:%=%.beam} appregdtl

clean:
	rm -f *.beam

test: clean compile
	$(EC) -W test_all.erl
	$(ERL) -noinput  -eval 'test_all:test(), erlang:halt()'
