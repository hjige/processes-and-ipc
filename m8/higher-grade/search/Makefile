ERLC_FLAGS=-Wall

SOURCES=$(wildcard src/*.erl)

HEADERS=$(wildcard src/*.hrl)

OBJECTS:=$(SOURCES:src/%.erl=ebin/%.beam)

APPNAME=Concurrent search

.PHONY: doc doc_url help skeleton

all: $(OBJECTS)

ebin/%.beam: src/%.erl
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	rm -f erl_crash.dump
	rm -Rf ebin/*
	rm -Rf src/*.beam
	rm -Rf doc/*.html
	rm -f doc/edoc-info

repl: all
	cd src && ERL_COMPILER_OPTIONS='[{outdir,"../ebin"}]' erl -pa ../ebin

doc:
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"."' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

doc_url:
	@echo
	@echo "EDoc index page available at file://$(PWD)/doc/index.html"
	@echo

view_doc: doc
	./view_doc

test: $(OBJECTS)
	erl -noshell -pa ebin -eval "master:start(10, 1, 100)" -s init stop
