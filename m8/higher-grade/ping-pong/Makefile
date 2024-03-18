ERLC_FLAGS=-Wall

SOURCES=$(wildcard src/*.erl)

HEADERS=$(wildcard src/*.hrl)

OBJECTS:=$(SOURCES:src/%.erl=ebin/%.beam)

APPNAME=Module 8 - OSPP (1DT096) 2020

.PHONY: clean doc_url repl view_doc

all: $(OBJECTS) 

ebin/%.beam: src/%.erl
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	rm -f erl_crash.dump
	rm -Rf ebin/*
	rm -Rf src/*.beam
	rm -Rf doc/*.html
	rm -f doc/edoc-info

doc: doc/index.html

doc/index.html: $(SOURCES)
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"."' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

doc_url: doc
	@echo
	@echo "EDoc index page available at file://$(PWD)/doc/index.html"
	@echo

repl:
	erl -pa ebin -s 

view_doc: doc/index.html
	./view_doc
