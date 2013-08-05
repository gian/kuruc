BIN=../../bin
COMPILER=mlton 
KUPEG=$(BIN)/kupeg

SMLDEPS= \
	config.sml debug.sml main.sml version.sml \
	ast/typed_ast.sml

all: $(BIN)/kuruc

tests: $(BIN)/kuruc
	$(MAKE) -C tests

$(BIN)/kuruc: $(SMLDEPS) kuruc.mlb
	./version.sh > version.k
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

frontend/kuru.kpg.k: frontend/kuru.kpg
	$(MAKE) -C frontend kuru.kpg.k

%.sml: %.k
	ln -sf $(notdir $<) $@

version.k: version.sh
	./version.sh > version.k

frontend: 
	$(MAKE) -C frontend

lib: 
	$(MAKE) -C lib

semant: 
	$(MAKE) -C semant

ast: 
	$(MAKE) -C ast

intermediate: 
	$(MAKE) -C intermediate

codegen:
	$(MAKE) -C codegen

clean:
	rm -f $(BIN)/kuruc config.sml main.sml compile.sml debug.sml version.sml 
	$(MAKE) -C frontend clean
	$(MAKE) -C lib clean
	$(MAKE) -C ast clean
	$(MAKE) -C semant clean
	$(MAKE) -C intermediate clean
	$(MAKE) -C codegen clean

.PHONY: tests clean intermediate ast semant codegen lib frontend 
.INTERMEDIATE: $(SMLDEPS)
