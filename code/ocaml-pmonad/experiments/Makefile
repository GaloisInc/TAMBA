OPT := 1
DEBUG := 1

OCAMLFIND := ocamlfind

ifdef OPT
OCAMLC := $(OCAMLFIND) ocamlopt 
OCAMLDEP := $(OCAMLFIND) ocamldep -native
OEXT := cmx
AEXT := cmxa
else
OCAMLC := $(OCAMLFIND) ocamlc
OCAMLDEP := $(OCAMLFIND) ocamldep
OEXT := cmo
AEXT := cma
endif

LIBS := extlib unix

BUILD_FLAGS := $(LIBS:%=-package %)

ifdef DEBUG
BUILD_FLAGS += -g
endif

ifndef OPT
BUILD_FLAGS += -dllpath-all -dllpath delimcc
endif

ALINK_FLAGS := $(BUILD_FLAGS)
BUILD_FLAGS += -linkpkg
LINK_FLAGS := $(BUILD_FLAGS)

current: exp_ships

all: exp_ships

exp_%: pmap.$(OEXT) util.$(OEXT) pmonad.$(OEXT) common.$(OEXT) exp_%.$(OEXT)
	$(OCAMLC) $(LINK_FLAGS) $^ -o exp_$*

%.cmo : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmx %.cmxi : %.ml
	$(OCAMLC) $(BUILD_FLAGS) -c $<

%.cmi : %.mli
	$(OCAMLC) $(BUILD_FLAGS) -c $<

clean:
	rm -Rf *.cmx *.cmi *.cmo *.o exp_ships .dep

.dep: $(find *.ml *.mli)
	$(OCAMLDEP) *.ml *.mli > .dep

-include .dep
