OCAMLBUILD=ocamlbuild
OCAMLBUILD_FLAGS=

# **COMMA-SEPARATED** list of packages to be passed to ocamlbuild
PACKAGES=zarith

TARGET=main.native
TARGET_DEBUG=main.d.byte

.PHONY: buildanyway_

all: $(TARGET)

debug: $(TARGET_DEBUG)

buildanyway_: ;

%.native: %.ml buildanyway_
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

%.byte: %.ml buildanyway_
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

%.p.native: %.ml buildanyway_
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

%.d.byte: %.ml buildanyway_
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

clean:
	$(OCAMLBUILD) -clean

dot: cfg.dot
	dot -Tpng $< | feh -
