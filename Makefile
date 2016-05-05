OCAMLBUILD=ocamlbuild
OCAMLBUILD_FLAGS=

# **COMMA-SEPARATED** list of packages to be passed to ocamlbuild
PACKAGES=zarith

TARGET=main.native
TARGET_DEBUG=main.d.byte

all: $(TARGET)

debug: $(TARGET_DEBUG)

%.native: %.ml
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

%.byte: %.ml
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

%.p.native: %.ml
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

%.d.byte: %.ml
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -pkgs $(PACKAGES) $@

clean:
	$(OCAMLBUILD) -clean

dot: cfg.dot
	dot -Tpng $< | feh -
