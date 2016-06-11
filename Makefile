OCAMLBUILD=ocamlbuild
OCAMLBUILD_FLAGS=

# **COMMA-SEPARATED** list of packages to be passed to ocamlbuild
PACKAGES=zarith

TARGET=main.native
TAR=abstract_interpretation-bastian_theophile.tgz
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
	
test: examples $(TARGET)
	for test in examples/*.c; do \
		bname="$$(dirname $$test)/$$(basename $$test '.c')"; \
		./$(TARGET) -interval $$test > $$bname.interv.out; \
		./$(TARGET) -constant $$test > $$bname.const.out; \
	done
	
tar: test clean
	tar czf $(TAR) --transform "flags=r;s|^|$$(basename $(TAR) .tgz)/|" \
	   	**/*.{ml,mli,mll,mly} main.ml examples/*.{c,out} report/report.pdf \
		Makefile _tags

clean:
	$(OCAMLBUILD) -clean
	rm -f $(TAR)

fullclean: clean
	rm -f examples/*.out

dot: cfg.dot
	dot -Tpng $< | feh -
