include Makefile.config
SOURCES   = config.ml types.ml fetch.ml cache.ml okey.ml gps.ml kml.ml geonames.ml resources.ml view.ml
OBJECTS   = $(SOURCES:.ml=.cmo)
XOBJECTS  = $(OBJECTS:.cmo=.cmx)
ARCHIVE   = archive.cma
XARCHIVE  = $(ARCHIVE:.cma=.cmxa)
REQUIRES  = getopt num netstring equeue-gtk2 netclient lablgtk2 pxp pcre

all: $(ARCHIVE)
	$(OCAMLC) -g -o gmcache -custom $(ARCHIVE) gmcache.ml -linkpkg

opt: $(XARCHIVE)
	$(OCAMLOPT) -o gmcache $(XARCHIVE) gmcache.ml -linkpkg


$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) $(OBJECTS)

$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) $(XOBJECTS)

#OPTIONS   = -syntax camlp4o -package camlp4
OCAMLC    = $(OCAMLFIND) ocamlcp  -w ys $(OPTIONS) -package "$(REQUIRES)"
OCAMLOPT  = $(OCAMLFIND) ocamlopt -w ys $(OPTIONS) -package "$(REQUIRES)"
OCAMLDEP  = $(OCAMLFIND) ocamldep $(OPTIONS)
OCAMLLEX  = ocamllex
OCAMLFIND = ocamlfind

depend: $(SOURCES)
	$(OCAMLDEP) $(SOURCES) $(APPNAME).ml >depend

.PHONY: install
install:
	install -m 755 -d $(PREFIX) $(PREFIX)/bin
	install -m 755 gmcache $(PREFIX)/bin

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/bin/gmcache

.PHONY: clean
clean:
	rm -f *~ *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

.PHONY: distclean
distclean: clean
	rm -f *~ depend Makefile.config
	rm -f gmcache

.SUFFIXES: .cmo .cmi .cmx .ml .mli .mll

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.mll.ml:
	$(OCAMLLEX) $<

*.mli:
	true

include depend
