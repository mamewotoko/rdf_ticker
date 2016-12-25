RESULT=rdf
OCAMLMAKEFILE=./OCamlMakefile
SOURCES=mytcp.ml url.ml http.ml utf.ml rdf.ml slash.ml
#utf_main.ml oldrdf.ml oldutf.ml utf_fast.ml  filterslash.ml ticker
LIBS=str unix labltk threads expat
#XXX cannot find by +expat, +labltk(when installed using opam) ... why?
TOPDIR=$(OCAML_TOPLEVEL_PATH)/../
INCDIRS=$(TOPDIR)/expat +labltk +threads

include $(OCAMLMAKEFILE)
