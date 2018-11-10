SRCS:=	utils.ml \
	app.ml \
#	oauth2.ml\

#PKGS:=unix,threads,pcre,netstring,ssl,netclient,netcgi2,equeue-ssl,batteries,calendar,json-wheel,cryptokit
#PKGS:=unix,threads,pcre,netstring,netcgi2,calendar,json-wheel,uutf,cryptokit
PKGS:=unix,threads,pcre,netstring,netcgi2,calendar,json-wheel,uutf
CAMLC:=ocamlfind ocamlc -thread
OPT:=ocamlfind ocamlopt -g -thread

VERSION:=0.9.6
NAME:=webapp
LIBNAME:=webApp
PACKNAME:=WebApp

CMOS:=$(SRCS:.ml=.cmo)
CMXS:=$(SRCS:.ml=.cmx)
CMIS:=$(MLIS:.mli=.cmi)

all:$(LIBNAME).cmx $(LIBNAME).cmxa META

$(LIBNAME).cmx:$(CMXS)
	$(OPT) -pack -o $@ $(CMXS)


$(LIBNAME).cmxa:$(LIBNAME).cmx
	$(OPT) -a -o $@ $<

%.cmi:%.mli
	$(OPT) -package $(PKGS) -for-pack $(PACKNAME) -c $<

%.cmx:%.ml
	$(OPT) -package $(PKGS) -for-pack $(PACKNAME) -c $<

META: Makefile
	echo "description = \"$(NAME) library\"" > $@
	echo "version = \"$(VERSION)\"" >> $@
	echo "archive(byte) = \"$(LIBNAME).cma\"" >> $@
	echo "archive(native) = \"$(LIBNAME).cmxa\"" >> $@
	echo "requires = \"$(PKG)\"" >> $@

install:
	ocamlfind install $(NAME) META $(LIBNAME).*

uninstall:
	ocamlfind remove $(NAME)

reinstall:
	make uninstall
	make install

clean:
	rm -f *.cm* *.a *.o

rebuild:
	make clean
	make all
