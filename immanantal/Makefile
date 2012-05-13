RELEASE=coimm enumerate_spectra check

all: $(RELEASE)

%.native %.byte %.p.native:
	ocamlbuild $@

$(RELEASE):
	if [ ! -e bin ]; then mkdir bin; fi
	make $@.native
	cp `readlink $@.native` bin/$@
	rm $@.native

run: $(RELEASE)
	ls bin/* | parallel -j 10 srun -p emhigh './{}'

clean:
	rm -rf bin
	ocamlbuild -clean
