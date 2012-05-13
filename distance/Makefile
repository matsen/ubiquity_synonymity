RELEASE=check codist

all: $(RELEASE)

%.native %.byte %.p.native:
	ocamlbuild $@

$(RELEASE):
	if [ ! -e bin ]; then mkdir bin; fi
	make $@.native
	cp `readlink $@.native` bin/$@
	rm $@.native

run: $(RELEASE)
	mkdir -p n_spectra_results
	mkdir -p check_results
	ls bin/* | parallel -j 10 srun -p emhigh './{}'

clean:
	rm -rf bin
	ocamlbuild -clean
