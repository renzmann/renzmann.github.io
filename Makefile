.PHONY: clean
clean:
	rm -rf build/

build: src/CNAME src/**/*.html
	mkdir -p ./build
	cp src/CNAME $@


build/%.html: src/pages/%.html | build
	bin/render $< > $@


.PHONY: all
all: build
