.PHONY: clean
clean:
	rm -rf build/


build: src/CNAME src/**/*.html
	mkdir -p ./$@
	cp src/CNAME $@
	cp -r src/static $@/static


build/%.html: src/pages/%.html | build
	bin/render $< > $@


.PHONY: all
all: build
