.DEFAULT_GOAL=build

.PHONY: clean
clean:
	@rm -rf public

.PHONY: build
build: clean
	@hugo -D
