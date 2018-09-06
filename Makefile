.PHONY: all
all:
	dune build

.PHONY: test
test:
	dune runtest --force

.PHONY: clean
clean:
	dune clean
