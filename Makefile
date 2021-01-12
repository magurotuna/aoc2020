.DEFAULT_GOAL := run

.PHONY: run
run:
	@dune build --display=quiet && _build/default/aoc2020.exe
