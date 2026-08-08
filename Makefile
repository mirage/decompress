.PHONY: bench


bench:
	dune build --profile benchmark bench/output.csv && \
	cat /dev/urandom | ./_build/default/bench/zpipe | ./_build/default/bench/bench.exe -j
