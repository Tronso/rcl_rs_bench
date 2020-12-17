BASEDIR = $(shell pwd)
REBAR = rebar3
RDIR = _build/default/lib/rcl_bench/R
TESTDIR = _build/default/rel/rcl_rs_bench/tests
OUTDIR = $(BASEDIR)/results

all: compile

compile:
	$(REBAR) compile

format:
	$(REBAR) format

clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer

test: 
	$(REBAR) eunit

rel:
	$(REBAR) release

run: rel
	_build/default/rel/rcl_rs_bench/bin/rcl_rs_bench console

visualize:
	mkdir -p $(BASEDIR)/results
	cat $(TESTDIR)/get_single.csv $(TESTDIR)/get-own-puts_single.csv $(TESTDIR)/http-get_single.csv $(TESTDIR)/http-get-own-puts_single.csv $(TESTDIR)/http-put_single.csv $(TESTDIR)/put_single.csv | sed '2, $$s/timestamp, unit, microseconds//g' > $(OUTDIR)/all_single.csv
	Rscript $(RDIR)/latency.R all "$(OUTDIR)/all_single.csv" $(OUTDIR)/latency.png
	Rscript $(RDIR)/throughput.R all "$(OUTDIR)/all_single.csv" $(OUTDIR)/throughput.png