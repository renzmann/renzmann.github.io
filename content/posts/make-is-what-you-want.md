---
title: "`make` might not be what you want, but it's probably all you need"
date: 2022-03-06T19:45:32-05:00
draft: true
---

In data science we tend to think in "DAGs" (directed acyclic graphs), which just
means "to make this report, we first have to build this other thing, and to build
_that_ thing, we have to run these two queries, and so on.  We decompose a system of
processing data and producing hard artifacts like visualizations or data exports
for others to consume.  

# TODO: image of example DAG here

There are a lot of contenders in this space, and each one solves it a little
differently.  Right now the hot thing is
[dbt](https://docs.getdbt.com/tutorial/setting-up), but before that we had
airflow, dagster, prefect, and argo, just to name a few, that all were build to
operate DAGs at scale on different platforms. For large, mission-critical data
pipelines these can provide a lot of value, but the truth is that as data
scientists, most of us don't need something this heavy.  Most projects I see
really just need some way of defining the links between "scrapbook output".
Maybe it's a jupyter notebook, or a python script, or some queries that have to
happen in a particular order based on an updated warehouse feed.  dbt calls
itself the "data build tool," and that's basically what we need: a way to
compile project assets from the source code. Moreover, there are a host of
other reasons you might not want to try building a whole new tooling ecosystem
into your workflow.  Maybe:

* you can't get permission for a new install
* you don't want to force another install on your end users or coworkers
* you don't want more transitive dependencies entering the picture
* you don't like someone trying to sell their cloud solution on top of the free
  tier offering

`make` was born from a history of compiling C programs on Unix machines, but it's completely
agnostic to language choice. It's job is to translate _targets_ and
_prerequisites_ into a DAG, and incrementally build only the parts it needs to
when any of the source files change.  Given that, why would I choose make over
one of the more modern alternatives?

* The commands are very elegant - `make report.xlsx` is completely intuitive
* Parallel execution is built in and easy to turn on or off
* It's installed on damn near everything[^0], and has proven over the last
  almost 50 years to be a shark, not a dinosaur
# TODO: any other points here? - Anything about code/text files?

# TODO: image of example project

I'm going to use an example of a recent project I built using just a Makefile,
some python, and a little SQL that shows simple tools can be just as efficient
and reliable as dbt, but without the overhead of learning, installing,
configuring, and inevitably debugging an unfamiliar tool.[^1]  Ultimately, I wanted
to hand this project off in such a way that any of my teammates could maintain
it if I was unavailable, so it has to be short, and stick to the tools I know
they have installed everywhere.

# TODO: emphasize importance of considering others

Our goal is to produce an Excel file for executive consumption that has a
meaningful summary of some data pulled out of our analytics warehouse.  Overall,
it'll look a little like this:

```
base queries --> summary csv's --> (report.xlsx, diagrams for powerpoint)
```

The "base" queries might look a lot like common table expressions (CTEs, or
those blocks you see in `WITH` statements), but we've broken them into several,
separate queries so that we can debug them separately if any underlying data has
changed that affects one, but not the others.  We're plopping those summarized
results into some flat files so that we can examine the results with our
favorite tools like `pandas` or `awk`.  We'll then take all those flat results
and produce deliverables from them, like charts and an excel file.

`myguy` is always there for me, so that's the name of our project, and its
basic structure looks like this:

```
.
├── config.yml
├── Makefile
├── myguy.py
├── README.md
└── sql
    ├── this_quarter_sales.sql
    ├── model_forecast.sql
    └── customer_disposition.sql
```

Each file in the `sql/` directory is a query we execute and then locally cache
the results.  Later we'll discuss how to handle the case where all our queries
are handled remotely, and don't create local files, such as running `CREATE
TABLE AS` (CTAS) queries prior to building the report, but for now we'll keep it
simple: query --> file on computer.

Our goal is to make reproducing this report dead simple.  I should only have to
run this command to rebuild the report at any time:

```sh
make report.xlsx
```

The cookbook that provides this _rule_ is the `Makefile`. 

```make
report.xlsx: myguy.py
	python -m myguy build-report
```

If this is your first time seeing make, there's a few terms to know:

* `report.xlsx` - this is the _target_ of the rule. It's the file that's
  produced by running `make report.xlsx`
* `myguy.py` - the _prerequisite_ of `report.xlsx`, it has to exist in order to
  create the excel file. If this python file's contents have changed recently,
  then that's an indication that `report.xlsx` will likely also change.
* `python -m myguy build-report` - this is the _recipe_ that make runs when you
  issue the command `make report.xlsx`. I am invoking python with the `-m`, or
  "run module as main" flag in case we ever refactor our single .py file into a
  module, like `myguy/__init__.py` with its complementary "dunder main"
  `myguy/__main__.py`.

In the python file, we need a few entry points, since it will handle our actual
runtime when we want to execute the queries or do some pandas hackery.

```python3
# myguy.py

from pathlib import Path
from time import sleep

import click


@click.group()
def cli():
    pass


@cli.command()
def build_report():
    print("Building")
    sleep(2)
    Path("build/report.xlsx").touch()
    print("Done!")


if __name__ == "__main__":
    cli()
```

I think `click` is just great, and provides me with a lot of zen writing a CLI
compared to `argparse`, so that's why I'm using it here. You can achieve
everything I'm doing in this article equally well with argparse too though. In
this script we create a cli group because we'll eventually add more commands to
it.  The `build_report` function just replicates a process that takes a couple
seconds before it outputs a file to `build/report.xlsx`. If we try building our
report now with `make report.xlsx`, we get a `FileNotFoundError: [Errno 2] No
such file or directory: 'build/report.xlsx'`, and that's because we need to make
sure the `build` directory exists before running this command. We could handle
that in the python with a few lines, but why not have our dependency management
tool, `make`, do it for us?

```make
# Makefile

build:
	mkdir -p build

report.xlsx: myguy.py | build
	python -m myguy build-report
```

Now our `make report.xlsx` works just fine, and we get a new directory `build`
with our empty report in it. Normally we won't need the `|`, but in this case it
declares that the `build` rule should only be run once, even if we have other
targets with `build` as a prerequisite.[^2]  If we rerun the command to make the
report, it doesn't try to create that directory again, because it can see that
it already exists.



# TODO: what if the processes are all remote? empty targets section

* building a simple dag
  - table dependencies for SQL models

* separating sources + targets
  - VPATH
  - pattern rules
  - order-only prerequisite for target dir

* making it easy on our users with a `dag.mk` -- could be a visual tool

* setting parallel by default - gif of it running



[^0]: Except Windows. You'll need to get it via mingw/cygwin or via the Windows
  subsystem for Linux.

[^1]: I fully acknowledge the irony here that `make` is, in fact, a very foreign
  tool to many data scientists.

[^2]: These are called [order-only
  prerequisites](https://www.gnu.org/software/make/manual/make.html#Prerequisite-Types)
