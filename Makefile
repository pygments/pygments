#
# Makefile for Pygments
# ~~~~~~~~~~~~~~~~~~~~~
#
# Combines scripts for common tasks.
#
# :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
# :license: BSD, see LICENSE for details.
#

PYTHON ?= python3

export PYTHONPATH = $(shell echo "$$PYTHONPATH"):$(shell python -c 'import os; print ":".join(os.path.abspath(line.strip()) for line in file("PYTHONPATH"))' 2>/dev/null)

.PHONY: all check clean clean-pyc docs mapfiles \
	pylint reindent test test-coverage \
	tox-test tox-test-coverage regexlint

all: clean-pyc check test

check:
	@$(PYTHON) scripts/check_crlf.py pygments build external
	@$(PYTHON) scripts/detect_missing_analyse_text.py --skip-no-aliases
	@pyflakes pygments | grep -v 'but unused' || true
	@$(PYTHON) scripts/check_sources.py -i build -i dist -i pygments/lexers/_mapping.py \
		   -i docs/build -i pygments/formatters/_mapping.py -i pygments/unistring.py \
		   -i tests/support/empty.py
	@$(PYTHON) scripts/count_token_references.py --minfiles=1 --maxfiles=1 \
		   --minlines=1 --maxlines=3 --subtoken

clean: clean-pyc
	-rm -rf doc/_build build Pygments.egg-info
	-rm -f codetags.html

clean-pyc:
	find . -name '__pycache__' -exec rm -rf {} +

docs:
	make -C doc html

mapfiles:
	(cd pygments/formatters; $(PYTHON) _mapping.py)
	(cd pygments/lexers; $(PYTHON) _mapping.py)

pylint:
	@pylint --rcfile scripts/pylintrc pygments

reindent:
	@$(PYTHON) scripts/reindent.py -r -B .

TEST = tests

test:
	@$(PYTHON) `which pytest` $(TEST)

test-coverage:
	@$(PYTHON) `which pytest` --cov --cov-report=html --cov-report=term $(TEST)

tox-test:
	@tox -- $(TEST)

tox-test-coverage:
	@tox -- --with-coverage --cover-package=pygments --cover-erase $(TEST)

RLMODULES = pygments.lexers

regexlint:
	@if [ -z "$(REGEXLINT)" ]; then echo "Please set REGEXLINT=checkout path"; exit 1; fi
	PYTHONPATH=`pwd`:$(REGEXLINT) $(REGEXLINT)/regexlint/cmdline.py $(RLMODULES)
