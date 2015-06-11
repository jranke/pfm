PKGSRC  := $(shell basename $(CURDIR))
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" pkg/DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" pkg/DESCRIPTION)
TGZ     := $(PKGSRC)_$(PKGVERS).tar.gz
R_HOME  ?= $(shell R RHOME)
DATE    := $(shell date +%Y-%m-%d)

.PHONEY: usage check clean

usage:
	@echo "Usage: make TARGET with TARGET being:"
	@echo ""
	@echo "  clean     - Clean up."
	@echo "  roxygen   - Roxygenize."
	@echo "  build     - Build source package."
	@echo "  check     - Run CRAN check on the package."
	@echo "  install   - Install the package."

pkgfiles = pkg/DESCRIPTION \
	   pkg/inst/testdata/* \
		 pkg/tests/testthat.R \
		 pkg/tests/testthat/* \
	   pkg/R/*

clean:
	@echo "Cleaning up..."
	rm -fR pkg.Rcheck
	@echo "DONE."

roxygen: 
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); document("pkg")' 2>&1 | tee roxygen.log
	@echo "DONE."

$(TGZ): $(pkgfiles)
	sed -i -e "s/Date:.*/Date: $(DATE)/" pkg/DESCRIPTION
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); document("pkg")' 2>&1 | tee roxygen.log
	@echo "Building package..."
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	"$(R_HOME)/bin/R" CMD build pkg > build.log 2>&1
	@echo "DONE."

build: $(TGZ)

test: build
	@echo "Running testthat tests..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); devtools::test("pkg")' 2>&1 | tee test.log
	@echo "DONE."

check: build
	@echo "Running check..."
	"$(R_HOME)/bin/R" CMD check $(TGZ)
	@echo "DONE."

crancheck: build
	@echo "Running CRAN check..."
	"$(R_HOME)/bin/R" CMD check --as-cran $(TGZ)
	@echo "DONE."

install: build
	@echo "Installing package..."
	"$(R_HOME)/bin/R" CMD INSTALL --no-multiarch $(TGZ)
	@echo "DONE."
