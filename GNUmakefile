PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
TGZ     := $(PKGNAME)_$(PKGVERS).tar.gz
R_HOME  ?= $(shell R RHOME)
DATE    := $(shell date +%Y-%m-%d)

.PHONEY: usage check clean

usage:
	@echo "Usage: make TARGET with TARGET being:"
	@echo ""
	@echo "  clean     - Clean up."
	@echo "  roxygen   - Roxygenize."
	@echo "  sd        - Build static documentation."
	@echo "  build     - Build source package."
	@echo "  check     - Run CRAN check on the package."
	@echo "  install   - Install the package."

pkgfiles = DESCRIPTION \
	   README.html \
		 .Rbuildignore \
	   inst/testdata/* \
	   inst/staticdocs/index.r \
		 tests/testthat.R \
		 tests/testthat/* \
		 data/* \
	   R/*

clean:
	@echo "Cleaning up..."
	rm -fR pfm.Rcheck
	@echo "DONE."

roxygen: 
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); document()'
	@echo "DONE."

sd: roxygen
	@echo "Building static documentation..."
	# suppressWarnings to get rid of mbcsToSbcs warnings when plotting the 'µ' character
	"$(R_HOME)/bin/Rscript" -e 'suppressWarnings(staticdocs::build_site())'
	@echo "DONE."

$(TGZ): $(pkgfiles)
	sed -i -e "s/Date:.*/Date: $(DATE)/" DESCRIPTION
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); document()'
	@echo "Building package..."
	git log --no-merges -M --date=iso > ChangeLog
	"$(R_HOME)/bin/R" CMD build . > build.log 2>&1
	@echo "DONE."

README.html: README.rmd
	"$(R_HOME)/bin/Rscript" -e "rmarkdown::render('README.rmd', output_format = 'html_document', clean = FALSE)"
	mv README.utf8.md README.md
	rm README.knit.md

build: $(TGZ)

test: build
	@echo "Running testthat tests..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); devtools::test()' 2>&1 | tee test.log
	@echo "DONE."

quickcheck: build
	@echo "Running check..."
	"$(R_HOME)/bin/R" CMD check $(TGZ)
	@echo "DONE."

check: build
	@echo "Running CRAN check..."
	"$(R_HOME)/bin/R" CMD check --as-cran $(TGZ)
	@echo "DONE."

install: build
	@echo "Installing package..."
	"$(R_HOME)/bin/R" CMD INSTALL --no-multiarch $(TGZ)
	@echo "DONE."

winbuilder: build
	date
	@echo "Uploading to R-release on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-release/
	@echo "Uploading to R-devel on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-devel/
