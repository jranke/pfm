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
	@echo "  sd        - Build static documentation."
	@echo "  build     - Build source package."
	@echo "  check     - Run CRAN check on the package."
	@echo "  install   - Install the package."

pkgfiles = pkg/DESCRIPTION \
	   README.html \
	   pkg/inst/testdata/* \
	   pkg/inst/staticdocs/index.r \
		 pkg/tests/testthat.R \
		 pkg/tests/testthat/* \
		 pkg/data/* \
	   pkg/R/*

clean:
	@echo "Cleaning up..."
	rm -fR pkg.Rcheck
	@echo "DONE."

roxygen: 
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); document("pkg")'
	@echo "DONE."

sd: roxygen
	@echo "Building static documentation..."
	# suppressWarnings to get rid of mbcsToSbcs warnings when plotting the 'µ' character
	cd pkg; "$(R_HOME)/bin/Rscript" -e 'suppressWarnings(staticdocs::build_site())'
	# The following workaround (setting the pdf encoding) as of stackoverflow 13251665 did not do it
	#cd pkg; "$(R_HOME)/bin/Rscript" -e 'pdf.options(encoding="ISOLatin1.enc"); staticdocs::build_site()'
	# The following also does not work (Konvertierungsfehler)
	#cd pkg; "$(R_HOME)/bin/Rscript" -e 'options(encoding="latin1"); staticdocs::build_site()'
	@echo "DONE."

$(TGZ): $(pkgfiles)
	sed -i -e "s/Date:.*/Date: $(DATE)/" pkg/DESCRIPTION
	@echo "Roxygenizing package..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); document("pkg")'
	@echo "Building package..."
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	"$(R_HOME)/bin/R" CMD build pkg > build.log 2>&1
	@echo "DONE."

README.html: README.rmd
	"$(R_HOME)/bin/Rscript" -e "rmarkdown::render('README.rmd', output_format = 'html_document', clean = FALSE)"
	mv README.utf8.md README.md
	rm README.knit.md

build: $(TGZ)

test: build
	@echo "Running testthat tests..."
	"$(R_HOME)/bin/Rscript" -e 'library(devtools); devtools::test("pkg")' 2>&1 | tee test.log
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
