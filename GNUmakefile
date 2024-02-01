PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
TGZ     := $(PKGNAME)_$(PKGVERS).tar.gz
WINBIN  := $(PKGNAME)_$(PKGVERS).zip
RBIN ?= $(shell dirname "`which R`")

.PHONEY: check

pkgfiles = \
	.Rbuildignore \
	DESCRIPTION \
	data/* \
	GNUmakefile \
	inst/data_generation/* \
	inst/testdata/* \
	README.html \
	R/* \
	tests/testthat.R \
	tests/testthat/*

all: build

roxy:
	Rscript -e "roxygen2::roxygenize(roclets = c('rd', 'collate', 'namespace'))"

README.html: README.md
	"$(RBIN)/Rscript" -e "rmarkdown::render('README.md', output_format = 'html_document', output_options = list(mathjax = NULL))"

$(TGZ): $(pkgfiles)
	"$(RBIN)/R" CMD build . 2>&1 | tee log/build.log

build: roxy $(TGZ)

install: build
	"$(RBIN)/R" CMD INSTALL $(TGZ)

$(WINBIN): build
	@echo "Building windows binary package..."
	"$(RBIN)/R" CMD INSTALL $(TGZ) --build
	@echo "DONE."

winbin: $(WINBIN)

check: roxy build
	_R_CHECK_CRAN_INCOMING_REMOTE_=false "$(RBIN)/R" CMD check --as-cran --no-tests $(TGZ) 2>&1 | tee log/check.log

test: install
	"$(RBIN)/Rscript" -e 'options(cli.dynamic = TRUE); devtools::test()' 2>&1 | tee log/test.log
	sed -i -e "s/.*\r.*\r//" log/test.log

drat: build
	"$(R_HOME)/bin/Rscript" -e "drat::insertPackage('$(TGZ)', commit = TRUE)"

dratwin: winbin
	"$(R_HOME)/bin/Rscript" -e "drat::insertPackage('$(WINBIN)', '~/git/drat/', commit = TRUE)"

winbuilder: build
	date
	@echo "Uploading to R-release on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-release/
	@echo "Uploading to R-devel on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-devel/

pd: roxy
	Rscript -e 'pkgdown::build_site(lazy = TRUE, run_dont_run = TRUE)'

pd_all: roxy
	Rscript -e 'pkgdown::build_site(lazy = FALSE, run_dont_run = TRUE)'
