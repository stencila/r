all: setup build

setup:
	Rscript -e 'install.packages(c("devtools","roxygen2","testthat","covr"),repo="http://cloud.r-project.org/")'
	Rscript -e 'devtools::install_github("hadley/pkgdown")'

build:
	Rscript -e 'devtools::document()'
	R CMD build .

install:
	Rscript -e 'devtools::install()'

docs:
	Rscript -e 'devtools::document(); pkgdown::build_site()'
.PHONY: docs

run:
	Rscript -e 'stencila::run()'

check:
	R CMD check $$(ls stencila_*.tar.gz | tail -n 1)

check-as-cran:
	R CMD check $$(ls stencila_*.tar.gz | tail -n 1) --as-cran

test:
	Rscript -e 'devtools::document(); devtools::test()'

cover:
	Rscript -e 'covr::package_coverage(".")'

clean:
	rm -rf ..Rcheck
