all: setup build

setup:
	Rscript -e 'install.packages(c("devtools","roxygen2","testthat","covr"),repo="http://cloud.r-project.org/")'

build:
	R CMD INSTALL .

check:
	R CMD check .

test:
	Rscript -e 'devtools::test()'

cover:
	Rscript -e 'cov<-covr::package_coverage("."); cat(covr:::to_codecov(cov),file="coverage.json"); print(cov)'

clean:
	rm -rf ..Rcheck
