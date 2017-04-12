# Stencila for R

[![Build status](https://travis-ci.org/stencila/r.svg?branch=master)](https://travis-ci.org/stencila/r)
[![Code coverage](https://codecov.io/gh/stencila/r/branch/master/graph/badge.svg)](https://codecov.io/gh/stencila/r)
[![CRAN](http://www.r-pkg.org/badges/version/stencila)](https://cran.r-project.org/package=stencila)
[![Chat](https://badges.gitter.im/stencila/stencila.svg)](https://gitter.im/stencila/stencila)

### Install

Right now this package isn't on CRAN, but you can install it from this repo using the [`devtools`](https://github.com/hadley/devtools) package:

```r
devtools::install_github("stencila/r")
```

### Use

At present, this package let's you launch an execution context for R so you can run R code from within your stencils:

```r
# Load the library
library(stencila)

# Start the Stencila R host so that it can respond to 
# external requests to create and call execution contexts
host$start()
```

In the future, you'll also be able to launch and edit your stencils from within R or RStudio but we're still working on that :).
