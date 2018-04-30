## Stencila for R

[![Build status](https://travis-ci.org/stencila/r.svg?branch=master)](https://travis-ci.org/stencila/r)
[![Code coverage](https://codecov.io/gh/stencila/r/branch/master/graph/badge.svg)](https://codecov.io/gh/stencila/r)
[![CRAN](http://www.r-pkg.org/badges/version/stencila)](https://cran.r-project.org/package=stencila)
[![Community](https://img.shields.io/badge/join-community-green.svg)](https://community.stenci.la)
[![Chat](https://badges.gitter.im/stencila/stencila.svg)](https://gitter.im/stencila/stencila)

### Install

This package isn't on CRAN yet, but you can install it from this repository using the [`devtools`](https://github.com/hadley/devtools) package,

```r
devtools::install_github("stencila/r")
```

Alternatively, check the [releases page](https://github.com/stencila/r/releases) for prebuilt versions.

Then install the package so that other Stencila packages and applications can detect it:

```r
stencila:::install()
```

**Note** Installing Stencila package for R may take a while as there are a number of dependencies which need to be installed. 

### Use

This package lets you run R code from inside Stencila Documents. When you start the [Stencila Desktop](https://github.com/stencila/desktop) it will be automatically detect the installed R package and you'll be able to execute R code cells from within your documents.

Check out what people are doing with Stencila and R - [Giulio Valentino Dalla Riva](https://github.com/gvdr) has created Tiny Open Access Data Samples ([TOADS](https://github.com/gvdr/toads/)). These awesome open datasets teach programming and statistics using Stencila. Check out the TOADS!

More documentation is available at https://stencila.github.io/r

### Discuss

We love feedback. Create a [new issue](https://github.com/stencila/r/issues/new), add to [existing issues](https://github.com/stencila/r/issues) or [chat](https://gitter.im/stencila/stencila) with members of the community.

### Develop

Most development tasks can be run using `make` shortcuts or RStudio keyboard shortcuts.

Task                                                    | `make`                | RStudio         |
------------------------------------------------------- |-----------------------|-----------------|
Install dependencies                                    | `make setup`          | 
Run tests                                               | `make test`           | `Ctrl+Shift+T`
Run tests with coverage                                 | `make cover`          |
Build documentation                                     | `make docs`           |
Check the package                                       | `make check`          | `Ctrl+Shift+E`
Build                                                   | `make build`          | `Ctrl+Shift+B`
Clean                                                   | `make clean`          |

To get started, please read our contributor [code of conduct](CONDUCT.md), then [get in touch](https://gitter.im/stencila/stencila) or checkout the [platform-wide, cross-repository kanban board](https://github.com/orgs/stencila/projects/1), or just send in a pull request!

Unit tests live in the `tests` folder and are mostly written using `testthat`. Documentation is written using `roxygen2` and the documentation site is generated by `pkgdown` into the [`docs`](docs) folder and published on Github pages.

Tests are run on [Travis](https://travis-ci.org/stencila/r) and code coverage tracked at [Codecov](https://codecov.io/gh/stencila/r).
