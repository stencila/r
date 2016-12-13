## `stencila/r` : Stencila for R

[![Build status](https://travis-ci.org/stencila/r.svg?branch=master)](https://travis-ci.org/stencila/r)
[![Code coverage](https://codecov.io/gh/stencila/r/branch/master/graph/badge.svg)](https://codecov.io/gh/stencila/r)
[![Chat](https://badges.gitter.im/stencila/stencila.svg)](https://gitter.im/stencila/stencila)

### Status

![](http://blog.stenci.la/wip.png)

This is very much a work in progress. See our [main repo](https://github.com/stencila/stencila) for more details.

### Install

Right now this package isn't on CRAN, but you can install it from here using the [`devtools`](https://github.com/hadley/devtools) package from within R,

```
devtools::install_github("stencila/r")
```

### Develop

Most development tasks can be run using `make` or RStudio keyboard shortcuts.

Task                                                    | `make`                | RStudio         |
------------------------------------------------------- |-----------------------|-----------------|
Install dependencies                                    | `make setup`          | 
Run tests                                               | `make test`           | `Ctrl+Shift+T`
Run tests with coverage                                 | `make cover`          |
Build documentation                                     | `make docs`           | `devtools::document()`
Check the package                                       | `make check`          | `Ctrl+Shift+E`
Build                                                   | `make build`          | `Ctrl+Shift+B`
Clean                                                   | `make clean`          |

Unit tests live in the `tests` folder and are mostly written using the [`testthat`](https://github.com/hadley/testthat) test harness.
