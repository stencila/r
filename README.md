## `stencila/r`

[![Build status](https://travis-ci.org/stencila/r.svg?branch=master)](https://travis-ci.org/stencila/r)
[![Code coverage](https://codecov.io/gh/stencila/r/branch/master/graph/badge.svg)](https://codecov.io/gh/stencila/r)
[![Chat](https://badges.gitter.im/stencila/stencila.svg)](https://gitter.im/stencila/stencila)

Stencila components for R

### Development

Most development tasks can be run using `make` or RStudio keyboard shortcuts.

Task                                                    | `make`                | RStudio         |
------------------------------------------------------- |-----------------------|-----------------|
Install dependencies                                    | `make setup`          | 
Run tests                                               | `make test`           | `Ctrl+Shift+T`
Run tests with coverage                                 | `make cover`          |
Check the package                                       | `make check`          | `Ctrl+Shift+E`
Build                                                   | `make build`          | `Ctrl+Shift+B`
Clean                                                   | `make clean`          |

Unit tests live in the `tests` folder and are mostly written using the [`testthat`](https://github.com/hadley/testthat) test harness.
