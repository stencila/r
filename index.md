## Stencila for R

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

### Use

This package lets you run R code from inside Stencila Documents. When you start the [Stencila Desktop](https://github.com/stencila/desktop) it will be automatically detect the installed R package and you'll be able to execute R code cells from within your documents.
