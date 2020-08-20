# demCore

<!-- badges: start -->
[![R build status](https://github.com/ihmeuw-demographics/demCore/workflows/R-CMD-check/badge.svg)](https://github.com/ihmeuw-demographics/demCore/actions)
[![Codecov test coverage](https://codecov.io/gh/ihmeuw-demographics/demCore/branch/master/graph/badge.svg)](https://codecov.io/gh/ihmeuw-demographics/demCore?branch=master)
<!-- badges: end -->

Common core functions for demographic methods.
Includes basic demography calculations such as:

* Mortality: life table composition
* Fertility: TFR, ASFR
* Population: CCMPP, Leslie matrices

## Installation

This package has not yet been published on CRAN, but can be installed from GitHub using the [remotes](https://remotes.r-lib.org/) package.
Installation of the [hierarchyUtils](https://github.com/ihmeuw-demographics/hierarchyUtils) package is also required.

```r
remotes::install_github("ihmeuw-demographics/hierarchyUtils")
remotes::install_github("ihmeuw-demographics/demCore")
```

## Getting help

demCore function documentation and vignettes can be found at [https://ihmeuw-demographics.github.io/demCore/index.html](https://ihmeuw-demographics.github.io/demCore/index.html)

If you encounter a clear bug, missing documentation, or a feature you'd like to see implemented please file an issue on [github](https://github.com/ihmeuw-demographics/demCore/issues) with the information requested in the issue template.

## Contributing

See the [contributing guidelines](https://github.com/ihmeuw-demographics/.github/blob/master/contributing.md) for more information.

## Additional resources

Much of the content in this package comes from well established demographic methods, as reported in the following book:

*Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling population processes. MA: Blackwell Publishing. 2001.*
