# lifetableUtils

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/ihmeuw/lifetableUtils.svg?branch=master)](https://travis-ci.com/ihmeuw/lifetableUtils)
[![Codecov test coverage](https://codecov.io/gh/ihmeuw/lifetableUtils/branch/master/graph/badge.svg)](https://codecov.io/gh/ihmeuw/lifetableUtils?branch=master)
<!-- badges: end -->

Utility functions for calculating life table parameters and performing common
manipulations of life tables. Life table parameters include mx, qx, ax, lx, dx,
Tx, nLx, and ex. Additional convenience functions include aggregation, scaling,
moving between abridged and full life tables, and others.

### Authors

All of the following authors of this package are at the [Institute for Health
Metrics and Evaluation](http://www.healthdata.org/) at the University of
Washington.

|                    |                 |
|--------------------|-----------------|
| Katie Paulson      | krpaul@uw.edu   |
| Charlton Callender | chacalle@uw.edu |
| Spencer Pease      | spease@uw.edu   |

### Installation

This package has not yet been published on CRAN, but can be installed from
GitHub using the [remotes](https://remotes.r-lib.org/) package. Installation of
the [demUtils](https://github.com/ihmeuw/demUtils) package is also required.

```r
remotes::install_github("ihmeuw/demUtils")
remotes::install_github("ihmeuw/lifetableUtils")
```

### Additional resources

Much of the content in this package comes from well established demographic
methods, as reported in the following book:

*Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling
population processes. MA: Blackwell Publishing. 2001.*
