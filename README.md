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

<table>
  <tr>
    <td> Katie Paulson </td>
    <td> krpaul@uw.edu </td>
  </tr>
  <tr>
    <td> Charlton Callender </td>
    <td> chacalle@uw.edu </td>
  </tr>
  <tr>
    <td> Spencer Pease </td>
    <td> spease@uw.edu </td>
  </tr>
</table>

### Installation

This package has not yet been published on CRAN, but can be installed from
GitHub using the <a href="https://github.com/r-lib/remotes" target="_blank">remotes
package</a> package. Installation of the
<a href="https://github.com/ihmeuw/demUtils" target="_blank">demUtils package</a>
from GitHub is also required.

```r
remotes::install_github("ihmeuw/demUtils")
remotes::install_github("ihmeuw/lifetableUtils")
```

### Additional resources

Much of the content in this package comes from well established demographic
methods, as reported in the following book:

*Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling
population processes. MA: Blackwell Publishing. 2001.*
