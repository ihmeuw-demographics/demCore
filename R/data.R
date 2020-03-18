#' @title Example life table (Norway 2010 females, from GBD 2019)
#'
#' @description GBD 2019 death count and population estimates, for females in
#'   Norway in 2010, made using Norway vital registration and census data. Also
#'   includes mx (deaths / population), and ax and qx calculated using functions
#'   from this package.
#'
#' @source Vital registration data obtained from: ADD SOURCE HERE.
#'  Census data obtained from: ADD SOURCE HERE.
#'  See GBD methods as described in GBD 2019 Lancet paper and methods appendix
#'   (cite GBD paper here).
#'
#' @format A data.table with columns:
#' \describe{
#'  \item{age}{Integer, abridged ages 0, 1, 5, 10, ..., 95}
#'  \item{location}{Character, "Norway"}
#'  \item{deaths}{GBD 2019 death count estimate for Norway 2010 females}
#'  \item{population}{GBD 2019 population estimate for Norway 2010 females}
#'  \item{age_length}{Integer, length in years of an age group}
#'  \item{mx}{mortality rate (deaths / population)}
#'  \item{ax}{mean person-years lived by those who die in interval. Obtained
#'    using `mx_to_ax` function.}
#'  \item{qx}{probability of death in interval conditional on survival to xth
#'    birthday. Obtained using `mx_ax_to_qx` function.}
#' }
#' @examples
#' fNOR2010
"fNOR2010"



#' Regression parameters for abridged to full life table conversion
#'
#' @description Parameters for age- and sex-specific regression model fit to
#'   Human Mortality Database data, to predict single-year qx values from
#'   abridged qx values:
#'   \deqn{log(q_x) = intercept + slope*log(q_x,abridged)}
#'
#' @source GBD 2019 methods (cite here).
#'   Regression fit to Human Mortality Database life tables (cite here).
#'
#' @format A data.table with columns:
#' \describe{
#'  \item{sex}{Character, "male" or "female" for the sex the parameters
#'    correspond to.}
#'  \item{age}{Integer, single-year ages 1-109 representing the age the
#'    parameters correspond to.}
#'  \item{intercept}{Numeric, intercept for the age- and sex-specific regression
#'    model fit.}
#'  \item{slope}{Numeric, slope for the age- and sex-specific regression model
#'    fit.}
#' }
#' @examples
#' fullLTpars
"fullLTpars"
