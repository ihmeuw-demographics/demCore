#' @title Example life table
#'
#' @description Example death count and population values. Also
#'   includes mx (deaths / population), and ax and qx calculated using functions
#'   from this package.
#'
#' @format A data.table with columns:
#' \describe{
#'  \item{age_start}{Integer, abridged ages 0, 1, 5, 10, ..., 95}
#'  \item{age_end}{Integer, end of age group inclusive, 1, 5, 10, ... , 95, Inf}
#'  \item{deaths}{Death counts}
#'  \item{population}{Population counts}
#'  \item{mx}{mortality rate (deaths / population)}
#'  \item{ax}{mean person-years lived by those who die in interval. Obtained
#'    using `mx_to_ax` function.}
#'  \item{qx}{probability of death in interval conditional on survival to xth
#'    birthday. Obtained using `mx_ax_to_qx` function.}
#' }
#' @examples
#' exampleLT
"exampleLT"



#' Regression parameters for abridged to full life table conversion
#'
#' @description Parameters for age- and sex-specific regression model fit to
#'   Human Mortality Database data, to predict single-year qx values from
#'   abridged qx values:
#'   \deqn{log(q_x) = intercept + slope*log(q_x,abridged)}
#'
#' @references
#' Human Mortality Database. University of California, Berkeley (USA), and Max
#' Planck Institute for Demographic Research (Germany). Available at
#' www.mortality.org or www.humanmortality.de (data downloaded on Jan 2018).
#'
#' @format A data.table with columns:
#' \describe{
#'  \item{sex}{Character, "male" or "female" for the sex the parameters
#'    correspond to.}
#'  \item{age_start}{Integer, single-year ages 1-109 representing the age the
#'    parameters correspond to.}
#'  \item{age_end}{Integer, 1 + age_start}
#'  \item{intercept}{Numeric, intercept for the age- and sex-specific regression
#'    model fit.}
#'  \item{slope}{Numeric, slope for the age- and sex-specific regression model
#'    fit.}
#' }
#' @examples
#' fullLTpars
"fullLTpars"
