#' @title Example life table
#'
#' @description Example death count and population values. Also
#'   includes mx (deaths / population), ax, and qx calculated using functions
#'   from this package.
#'
#' @format \[`data.table()`\] columns:
#' \describe{
#'  \item{age_start}{\[`integer()`\]\cr Abridged ages 0, 1, 5, 10, ..., 95}
#'  \item{age_end}{\[`integer()`\]\cr End of age group inclusive, 1, 5, 10,
#'     ... , 95, Inf}
#'  \item{deaths}{\[`numeric()`\]\cr Death counts}
#'  \item{population}{\[`numeric()`\]\cr Population counts}
#'  \item{mx}{\[`numeric()`\]\cr Mortality rate (deaths / population)}
#'  \item{ax}{\[`numeric()`\]\cr Mean person-years lived by those who die in
#'    interval. Obtained using `mx_to_ax` function.}
#'  \item{qx}{\[`numeric()`\]\cr Probability of death in interval conditional
#'    on survival to xth birthday. Obtained using `mx_ax_to_qx` function.}
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
#' @format \[`data.table()`\] columns:
#' \describe{
#'  \item{sex}{\[`character()`\]\cr Sex the parameters correspond to: "male" or
#'    "female"}
#'  \item{age_start}{\[`integer()`\]\cr Single-year ages 1-109 representing the
#'    age the parameters correspond to}
#'  \item{age_end}{\[`integer()`\]\cr 1 + age_start}
#'  \item{intercept}{\[`numeric()`\]\cr Intercept for the age- and sex-specific
#'    regression model fit}
#'  \item{slope}{\[`numeric()`\]\cr Slope for the age- and sex-specific
#'    regression model fit}
#' }
#' @examples
#' fullLTpars
"fullLTpars"
