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


#' @title Example life table 2
#'
#' @description Example life table from Preston text book page 49. Post-hoc
#'   adjustment has been made to use radix (l0) of 1 instead of 100,000, as
#'   presented in the Preston book.
#'
#'   Original input data if from Austria, for males, from 1992, and comes
#'   from the United Nations, 1994, as cited in Preston book.
#'
#' @format \[`data.table()`\] columns:
#' \describe{
#'  \item{age_start}{\[`integer()`\]\cr Abridged ages 0, 1, 5, 10, ..., 85.
#'    This is the 'x' variable in life table notation.}
#'  \item{age_end}{\[`integer()`\]\cr End of age group inclusive, 1, 5, 10,
#'     ... , 85, Inf}
#'  \item{age_length}{\[`integer()`\]\cr Width of the age interval in years.
#'    This is the 'n' variable in life table notation.}
#'  \item{deaths}{\[`numeric()`\]\cr Death counts in interval}
#'  \item{population}{\[`numeric()`\]\cr Population counts in interval}
#'  \item{mx}{\[`numeric()`\]\cr Mortality rate (deaths / population)}
#'  \item{ax}{\[`numeric()`\]\cr Mean person-years lived by those who die in
#'    interval.}
#'  \item{qx}{\[`numeric()`\]\cr Probability of death in interval conditional
#'    on survival to xth birthday.}
#'  \item{px}{\[`numeric()`\]\cr Probability of survival in interval.}
#'  \item{lx}{\[`numeric()`\]\cr Proportion surviving to age x.}
#'  \item{dx}{\[`numeric()`\]\cr Proportion of cohort dying in age interval
#'    x to x+n.}
#'  \item{nLx}{\[`numeric()`\]\cr Person-years lived between x and x+n per
#'    every one person in the cohort at birth.}
#'  \item{Tx}{\[`numeric()`\]\cr Person-years lived beyond age x per every one
#'    person in the cohort at birth.}
#'  \item{ex}{\[`numeric()`\]\cr Life expectancy.}
#' }
#' @examples
#' exampleLT2
"exampleLT2"


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
