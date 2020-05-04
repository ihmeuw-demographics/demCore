#' @title Burkina Faso example population data and ccmpp initial estimates
#'
#' @description Example data and initial estimates for reconstructing the female
#'   population in Burkina Faso from 1960 to 2005.
#'
#' @format
#' `burkina_faso_data`: \[`data.table()`\] with 'population' data.
#'   * population: \[`data.table()`\] year-sex-age-specific census counts in
#'   years after the baseline year (1975, 1985, 1995, 2005). Age groups are
#'   five-year age groups from 0 to 80+.
#'
#' `burkina_faso_initial_estimates`: list of \[`data.table()`\] of initial
#' estimates for each [ccmpp()] input. Calendar year intervals are for five-year
#' intervals between 1960 and 2005. Age groups are five-year age groups from 0
#' to 80+ (except 'survival' goes up to 85+). See **Section: CCMPP inputs** and
#' **Section: Possible \[`data.table()`\] columns for inputs** for more information
#' on each of the inputs.
#'
#' @inheritSection ccmpp CCMPP inputs
#'
#' @inheritSection ccmpp Possible \[`data.table()`\] columns for inputs
#'
#' @references
#' Wheldon, Mark C., Adrian E. Raftery, Samuel J. Clark, and Patrick Gerland.
#' 2013. “Reconstructing Past Populations With Uncertainty From Fragmentary
#' Data.” Journal of the American Statistical Association 108 (501): 96–110.
#' [https://doi.org/10.1080/01621459.2012.737729](https://doi.org/10.1080/01621459.2012.737729).
#'
#' [popReconstruct R Package](https://cran.r-project.org/web/packages/popReconstruct/popReconstruct.pdf)
#'
#' @seealso [ccmpp()], [popMethods::popReconstruct()]
#'
#' @rdname burkina_faso
"burkina_faso_data"

#' @rdname burkina_faso
"burkina_faso_initial_estimates"

#' @title Thailand example population data and ccmpp initial estimates
#'
#' @description Example data and initial estimates for reconstructing the female
#'   and male population in Thailand from 1960 to 2000.
#'
#' @format
#' `thailand_data`: \[`data.table()`\] with 'population' data.
#'   * population: \[`data.table()`\] year-sex-age-specific census counts in
#'   years after the baseline year (1970, 1980, 1990, 2000). Age groups are
#'   five-year age groups from 0 to 80+.
#'
#' `thailand_initial_estimates`: list of \[`data.table()`\] of initial
#' estimates for each [ccmpp()] input. Calendar year intervals are for five-year
#' intervals between 1960 and 2000. Age groups are five-year age groups from 0
#' to 80+ (except 'survival' goes up to 85+). See **Section: CCMPP inputs** and
#' **Section: Possible \[`data.table()`\] columns for inputs** for more information
#' on each of the inputs.
#'
#' @inheritSection ccmpp CCMPP inputs
#'
#' @inheritSection ccmpp Possible \[`data.table()`\] columns for inputs
#'
#' @references
#' Wheldon, Mark C., Adrian E. Raftery, Samuel J. Clark, and Patrick Gerland.
#' 2015. “Bayesian Reconstruction of Two-Sex Populations by Age: Estimating Sex
#' Ratios at Birth and Sex Ratios of Mortality.” Journal of the Royal
#' Statistical Society. Series A: Statistics in Society 178 (4): 977–1007.
#' [https://doi.org/10.1111/rssa.12104](https://doi.org/10.1111/rssa.12104).
#'
#' [markalava/Bayesian-Reconstruction github repo](https://github.com/markalava/Bayesian-Reconstruction)
#'
#' @seealso [ccmpp()], [popMethods::popReconstruct()]
#'
#' @rdname thailand
"thailand_data"

#' @rdname thailand
"thailand_initial_estimates"
