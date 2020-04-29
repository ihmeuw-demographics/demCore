#' @title Burkina Faso example population data and ccmpp initial estimates
#'
#' @description Example data and initial estimates for reconstructing the female
#'   population in Burkina Faso from 1960 to 2005.
#'
#' @format
#' `burkina_faso_data`: \[`data.table()`\] with 'population' data.
#'   * population: \[`data.table()`\] year-sex-age-specific census counts in
#'   years after the baseline year (1975, 1985, 1995, 2005).
#'
#' `burkina_faso_initial_estimates`: list of \[`data.table()`\] of initial
#' estimates for each [ccmpp()] input.
#'   * srb: \[`data.table()`\] of year-specific sex ratio at birth estimates.
#'   * asfr: \[`data.table()`\] of year-age-specific average annual single-year
#'   age-specific fertility rate estimates.
#'   * baseline: \[`data.table()`\] of year-sex-age specific baseline year
#'   (1960) population counts.
#'   * survival: \[`data.table()`\] of year-sex-age-specific survivorship ratio
#'   estimates.
#'   * net_migration: \[`data.table()`\] of year-sex-age-specific average annual
#'   net migration proportion estimates
#'
#' @details
#' Possible \[`data.table()`\] columns for initial estimates and data:
#'
#' All contain:
#'   * value: \[`numeric()`\] contains value for each group.
#'
#' If year-specific:
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive).
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'
#' If sex-specific:
#'   * sex: \[`character()`\] either 'female' or 'male'.
#'
#' If age-specific:
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'
#' @references
#' Wheldon, Mark C., Adrian E. Raftery, Samuel J. Clark, and Patrick Gerland.
#' 2013. “Reconstructing Past Populations With Uncertainty From Fragmentary
#' Data.” Journal of the American Statistical Association 108 (501): 96–110.
#' [https://doi.org/10.1080/01621459.2012.737729](https://doi.org/10.1080/01621459.2012.737729).
#'
#' [popReconstruct R Package](https://cran.r-project.org/web/packages/popReconstruct/popReconstruct.pdf)
#'
#' @seealso [ccmpp()], [populationMethods::popReconstruct()]
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
#'   years after the baseline year (1970, 1980, 1990, 2000).
#'
#' `thailand_initial_estimates`: list of \[`data.table()`\] of initial
#' estimates for each [ccmpp()] input.
#'   * srb: \[`data.table()`\] of year-specific sex ratio at birth estimates.
#'   * asfr: \[`data.table()`\] of year-age-specific average annual single-year
#'   age-specific fertility rate estimates.
#'   * baseline: \[`data.table()`\] of year-sex-age specific baseline year
#'   (1960) population counts.
#'   * survival: \[`data.table()`\] of year-sex-age-specific survivorship ratio
#'   estimates.
#'   * net_migration: \[`data.table()`\] of year-sex-age-specific average annual
#'   net migration proportion estimates
#'
#' @inherit burkina_faso_data details
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
#' @seealso [ccmpp()], [populationMethods::popReconstruct()]
#'
#' @rdname thailand
"thailand_data"

#' @rdname thailand
"thailand_initial_estimates"
