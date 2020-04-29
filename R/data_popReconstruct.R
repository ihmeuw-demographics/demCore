#' @title Burkina Faso example population data and ccmpp initial estimates
#'
#' @description Example data and initial estimates for reconstructing the female
#'   population in Burkina Faso from 1960 to 2005.
#'
#' @format
#' `burkina_faso_data`: list containing sex-specific matrices of population
#' count data.
#'   * population: named list with female matrix of age-specific census
#'   counts in years after the baseline year (1975, 1985, 1995, 2005). Rows
#'   correspond to age groups while columns correspond to years.
#'
#' `burkina_faso_initial_estimates`: list containing matrices of initial
#' estimates for each [ccmpp()] input.
#'   * srb: single row matrix of initial estimates of sex ratio at birth.
#'   * asfr: matrix of initial estimates of average annual single-year
#'   age-specific fertility rates.
#'   * baseline: List with female single column matrix of age-specific
#'   population counts in the baseline year (1960).
#'   * survival: List with female matrix of initial estimates of age-specific
#'   survivorship ratios
#'   * net_migration: List with female matrix of initial estimates of
#'   age-specific average annual net migration proportions.
#'
#' @seealso [ccmpp()], [populationMethods::popReconstruct()]
#'
#' @references
#' Wheldon, Mark C., Adrian E. Raftery, Samuel J. Clark, and Patrick Gerland.
#' 2013. “Reconstructing Past Populations With Uncertainty From Fragmentary
#' Data.” Journal of the American Statistical Association 108 (501): 96–110.
#' [https://doi.org/10.1080/01621459.2012.737729](https://doi.org/10.1080/01621459.2012.737729).
#'
#' [popReconstruct R Package](https://cran.r-project.org/web/packages/popReconstruct/popReconstruct.pdf)
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
#' `thailand_data`: list containing sex-specific matrices of population
#' count data.
#'   * population: named list with female and male matrices of age-specific
#'   census counts in years after the baseline year (1970, 1980, 1990, 2000).
#'   Rows correspond to age groups while columns correspond to years.
#'
#' `thailand_initial_estimates`: list containing matrices of initial
#' estimates for each [ccmpp()] input.
#'   * srb: single row matrix of initial estimates of sex ratio at birth.
#'   * asfr: matrix of initial estimates of average annual single-year
#'   age-specific fertility rates.
#'   * baseline: List with female and male single column matrices of
#'   age-specific population counts in the baseline year (1960).
#'   * survival: List with female and male matrices of initial estimates of
#'   age-specific survivorship ratios
#'   * net_migration: List with female and male matrices of initial estimates of
#'   age-specific average annual net migration proportions.
#'
#' @seealso [ccmpp()], [populationMethods::popReconstruct()]
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
#' @rdname thailand
"thailand_data"

#' @rdname thailand
"thailand_initial_estimates"
