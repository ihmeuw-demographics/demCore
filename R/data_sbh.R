#' @title Example children ever born and surviving data from a Panama 1976
#'  survey.
#'
#' @description Example children ever born and surviving data from a Panama 1976
#'  survey as described in the UN Population Division Manual X table 49.
#'
#' @format
#' 21 rows and 6 columns  \[`data.table()`\] with counts of children ever born
#'   alive (ceb) and children ever born alive who died (ced) tabulated by 5-year
#'   maternal age  groups between age 15 and 49 and the child's sex. Also
#'   includes a column for the number of women in each age group (n_women).
#'
#' @references
#' UN Population Division. 1983. Manual X: Indirect Techniques for Demographic
#' Estimation. New York: United Nations, Department of Economic and Social
#' Affairs, ST/ESA/SER.A/81.
#' http://www.un.org/esa/population/techcoop/DemEst/manual10/manual10.html
#'
#' @seealso [calc_nqx_brass()]
#' @seealso [sbh_trussell_coeffs()]
#'
#' @rdname sbh_data
"sbh_panama_1976"

#' @title Trussell multipliers to use in the Brass sbh method.
#'
#' @description Trussell multipliers to use in the Brass sbh method to convert
#' parity ratios and proportion of children ever born who died into estimates of
#' probability of dying before certain child ages. As described in the UN
#' Population Division Manual X tables 47 and 48.
#'
#' @format
#' 28 rows and 9 columns \[`data.table()`\]
#'   - `model`: the Coale-Demeny model fertility schedule used to produce the
#'   coefficients.
#'   - `type`: whether the coefficients are to be used for estimating the child
#'   mortality level ('q(x)/D(x)') or reference period ('t(x)').
#'   - `age_start`: start of maternal age group (inclusive).
#'   - `age_end`: end of maternal age group (exclusive).
#'   - `age_start_nqx`: start of child age group the probability of dying
#'   applies to (inclusive).
#'   - `age_start_nqx`: end of child age group the probability of dying applies
#'   to (exclusive).
#'   - `a_i`: `a(i)` coefficient
#'   - `b_i`: `b(i)` coefficient
#'   - `c_i`: `c(i)` coefficient
#'
#' @references
#' UN Population Division. 1983. Manual X: Indirect Techniques for Demographic
#' Estimation. New York: United Nations, Department of Economic and Social
#' Affairs, ST/ESA/SER.A/81.
#' http://www.un.org/esa/population/techcoop/DemEst/manual10/manual10.html
#'
#' @seealso [calc_nqx_brass()]
#' @seealso [sbh_panama_1976()]
#'
#' @rdname sbh_data
"sbh_trussell_coeffs"
