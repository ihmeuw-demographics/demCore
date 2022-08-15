#' @title Generate ax for ages under 5 years, from mx
#'
#' @description Add ax onto a data.table for ages <1 and 1-4 years, with
#'   algorithm based on under-1 mx.
#'
#' @param dt \[`data.table()`\]\cr Columns:
#'   * 'age_start' with values 0 (for under-1) and 1 (for 1-4). Other ages
#'      permitted but will result in NA output for ax or unchanged ax if ax in
#'      input `dt`.
#'   * 'age_end' with values 1 (for under-1) and 5 (for 1-4).
#'   * 'sex' where values must be 'male', 'female', or 'both'
#'   * 'mx' for mortality rate
#'   * All additional columns from 'id_cols'
#' @param id_cols \[`character()`\]\cr Columns that uniquely identify
#'   observations in `dt`. Must include 'age_start', 'age_end', and 'sex'.
#'
#' @return Modifies `dt` in place with 'ax' column added on.
#'
#' @details Takes a `data.table` with 'age_start', 'age_end', 'sex', and
#'   infant mortality rate ('mx') and adds a column 'ax'.
#'
#'   The following table shows the conversions from 1m0 to 1a0 and 4a1. Note
#'   that when sex is "both" the relationship is a mean of the male and female
#'   relationships.
#'
#'  |                     || Males               || Females             |
#'  | ------------------- |-|--------------------|-| -------------------|
#'  | 1a0:                ||                     ||                     |
#'  | If 1m0 >= 0.107     || 0.330               || 0.350               |
#'  | If 1m0 < 0.107      || 0.045 + 2.684 * 1m0 || 0.053 + 2.800 * 1m0 |
#'  |                     ||                     ||                     |
#'  | 4a1:                ||                     ||                     |
#'  | If 1m0 >= 0.107     || 1.352               || 1.361               |
#'  | If 1m0 < 0.107      || 1.651 - 2.816 * 1m0 || 1.522 - 1.518 * 1m0 |
#'
#' @references
#' Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling
#'   population processes. MA: Blackwell Publishing. 2001.
#'
#' @examples
#' dt <- data.table::data.table(
#'   age_start = c(0, 1),
#'   age_end = c(1, 5),
#'   mx = c(0.09, 0.12),
#'   sex = c("male", "male")
#' )
#' gen_u5_ax_from_mx(dt, id_cols = c("age_start", "age_end", "sex"))
#' @export

gen_u5_ax_from_mx <- function(dt, id_cols) {

  # validate/prep ----------------------------------------------------------

  # high/low infant mx threshold from Preston book
  threshold <- 0.107

  # get `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # validate
  validate_gen_u5_ax(dt, id_cols, param_cols = "mx")

  # merge on 1m0 -----------------------------------------------------------

  dt[, mx_inf := .SD[age_start == 0 & age_end == 1, mx],
     by = id_cols_no_age]

  # require 1m0 for all `id_col` combinations
  if(nrow(dt[is.na(mx_inf)]) > 0) {
    stop("You do not have age 0-1 years for all `id_col` combinations.")
  }

  # calculate 1a0 ----------------------------------------------------------

  dt[sex == "male" & age_start == 0 & age_end == 1,
     ax := ifelse(mx_inf >= threshold, 0.330, 0.045 + 2.684 * mx_inf)]
  dt[sex == "female" & age_start == 0 & age_end == 1,
     ax := ifelse(mx_inf >= threshold, 0.350, 0.053 + 2.800 * mx_inf)]
  dt[sex == "both" & age_start == 0 & age_end == 1,
     ax := ifelse(mx_inf >= threshold, 0.340, 0.049 + 2.742 * mx_inf)]

  # calculate 4a1 ----------------------------------------------------------

  dt[sex == "male" & age_start == 1 & age_end == 5,
     ax := ifelse(mx_inf >= threshold, 1.352, 1.651 - 2.816 * mx_inf)]
  dt[sex == "female" & age_start == 1 & age_end == 5,
     ax := ifelse(mx_inf >= threshold, 1.361, 1.522 - 1.518 * mx_inf)]
  dt[sex == "both" & age_start == 1 & age_end == 5,
     ax := ifelse(mx_inf >= threshold, 1.3565, 1.5865 - 2.167 * mx_inf)]

  # clean up and return -----------------------------------------------------

  dt[, c("mx_inf") := NULL]

}



#' @title Generate ax for ages under 5 years, from qx
#'
#' @description Add ax onto a data.table for ages <1 and 1-4 years, with
#'   algorithm based on under-1 qx.
#'
#' @param dt \[`data.table()`\]\cr Columns:
#'   * 'age_start' with values 0 (for under-1) and 1 (for 1-4). Other ages
#'      permitted but will result in NA output for ax or unchanged ax if ax in
#'      input `dt`.
#'   * 'age_end' with values 1 (for under-1) and 5 (for 1-4).
#'   * 'sex' where values must be 'male', 'female', or 'both'
#'   * 'qx' for mortality rate
#'   * All additional columns from 'id_cols'
#' @param id_cols \[`character()`\]\cr Columns that uniquely identify
#'   observations in `dt`. Must include 'age_start', 'age_end', and 'sex'.
#'
#' @return Modifies `dt` in place with 'ax' column added on.
#'
#' @details Takes a `data.table` with 'age_start', 'age_end', 'sex', and
#'   infant probability of death ('qx') and adds a column 'ax'. There is no
#'   absolute relationship between ax and qx without mx also known. Instead,
#'   this function performs an inverse calculation of the calculation described
#'   for [demCore::gen_u5_ax_from_mx()] which estimates ax from 1m0. Since
#'   the inverse involves a quadratic, we use [stats::uniroot()] to find the
#'   solution.
#'
#' @references
#' Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling
#'   population processes. MA: Blackwell Publishing. 2001.
#'
#' @seealso [demCore::gen_u5_ax_from_mx()]
#'
#' @examples
#' dt <- data.table::data.table(
#'   age_start = c(0, 1),
#'   age_end = c(1, 5),
#'   qx = c(0.0846, 0.3658),
#'   sex = c("male", "male")
#' )
#' gen_u5_ax_from_qx(dt, id_cols = c("age_start", "age_end", "sex"))
#' @export

gen_u5_ax_from_qx <- function(dt, id_cols) {

  # validate/prep ----------------------------------------------------------

  # get `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # validate
  validate_gen_u5_ax(dt, id_cols, param_cols = "qx")

  # merge on 1q0 -----------------------------------------------------------

  dt[, qx_inf := .SD[age_start == 0 & age_end == 1, qx],
     by = id_cols_no_age]

  # require 1q0 for all `id_col` combinations
  if(nrow(dt[is.na(qx_inf)]) > 0) {
    stop("You do not have age 0-1 years for all `id_col` combinations.")
  }

  # calculate ax -----------------------------------------------------------

  # split up by life table
  dt <- rbindlist(lapply(
    split(dt, by = id_cols_no_age),
    solve_u5_ax_from_qx
  ))

  # clean up and return -----------------------------------------------------

  dt[, c("qx_inf") := NULL]

}


# ==========================================================================
#' @title Helper objective function for `gen_u5_ax_from_qx`
#' @description Function we want to zero. Uses relationship between under-5
#'   mx and ax as described in [gen_u5_ax_from_mx()]. **Use only for 1a0.**
#' @param ax \[`numeric(1)`\] average person-years lived by those who die (1a0)
#' @param qx \[`numeric(1)`\] probability of death (1q0)
#' @param A \[`numeric(1)`\] Intercept for 1m0 --> 1a0
#' @param B \[`numeric(1)`\] Slope for 1m0 --> 1a0
#' @keywords internal
zero_u5_ax_from_qx <- function(ax, qx, A, B) {
  mx_inf <- demCore::qx_ax_to_mx(qx = qx, ax = ax, age_length = 1)
  zero <- (A + B * mx_inf) - ax
  return(zero)
}



# ==========================================================================
#' @title Helper solve function for `gen_u5_ax_from_qx`
#' @description Function which solves for zero. Uses relationship between
#'   under-5 mx and ax as described in [gen_u5_ax_from_mx()].
#' @param dt \[`data.table()`\] A single life table.
#' @keywords internal
solve_u5_ax_from_qx <- function(dt) {

  dt <- copy(dt)

  # settings
  if (unique(dt$sex) == "male") {
    # high/low mx threshold coverted to qx
    threshold <- mx_ax_to_qx(mx = 0.107, ax = 0.330, age_length = 1)
    # ax value at that high/low threshold
    baseline_inf <- 0.330
    baseline_ch <- 1.352
    # slope/intercept
    A_inf <- 0.045
    B_inf <- 2.684
    A_ch <- 1.651
    B_ch <- 2.816
  } else {
    threshold <- mx_ax_to_qx(mx = 0.107, ax = 0.350, age_length = 1)
    baseline_inf <- 0.350
    baseline_ch <- 1.361
    A_inf <- 0.053
    B_inf <- 2.800
    A_ch <- 1.522
    B_ch <- 1.518
  }

  # use baseline ax if greater than or equal to threshold
  if (dt[age_start == 0 & age_end == 1, qx] >= threshold) {
    ax_inf <- baseline_inf
    ax_ch <- baseline_ch

  } else {
    # solve for 1a0
    ax_inf <- stats::uniroot(
      zero_u5_ax_from_qx,
      interval = c(0, 1),
      qx = dt[age_start == 0 & age_end == 1, qx],
      A = A_inf, B = B_inf
    )$root
    # calculate 1m0
    mx_inf <- qx_ax_to_mx(
      qx = dt[age_start == 0 & age_end == 1, qx],
      ax = ax_inf,
      age_length = 1
    )
    # calculate 4a1
    ax_ch <- A_ch - B_ch * mx_inf
  }

  # add ax onto data.table
  dt[age_start == 0 & age_end == 1, ax := ax_inf]
  dt[age_start == 1 & age_end == 5, ax := ax_ch]

  return(dt)
}



# ==========================================================================
#' @title Helper for `gen_u5_ax` validations
#' @inheritParams gen_u5_ax_from_mx
#' @param param_cols \[`character()`\] "mx" for `gen_u5_ax_from_mx` and "qx" for
#'     `gen_u5_ax_from_qx`
#' @keywords internal
validate_gen_u5_ax <- function(dt, id_cols, param_cols) {

  # standard validations
  validate_lifetable(dt, id_cols = id_cols, param_cols = param_cols)

  # check age 0-1 is present in `dt`
  if(nrow(dt[age_start == 0 & age_end == 1]) == 0) {
    stop("Age 0-1 years must be in `dt`.")
  }

  # check for age 1-4 years in `dt`. Not required but will usually be present
  # and may be miscoded.
  if(nrow(dt[age_start == 1 & age_end == 5]) == 0) {
    warning("'dt' does not have any rows with 'age_start' 1 and 'age_end' 5
            (1-4 years). Make sure you expect this.")
  }

  # add message for ages not <1 or 1-4 in `dt`
  if(length(setdiff(unique(dt$age_start), c(0,1)) > 0)) {
    message("Rows with age not 0-1 or 1-4 will not be assigned ax values.")
  }

  # check sex
  assertthat::assert_that("sex" %in% id_cols,
                          msg = "`id_cols` must include 'sex'.")
  assertable::assert_values(dt, colnames = "sex", test = "in",
                            test_val = c("male", "female", "both"), quiet = T)
}
