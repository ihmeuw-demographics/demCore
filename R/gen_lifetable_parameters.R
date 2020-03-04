#' Generate life table parameters
#'
#' @description
#' These functions perform life table calculations, to add selected life table
#'   parameters from others, using established demographic relationships.
#'
#' The following are required inputs for given parameters, as written in
#'   these functions:
#'   * dx requires lx
#'   * lx requires qx
#'   * qx requires lx
#'   * ex requires lx and Tx
#'   * Tx requires nLx
#'   * nLx requires lx, ax, and dx
#'
#' @param dt data.table with lx, ax, dx, mx, age, and age_length variables.
#' @param id_cols character vector of id columns that uniquely identify each row
#'   of `dt`.
#' @param terminal_age numeric, the terminal age group for the data. Default: 110.
#' @param assert_na logical, whether to check for NA values in the generated nLx
#'   variable.
#' @param param_cols character vector of columns containing life table
#'   parameters (qx, lx, etc.)
#'   -- `param_cols` used only for 'validate_param_conversion_input' function
#'
#' @return dt with column added for new life table parameter.
#'
#' @details
#' **Parameter definitions:**
#' For an age interval x to x+n:
#'   * mx = mortality rate (deaths / person-years)
#'   * qx = probability of death, conditional on survival to x-th birthay
#'   * ax = average years lived of those who died in the age interval
#'   * dx = number (or proportion of cohort) who died in the age interval
#'   * lx = number (or proportion of cohort) alive at the beginning of the age
#'     interval
#'   * ex = life expectancy, expected value for years lived after age x
#'   * Tx =  total person-years lived beyond age x
#'   * nLx = total person-years lived in interval
#'
#' **qx_to_lx:** We can use probability of death (qx) to get the proportion of
#'   survivors in the beginning of an age group (lx).
#'   \deqn{At age = 0: lx = 1}
#'   \deqn{At age > 0: lx = lx_previous * (1-qx_previous)}
#'
#' **lx_to_qx:** We can use survival proportion (lx) and survival proportion at
#'   next age group (l{x+n}) to get probability of death (qx).
#'   \deqn{qx = 1 - (l{x+n}/lx)}
#'   \deqn{For terminal age group: qx = 1}
#'
#' **lx_to_dx:** Given the proportion surviving to each age group (lx), we can
#'   calculate the proportion dying in the age interval (dx).
#'  \deqn{dx = lx - l{x+n}}
#'  \deqn{For terminal age group: dx = lx}
#'
#' **gen_nLx:** To get \eqn{nLx} we calculate years lived by survivors plus years
#'   lived by those who died. For terminal ages, assume that average person years
#'   lived equals number at the start of the age group divided by the death rate.
#'   \deqn{nLx = n * l{x+n} + ax * d_x}
#'   \deqn{For terminal age group: infinityLx = lx / mx}
#'
#' **gen_Tx:** Person-years lived above age x (Tx) is based on person-years
#'   lived by people in each age interval above age x (nLx).
#'   \deqn{For terminal age group: Tx = nLx}
#'   \deqn{Otherwise: Tx = cumulative sum of nLx beyond age x}
#'
#' **gen_ex:** Life expectancy (ex) at age x is based on person-years lived
#'   above the age group (Tx) and proportion of people alive at the start of
#'   age group (lx).
#'   \deqn{ex = Tx / lx}
#'
#' @seealso lifetableUtils::mx_qx_ax_conversions
#'
#' @examples
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age = c(0, 5, 10, 15),
#'   age_length = c(5, 5, 5, 120),
#'   mx = c(0.1, 0.2, 0.3, 0.4),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
#' dt <- qx_to_lx(dt, id_cols = c("sex", "age"))
#' dt <- lx_to_dx(dt, id_cols = c("sex", "age"), terminal_age = 15)
#' dt <- gen_nLx(dt, id_cols = c("sex", "age"), terminal_age = 15)
#' dt <- gen_Tx(dt, id_cols = c("sex", "age"))
#' dt <- gen_ex(dt)
#' dt <- lx_to_qx(dt, id_cols = c("sex", "age"), terminal_age = 15)
#'
#' @name gen_lifetable_parameters
NULL

# ============================================================================
#' @rdname gen_lifetable_parameters
validate_param_conversion_input <- function(dt, id_cols = c(), param_cols = c(),
                                            terminal_age = NA, assert_na = NA) {

  # check `id_cols` argument -------------------------------------------------

  if(length(id_cols) > 0) {

    # character
    assertive::assert_is_character(id_cols)

    # includes "age"
    assertthat::assert_that("age" %in% id_cols,
                            msg = "`id_cols` must include 'age'.")

    # shouldn't include other age variables
    if("age_start" %in% id_cols) stop("'age_start' cannot be in id_cols.")
    if("age_end" %in% id_cols) stop("'age_end' cannot be in id_cols.")
    if("age_length" %in% id_cols) stop("'age_length' cannot be in id_cols.")
    if("age_group" %in% id_cols) stop("'age_group' cannot be in id_cols.")
    if("age_group_id" %in% id_cols) stop("'age_group_id' cannot be in id_cols.")
    id_cols_no_age <- id_cols[id_cols != "age"]
    if(any(tolower(id_cols_no_age) %like% "age")) {
      warning("Confirm that no age vars other than 'age' are in 'id_cols'.")
    }
  }

  # check `dt` ---------------------------------------------------------------

  # data.table
  assertive::assert_is_data.table(dt)

  # unique
  if(length(id_cols) > 0) {
    assert_is_unique_dt(dt, id_cols = id_cols) # demUtils not working
  }

  # has correct columns
  assertable::assert_colnames(dt, c(param_cols, id_cols),
                              only_colnames = F, quiet = T)

  # age and parameter columns numeric
  if("age" %in% names(dt)) assertive::assert_is_numeric(dt[["age"]])
  for(param in param_cols) {
    assertive::assert_is_numeric(dt[[param]])
  }

  # all life table params > 0
  assertable::assert_values(dt, param_cols, test = "gte",
                            test_val = 0, quiet = T)

  # qx, lx, dx < 1
  params_lte_1 <- intersect(param_cols, c("qx", "lx", "dx"))
  if(length(params_lte_1) > 0) {
    assertable::assert_values(dt, params_lte_1, test = "lte",
                              test_val = 1, quiet = T)
  }

  # check `terminal_age` -----------------------------------------------------

  if(!is.na(terminal_age)) {
    # numeric
    assertive::assert_is_numeric(terminal_age)
    # all age values are less than terminal age
    assertable::assert_values(dt, c("age"), test = "lte",
                              test_val = terminal_age, quiet = T)
  }

  # check `assert_na` --------------------------------------------------------

  if(!is.na(assert_na)) {
    assertive::assert_is_logical(assert_na)
  }

}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
qx_to_lx <- function(dt, id_cols, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_param_conversion_input(dt = dt,
                                  id_cols = id_cols,
                                  param_cols = c("qx"),
                                  assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age"))

  # calculate lx -------------------------------------------------------------

  dt[, lx := 1]
  dt[, lx := lx[1] * cumprod(c(1, head(1 - qx, -1))), by = id_cols_no_age]

  # check and return ---------------------------------------------------------

  if (assert_na == T) assertable::assert_values(dt, "lx", "not_na", quiet = T)
  assertable::assert_values(dt, c("lx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("lx"), test = "lte", test_val = 1, quiet = T)

  setkeyv(dt, original_keys)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
lx_to_qx <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_param_conversion_input(dt = dt,
                                  id_cols = id_cols,
                                  param_cols = c("lx"),
                                  assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age"))

  # calculate qx -------------------------------------------------------------

  dt[, qx := 1 - (shift(lx, 1, type = "lead") / lx), by = id_cols_no_age]
  dt[age == terminal_age, qx := 1]

  # check and return ---------------------------------------------------------

  if (assert_na == T) assertable::assert_values(dt, "qx", "not_na", quiet = T)
  assertable::assert_values(dt, c("qx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("qx"), test = "lte", test_val = 1, quiet = T)

  setkeyv(dt, original_keys)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
lx_to_dx <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_param_conversion_input(dt = dt,
                                  id_cols = id_cols,
                                  param_cols = c("lx"),
                                  terminal_age = terminal_age,
                                  assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age"))

  # calculate dx -------------------------------------------------------------
  dt[, dx := lx - shift(lx, 1, type = "lead"), by = c(id_cols_no_age)]
  dt[age == terminal_age, dx := lx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) {
    assertable::assert_values(dt, "dx", "not_na", quiet = T)
  }
  setkeyv(dt, original_keys)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_nLx <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

  # prep -------------------------------------------------------------------

  # validate inputs
  validate_param_conversion_input(dt = dt,
                                  id_cols = id_cols,
                                  param_cols = c("lx", "ax", "dx", "mx"),
                                  terminal_age = terminal_age,
                                  assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age"))

  # calculate nLx -----------------------------------------------------------

  # add age_length -- TODO: switch to demUtils function
  dt[, age_length := shift(age, 1, type = "lead") - age, by = id_cols_no_age]
  dt[age == terminal_age, age_length := 100]

  # calculte nLx
  dt[, nLx := age_length * shift(lx, 1, type = "lead") + ax * dx,
       by = id_cols_no_age]
  dt[age == terminal_age, nLx := lx / mx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) {
    assertable::assert_values(dt[age != terminal_age], "nLx", "not_na",
                              quiet = T)
  }
  setkeyv(dt, original_keys)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_Tx <- function(dt, id_cols, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_param_conversion_input(dt = dt,
                                  id_cols = id_cols,
                                  param_cols = c("nLx"),
                                  assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age"))

  # calculate Tx -------------------------------------------------------------

  # Set descending order to begin the cumulative sum at the oldest age group
  setorderv(dt, id_cols, order = -1)
  dt[, Tx := cumsum(nLx), by = id_cols_no_age]
  setorderv(dt, id_cols) # undo reverse order

  # check outputs ------------------------------------------------------------
  if (assert_na == T) assertable::assert_values(dt, "Tx", "not_na", quiet = T)
  setkeyv(dt, original_keys)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_ex <- function(dt, assert_na = T) {

  # validate ----------------------------------------------------------------

  validate_param_conversion_input(dt = dt,
                                  param_cols = c("Tx", "lx"),
                                  assert_na = assert_na)

  # calculate ex -------------------------------------------------------------
  dt[, ex := Tx / lx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) assertable::assert_values(dt, "ex", "not_na", quiet = T)
  return(dt)
}
