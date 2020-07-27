#' @title Calculate survivorship ratios
#'
#' @description Calculate survivorship ratios to be used in projecting
#'   populations forward in time with [`ccmpp()`]. The survivorship ratios
#'   represent the proportion of people in one age group that will survive into
#'   the next age group given the age-specific mortality rates in a life table.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data that includes columns for `id_cols`, 'lx', 'nLx' and 'Tx'.
#' @inheritParams gen_lifetable_parameters
#' @param terminal_age \[`integer(1)`\]\cr
#'   The terminal age group in the population that the calculated survivorship
#'   ratios will be used to project in. This must be less than or equal to the
#'   maximum 'age_start' in `dt`.
#'
#' @details
#' See the [references page](https://ihmeuw-demographics.github.io/demCore/index.html)
#' for the formatted equations below.
#'
#' First age group:
#'   \deqn{{}_{n}S_0 = \frac{{}_{n}L_{0}}{n \cdot l_0}}
#'
#' Other age groups:
#'   \deqn{{}_{n}S_x = \frac{{}_{n}L_{x}}{{}_{n}L_{x-n}}}
#'
#' Terminal age group:
#'   \deqn{{}_{n}S_x = \frac{T_{x}}{T_{x-n}}}
#'
#'
#' @return \[`data.table()`\] with the `id_cols` plus a column for 'nSx' with the
#'   survivorship ratio value.
#'
#' @family survivorship_ratio
#' @seealso [`ccmpp()`]
#'
#' @examples
#' id_cols <- c("sex", "age_start", "age_end")
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age_start = c(0, 5, 10, 15),
#'   age_end = c(5, 10, 15, Inf),
#'   mx = c(0.1, 0.2, 0.3, 0.4),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' lifetable(dt, id_cols)
#' nSx <- nSx_from_lx_nLx_Tx(dt, id_cols, terminal_age = 15)
#'
#' @export
nSx_from_lx_nLx_Tx <- function(dt, id_cols, terminal_age) {

  # Validate arguments ------------------------------------------------------

  dt <- copy(dt)
  validate_lifetable(
    dt = dt,
    id_cols = id_cols,
    param_cols = c("lx", "nLx", "Tx"),
    assert_uniform_age_length = TRUE,
    assert_uniform_terminal_age = TRUE,
    assert_age_start_0 = TRUE,
  )

  # check `terminal_age` argument
  assertthat::assert_that(
    assertthat::is.count(terminal_age),
    terminal_age <= max(dt$age_start),
    msg = paste("`terminal_age` must be an integer lower than or equal to the",
                "maximum 'age_start' in `dt`")
  )

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # determine `age_int` for all combinations of `id_cols_no_age`
  age_int <- determine_age_int(dt, id_cols_no_age)
  assertthat::assert_that(
    assertthat::is.number(age_int),
    msg = "identified age interval in input `dt` must be identical across `id_cols`"
  )

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # Calculate survivorship ratios -------------------------------------------

  # calculate the survivorship ratio in only the youngest age group
  youngest <- dt[age_start == 0,
                 list(age_start = 0,
                      age_end = age_int,
                      nSx = nLx[1] / (age_int * lx[1])),
                 by = id_cols_no_age]

  normal <- dt[age_start < terminal_age,
               list(age_start = age_start[2:.N],
                    age_end = age_end[2:.N],
                    nSx = (shift(nLx, type = "lead") / nLx)[-.N]),
               by = id_cols_no_age]

  if (max(dt$age_start) == terminal_age) {
    terminal <- dt[age_start >= terminal_age - age_int,
                   list(age_start = terminal_age,
                        age_end = Inf,
                        nSx = Tx[2] / Tx[1]),
                   by = id_cols_no_age]
  } else {
    terminal <- dt[age_start >= terminal_age - age_int,
                   list(age_start = c(terminal_age, terminal_age + age_int),
                        age_end = c(terminal_age + age_int, Inf),
                        nSx = c((nLx[2] / nLx[1]), Tx[3] / Tx[2])),
                   by = id_cols_no_age]
  }

  nSx_dt <- rbind(youngest, normal, terminal, use.names = T)
  data.table::setkeyv(nSx_dt, original_keys)
  return(nSx_dt)
}

#' @title Generate nLx life table parameter from survivorship ratios
#'
#' @description Generate the nLx life table parameter given survivorship ratios
#'   (nSx).
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data that includes columns for `id_cols` and 'nSx'.
#' @inheritParams gen_lifetable_parameters
#'
#' @inherit gen_lifetable_parameters return
#'
#' @details
#' See the [references page](https://ihmeuw-demographics.github.io/demCore/index.html)
#' for the formatted equations below.
#'
#' First age group:
#'   \deqn{{}_{n}S_0 = \frac{{}_{n}L_{0}}{n \cdot l_0}}
#'   \deqn{{}_{n}L_{0} = {}_{n}S_0 \cdot n \cdot l_0}
#'
#' Other age groups:
#'   \deqn{{}_{n}S_x = \frac{{}_{n}L_{x}}{{}_{n}L_{x-n}}}
#'   \deqn{{}_{n}L_{x} = {}_{n}S_x \cdot {{}_{n}L_{x-n}}}
#'
#' Terminal age group:
#'   \deqn{{}_{n}S_x = \frac{T_{x}}{T_{x-n}} =
#'     \frac{{}_{\infty}L_{x}}{{}_{\infty}L_{x} + {}_{n}L_{x - n}}}
#'   \deqn{{}_{\infty}L_{x} = \frac{{}_{n}S_x \cdot {}_{n}L_{x - n}}{1 - {}_{n}S_x}}
#'
#' @examples
#' id_cols <- c("sex", "age_start", "age_end")
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age_start = c(0, 5, 10, 15),
#'   age_end = c(5, 10, 15, Inf),
#'   nSx = c(0.95, 0.99, 0.99, 0.75)
#' )
#' gen_nLx_from_nSx(dt, id_cols)
#'
#' @family survivorship_ratio
#'
#' @export
gen_nLx_from_nSx <-function(dt, id_cols) {

  # Validate arguments ------------------------------------------------------

  validate_lifetable(
    dt = dt,
    id_cols = id_cols,
    param_cols = c("nSx"),
    assert_uniform_age_length = TRUE,
    assert_uniform_terminal_age = TRUE,
    assert_age_start_0 = TRUE,
  )
  terminal_age <- dt[age_end == "Inf", unique(age_start)]

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # determine `age_int` for all combinations of `id_cols_no_age`
  age_int <- determine_age_int(dt, id_cols_no_age)
  assertthat::assert_that(
    assertthat::is.number(age_int),
    msg = "identified age interval in input `dt` must be identical across `id_cols`"
  )

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # Back-calculate nLx ------------------------------------------------------

  # calculate nLx for the first age group
  l0 <- 1
  dt[age_start == 0, nLx := nSx * age_int * l0]

  # calculate nLx for all other age groups except the terminal age group
  dt[, nLx := c(nLx[1], (nLx[1] * cumprod(nSx[-1]))), by = id_cols_no_age]

  # calculate nLx for the terminal age group
  dt[age_start >= terminal_age - age_int,
     nLx := c(nLx[1], (nSx[2] * nLx[1]) / (1 - nSx[2])),
     by = id_cols_no_age]

  # check and return ---------------------------------------------------------

  assertable::assert_values(dt, "nLx", "not_na", quiet = T)
  assertable::assert_values(dt, "nLx", test = "gte", test_val = 0, quiet = T)

  setkeyv(dt, original_keys)

  return(invisible(dt))
}

#' @title Generate the lx life table parameter given nLx and ax.
#'
#' @description Generate the lx life table parameter given nLx and ax.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data that includes columns for `id_cols`, 'nLx', and 'ax'.
#' @inheritParams gen_lifetable_parameters
#'
#' @inherit gen_lifetable_parameters return
#'
#' @details
#' See the [references page](https://ihmeuw-demographics.github.io/demCore/index.html)
#' for the formatted equations below.
#'
#' Terminal age group:
#'   \deqn{\begin{align} {}_{\infty}L_{x} &= n \cdot l_{x+n} + {}_{\infty}a_{x}
#'     \cdot {}_{\infty}d_{x} \\ &= {}_{\infty}a_{x} \cdot {}_{\infty}d_{x}
#'     \\ &= {}_{\infty}a_{x} \cdot l_{x} \end{align}}
#'   \deqn{l_{x} = \frac{{}_{\infty}L_{x}}{{}_{\infty}a_{x}}}
#'
#' Other age groups:
#'   \deqn{\begin{align} {}_{n}L_{x} &= n \cdot l_{x+n} + {}_{n}a_{x} \cdot
#'     {}_{n}d_{x} \\ &= n \cdot l_{x+n} + {}_{n}a_{x} \cdot (l_x - l_{x+n})
#'     \end{align}}
#'   \deqn{l_x = \frac{{}_{n}L_{x} - ((n - {}_{n}a_{x}) \cdot
#'     l_{x+n})}{{}_{n}a_{x}}}
#'
#' @examples
#' id_cols <- c("sex", "age_start", "age_end")
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age_start = c(0, 5, 10, 15),
#'   age_end = c(5, 10, 15, Inf),
#'   nLx = c(4, 2, 0.6, 0.1),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' gen_lx_from_nLx_ax(dt, id_cols)
#'
#' @family survivorship_ratio
#'
#' @export
gen_lx_from_nLx_ax <- function(dt, id_cols) {

  # Validate arguments ------------------------------------------------------

  validate_lifetable(
    dt = dt,
    id_cols = id_cols,
    param_cols = c("nLx", "ax"),
    assert_uniform_age_length = TRUE,
    assert_uniform_terminal_age = TRUE,
    assert_age_start_0 = TRUE,
  )

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # determine `age_int` for all combinations of `id_cols_no_age`
  age_int <- determine_age_int(dt, id_cols_no_age)
  assertthat::assert_that(
    assertthat::is.number(age_int),
    msg = "identified age interval in input `dt` must be identical across `id_cols`"
  )

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # Back-calculate lx -------------------------------------------------------

  # calculate for all other age groups starting from the oldest ages
  setorderv(dt, "age_start", order = -1)
  dt[, lx := unlist(purrr::accumulate2(
    .x = nLx,
    .y = ax,
    .f = lx_from_lxpn_nLx_ax,
    age_int = age_int,
    .init = 0 # no one in the population at the end of the terminal age group
  ))[-1], by = id_cols_no_age]
  setorderv(dt, "age_start", order = 1)

  l0 <- 1
  dt[age_start == 0, lx := l0]

  # check and return ---------------------------------------------------------

  assertable::assert_values(dt, "lx", "not_na", quiet = T)
  assertable::assert_values(dt, "lx", test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, "lx", test = "lte", test_val = 1, quiet = T)

  setkeyv(dt, original_keys)

  return(invisible(dt))
}

#' @title Helper function to back calculate lx for one age group
#'
#' @description Helper function to back calculate lx from the lx value of the
#'   next oldest age group (lxpn which is short for \eqn{l_{x+n}}), and the nLx
#'   and ax values at age x.
#'
#' @param lxpn \[`numeric(1)`\]\cr
#' @param nLx \[`numeric(1)`\]\cr
#' @param ax \[`numeric(1)`\]\cr
#' @param age_int \[`integer(1)`\]\cr
#'
#' @return \[`numeric(1)`\]\cr
lx_from_lxpn_nLx_ax <- function(lxpn, nLx, ax, age_int) {
  return((nLx - ((age_int - ax) * lxpn)) / ax)
}

#' @title Helper function to identify unique age intervals for input data.table
#'
#' @inheritParams nSx_from_lx_nLx_Tx
#' @param id_cols_no_age \[`character()`\]\cr
#'   Columns that uniquely identify each unique lifetable in `dt`. Must not
#'   include 'age_start' and 'age_end'.
#'
#' @return \[`numeric(1)`\] age interval
determine_age_int <- function(dt, id_cols_no_age) {
  # determine `age_int` for all combinations of `id_cols_no_age`
  age_int <- dt[age_end != Inf, list(int = unique(diff(age_start))),
                by = id_cols_no_age]
  age_int <- unique(age_int$int)
  return(age_int)
}
