#' @title Ax iteration method
#'
#' @description Implement iteration method to minimize the difference across
#'   initialized ax and its expected relationship with dx. This method can be
#'   used to start with naive values of ax (like age_length/2) and arrive
#'   at more informed ax values based on an approximation that assumes a
#'   relationship between ax and the age pattern of deaths (dx) in a cohort.
#'   Since dx is often calculated using the mx --> ax --> qx method in the
#'   first place, initialization and iteration is required.
#'
#' @param dt \[`data.table()`\]\cr Life tables with columns: all `id_cols`,
#'   'age_start', 'age_end', 'dx', 'ax', 'qx', 'mx'.
#' @param n_iterations \[`integer(1)`\]\cr Maximum number of iterations to run.
#' @param threshold \[`numeric(1)`\]\cr How far apart should ax and the
#'   implied ax from dx be before removal from iteration. Default: 0.01.
#' @param quiet \[`logical(1)`\]\cr Whether to omit messages about iteration
#'   progress. Default is F and progress messages are given.
#' @inheritParams gen_lx_from_qx
#'
#' @return \[`data.table()`\]\cr Input life tables with ax, dx, qx modified.
#'   Input mx is unchanged.
#'
#' @details
#' Limitations: this method relies on initialized ax and dx values, and cannot
#' produce estimates for the first and last age groups. Preston pg 45.
#'
#' Note: this is not the best method for ages under 5 years. See
#' [gen_u5_ax()].
#'
#' @seealso Preston Demography book pg 45. Also, see more details on this
#'    method explained here:
#'   [Life table introduction vignette](https://ihmeuw.github.io/lifetableUtils/articles/introduction_to_life_tables.html)
#'
#' @references
#' Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling
#'   population processes. MA: Blackwell Publishing. 2001.
#'
#' @examples
#' data("austria_1992_lt")
#' dt <- austria_1992_lt[,.(age_start, age_end, age_length, mx, ax, qx, dx, lx)]
#' new_dt <- iterate_ax(dt, id_cols = c("age_start", "age_end"))
#'
#' \dontrun{
#'   # plot change in qx, ax, lx, dx, mx
#'   dt <- merge(dt, new_dt, by = c("age_start", "age_end"))
#'   plot(data = dt, qx.y/qx.x ~ age_start)
#'   plot(data = dt, ax.y/ax.x ~ age_start)
#'   plot(data = dt, lx.y/lx.x ~ age_start)
#'   plot(data = dt, dx.y/dx.x ~ age_start)
#'   plot(data = dt, mx.y/mx.x ~ age_start) # expect no change in mx
#' }
#'
#' @export
iterate_ax <- function(dt, id_cols, n_iterations = 30L,
                       threshold = 0.01, quiet = F) {

  # validate ----------------------------------------------------------------

  # standard validation
  validate_lifetable(dt, id_cols, param_cols = c("ax", "qx", "dx"))

  # check `n_iterations`
  assertthat::assert_that(is.integer(n_iterations) &
                            length(n_iterations) == 1,
                          msg = "`n_iterations' must be of class integer
                                  and length 1.")

  # prep --------------------------------------------------------------------

  dt <- copy(dt)
  setkeyv(dt, id_cols)
  id_cols_no_age <- setdiff(id_cols, c("age_start", "age_end"))
  if(!"age_length" %in% names(dt)) demUtils::gen_length(dt, "age")

  # initialize
  iter_num <- 1
  holdouts <- data.table()

  # iterate -----------------------------------------------------------------

  while(nrow(dt) > 0 & iter_num < n_iterations) {

    if(!quiet) message("Iteration ", iter_num)
    setorderv(dt, id_cols)

    # estimate ax from dx (Preston pg 45)
    dt[, new_ax := ((-5/24) * shift(dx, 1, type = "lag")
                    + 2.5 * dx
                    + (5/24) * shift(dx, 1, type = "lead"))
       / dx]

    # reset ax if it goes out of bounds
    dt[new_ax <= 0, new_ax := 0.01]
    dt[new_ax >= age_length, new_ax := age_length - 0.01]

    # for ages without previous or next age, use initialized ax
    dt[age_start == min(age_start) | age_start == max(age_start), new_ax := ax]

    # calculate maximum difference between ax and new ax
    dt[, diff := abs(new_ax - ax)]
    dt[, max_ax_diff := max(diff), by = id_cols_no_age]

    # replace ax with new_ax
    dt[, ax := new_ax]
    dt[, new_ax := NULL]

    # calculate qx: cap at 1 and recalculate ax in case qx adjusted
    dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
    dt[qx > 1, qx := 0.99]
    dt[, ax := mx_qx_to_ax(mx, qx, age_length)]

    # calculate lx and dx
    gen_lx_from_qx(dt, id_cols)
    dt[, dx := lx * qx]

    # remove life tables where diff is satisfactorily small
    holdouts <- rbindlist(list(holdouts, dt[max_ax_diff <= threshold]),
                          use.names = T, fill = T)
    dt <- dt[max_ax_diff > threshold]

    n_remaining <- ifelse(nrow(dt) == 0, 0,
                          round(nrow(dt) / length(unique(dt$age_start)), 0))
    if(!quiet) {
      message("Number of remaining life tables: ", n_remaining)
      if(n_remaining > 0) {
        message(paste0("Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. \n ",
                paste(round(summary(dt$max_ax_diff), 2), collapse = " | ")))
      }
    }

    iter_num <- iter_num + 1

  }

  # clean up and return -----------------------------------------------------

  if(nrow(dt) > 0) {
    warning(paste0(n_iterations, " iterations reached but ",
                   n_remaining, " life tables remain unconverged."))
  }

  dt <- rbindlist(list(dt, holdouts), use.names = T, fill = T)

  dt[, c("diff", "max_ax_diff") := NULL]

  if(!quiet) message("Iterations done")
  return(dt)

}