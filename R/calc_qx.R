#' Calculate aggregated age qx values given granular age qx
#'
#' Given a data.table with a qx variable and ID variables that uniquely identify the data,
#' compile granular ages and aggregate into combined qx values.
#'
#' @param dt data.table with variables: qx, age_start, age_end, all id_cols
#' @param age_start numeric, year of age to start at (e.g. 0 for 5q0)
#' @param age_end numeric, year of age to end at (e.g. 5 for 5q0)
#' @param id_cols character vector of column names in dt that, along with age_start and age_end,
#'          uniquely identify all rows in dt
#'
#' @return data.table with id_cols and a variable called qx_#q#, where the first is
#'          age_end - age_start, and the second is age_start
#' @import data.table
#'
#' @examples
#' dt <- data.table::data.table(
#'   id = c(rep(1, 5), rep(2, 5)), qx = c(rep(.1, 5), rep(.2, 5)),
#'   age_start = rep(seq(15, 35, 5), 2),
#'   age_end = rep(seq(20, 40, 5), 2)
#' )
#' calc_qx(dt, age_start = 15, age_end = 40, id_cols = "id")
#'
#' @export

calc_qx <- function(dt, age_start, age_end, id_cols = c()) {

  ## Validations
  if (age_start > age_end) stop("Age start can't be larger than age end")
  if (age_start < min(dt$age_start)) stop("Age start is lower than lowest age start in dt")
  if (age_end > max(dt$age_end)) stop("Age end is higher than highest age end in dt")
  if (!is.data.table(dt)) stop("The input dataset must be in data.table format")
  if ("qx" %in% id_cols) stop("qx cannot be an id_var")
  if ("px" %in% id_cols) stop("px cannot be an id_var")
  if (!"qx" %in% names(dt)) stop("qx needs to be present in the dataset")

  ## Prep
  dt <- dt[, .SD, .SDcols = c(id_cols, "qx", "age_start", "age_end")]
  setnames(dt, "age_start", "age_group_years_start")
  setnames(dt, "age_end", "age_group_years_end")

  ## Calculate age length
  dt[, age_length := age_group_years_end - age_group_years_start]

  ## Subset to ages of interest
  dt <- dt[age_group_years_start >= age_start & age_group_years_end <= age_end]

  ## Check that you have all consecutive age groups
  setorderv(dt, c(id_cols, "age_group_years_start"))
  dt[, test := ifelse(age_group_years_start + age_length ==
    shift(age_group_years_start, type = "lead"), 0, 1), by = id_cols]
  dt[age_group_years_end == age_end, test := 0]
  if (nrow(dt[test == 1]) > 0) stop("Need a full set of consecutive age groups")
  if (nrow(dt[age_group_years_start == age_start]) == 0) stop("Data should include age_start")
  if (nrow(dt[age_group_years_end == age_end]) == 0) stop("Data should include age_end")

  ## Calculate survival probability (px)
  dt[, px := 1 - qx]
  dt[, qx := NULL]

  ## Collapse on the product of the transformed qx values
  nrow_master <- nrow(dt)
  n_ages <- length(unique(dt$age_group_years_start))
  dt <- dt[, lapply(.SD, prod), .SDcols = "px", by = id_cols]

  ## Generate new qx based on the product of the collapsed px values
  dt[, paste0("qx_", age_end - age_start, "q", age_start) := 1 - px]
  dt[, px := NULL]

  ## Check that we have lost the correct number of rows
  ## Example: if there are 5 age groups within, the resulting dataset should be 1/5th the size of the original
  nrow_collapse <- nrow(dt)
  if (nrow_collapse == nrow_master) stop("You have specified an id variable that is unique within age -- re-check your variables")
  if (nrow_collapse * n_ages != nrow_master) stop("The input dataset is not square (e.g. not all required age groups are present for all combinations of id_cols)")

  return(dt)
}
