#' @title Calculate Mean Age of Childbearing from Fertility Rate (MACB)
#'
#' @description
#' The mean age at childbearing is the mean age of mothers at the birth of their
#' children if women were subject throughout their lives to the age-specific
#' fertility rates observed in a given year.
#'
#' See [UNPOP page][1] for more deatils.
#'
#' [1]: https://www.un.org/en/development/desa/population/publications/dataset/fertility/age-childbearing.asp
#'
#' @param dt \[`data.table()`\]\cr A data.table with columns 'age_start',
#'   'age_end', and a column of fertility rates named after `nfx_col`.
#' @param nfx_col \[`character(1)`\]\cr Name of fertility rate column in `dt`.
#'   Defualts to "nfx".
#' @inheritParams gen_lifetable_parameters
#' @param value_col \[`character(1)`\]\cr Name of output 'MACB' column. Defaults
#'   to "mcab"
#'
#' @return \[`data.table()`\] A data.table with calculated 'MACB' for each
#'   unique grouping in `id_cols`, stored in `value_col`.
#'
#' @export
#'
#' @examples
#' asfr_data <- data.table(
#'   asfr = c(.051, .196, .208, .147, .075, .024, .004),
#'   age_start = seq(15, 45, 5),
#'   age_end = seq(20, 50, 5)
#'  )
#'
#' id_cols <- c("age_start", "age_end")
#' macb_dt <- macb_from_nfx(asfr_data, id_cols = id_cols, nfx_col = "asfr")
#'
#'
macb_from_nfx <- function(dt, id_cols, nfx_col = "nfx", value_col = "macb") {


  # Validate parameters -----------------------------------------------------

  assertthat::assert_that(
    assertthat::is.string(nfx_col),
    assertthat::is.string(value_col)
  )


  # Prep id columns ---------------------------------------------------------

  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]


  # Calculate MACB ----------------------------------------------------------

  macb_dt <- dt[
    ,
    .(macb = weighted.mean((age_end + age_start) / 2, get(nfx_col))),
    by = id_cols_no_age
  ]

  setnames(macb_dt, "macb", value_col)

  return(macb_dt)

}
