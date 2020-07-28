
id_cols <- c("id", "age_start", "age_end")

# create input lifetable in 5-year age-groups with all lifetable parameters
lt <- austria_1992_lt[, list(age_start, age_end, qx, ax)]
lt <- agg_lt(
  dt = lt,
  id_cols = setdiff(id_cols, "id"),
  age_mapping = data.table(
    age_start = seq(0, 85, 5),
    age_end = c(seq(5, 85, 5), Inf)
  )
)
lt <- rbind(
  lt[, list(id = 1, age_start, age_end, qx, ax)],
  lt[, list(id = 2, age_start, age_end, qx, ax)]

)

lifetable(lt, id_cols = id_cols)


calculate_nSx_then_inverse <- function(terminal_age) {

  # calculate survivorship ratio
  nSx <- nSx_from_lx_nLx_Tx(
    dt = lt,
    id_cols = id_cols,
    terminal_age = terminal_age
  )

  # back-calculate nLx
  output_lt <- copy(nSx)
  gen_nLx_from_nSx(
    dt = output_lt,
    id_cols = id_cols
  )
  output_lt[, nSx := NULL]

  # merge on ax
  output_lt <- merge(output_lt, lt[, .SD, .SDcols = c(id_cols, "ax")],
                     by = id_cols, all.x = TRUE)
  data.table::setkeyv(output_lt, NULL)

  # back-calculate lx
  gen_lx_from_nLx_ax(
    dt = output_lt,
    id_cols = id_cols
  )
  gen_qx_from_lx(
    dt = output_lt,
    id_cols = id_cols
  )

  # calculate all life table parameters
  lifetable(output_lt, id_cols = id_cols)
  data.table::setcolorder(output_lt, names(lt))

  testthat::expect_equal(lt, output_lt)
}

testthat::test_that("survivorship ratios are calculated correctly", {

  # terminal age is below maximum in the lifetable
  calculate_nSx_then_inverse(terminal_age = 80)

  # terminal age is equal to maximum in the lifetable
  calculate_nSx_then_inverse(terminal_age = 85)
})
