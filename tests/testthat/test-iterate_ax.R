library(data.table)

# test input
data("austria_1992_lt")
dt <- austria_1992_lt[,.(age_start, age_end, age_length, mx, ax, qx, dx, lx)]

# run function
new_dt <- iterate_ax(dt, c("age_start", "age_end"))

# combine input (x) and output (y)
dt <- merge(dt, new_dt, by = c("age_start", "age_end"))

test_that("test that `iterate_ax` gives expected output", {
  testthat::expect_equal(names(new_dt), c("age_start", "age_end", "age_length",
                                          "mx", "ax", "qx", "dx", "lx"))
  validate_lifetable(new_dt, id_cols = c("age_start", "age_end"),
                     param_cols = c("mx", "ax", "qx", "dx", "lx"))
})

test_that("test that `iterate_ax` doesn't change mx", {
  testthat::expect_equivalent(dt$mx.x, dt$mx.y)
})

test_that("test that `iterate_ax` doesn't change ax, qx, dx, lx too much", {

  # subset to over-5 because under-5 should be using `gen_u5_ax`.
  over5 <- dt[age_start >= 5]

  testthat::expect_true(all(abs(1 - over5$ax.x / over5$ax.y) < 0.1))
  testthat::expect_true(all(abs(1 - over5$qx.x / over5$qx.y) < 0.05))
  testthat::expect_true(all(abs(1 - over5$lx.x / over5$lx.y) < 0.05))
  testthat::expect_true(all(abs(1 - over5$dx.x / over5$dx.x) < 0.05))
})

test_that("test that `iterate_ax` throws error if duplicates exist", {
  data("austria_1992_lt")
  dt <- austria_1992_lt[,.(age_start, age_end, age_length, mx, ax, qx, dx, lx)]
  dt <- rbind(dt, dt)
  testthat::expect_error(iterate_ax(dt, c("age_start", "age_end")))
})
