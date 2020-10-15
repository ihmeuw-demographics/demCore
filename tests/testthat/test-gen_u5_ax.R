library(data.table)


# gen_u5_ax ---------------------------------------------------------------


# test input
dt <- data.table::data.table(
  age_start = rep(c(0, 1, 5), 3),
  age_end = rep(c(1, 5, 10), 3),
  mx = c(0.09, 0.12, 0.08, 0.11, 0.14, 0.05, 0.4, 0.3, 0.07),
  sex = rep(c("male", "female", "both"), each = 3),
  location = rep("Canada", 9)
)
setorderv(dt, c("sex", "age_start"))

# expected output
expected <- copy(dt)
expected$ax <- c(0.34000, 1.35650, NA,
                 0.35000, 1.36100, NA,
                 0.28656, 1.39756, NA)

test_that("test that `gen_u5_ax` gives expected output", {
  test_dt <- copy(dt)
  gen_u5_ax(test_dt, id_cols = c("age_start", "age_end", "sex", "location"))
  setorderv(test_dt, c("sex", "age_start"))
  setcolorder(test_dt, c("age_start", "age_end", "mx", "sex", "location", "ax"))
  testthat::expect_equivalent(test_dt, expected, tolerance = 0.001)
})

test_that("test that `gen_u5_ax` gives errors when it should", {
  test_dt <- copy(dt)
  testthat::expect_error(gen_u5_ax(test_dt, id_cols = c("age_start", "age_end",
                                                   "sex", "year")))
  testthat::expect_error(gen_u5_ax(test_dt[age_start != 0],
                                   id_cols = c("age_start", "age_end", "sex")))
})

test_that("test that `gen_u5_ax` modifies in place", {
  test_dt <- copy(dt)
  mem1 <- pryr::address(test_dt) # memory address before
  gen_u5_ax(test_dt, id_cols = c("age_start", "age_end", "sex", "location"))
  mem2 <- pryr::address(test_dt) # memory address after
  testthat::expect_equal(mem1, mem2)
})


# gen_u5_ax_from_qx -------------------------------------------------------


# setup input
dt <- data.table::data.table(
  age_start = rep(c(0, 1), 4),
  age_end = rep(c(1, 5), 4),
  qx = c(0.0846, 0.3658, 0.11, 0.42, 0.06, 0.35, 0.082, 0.37),
  sex = rep(c("male", "male", "female", "female"), 2),
  location_id = c(rep(1, 4), rep(2, 4))
)
id_cols <- c("age_start", "age_end", "sex", "location_id")


testthat::test_that("`gen_u5_ax_from_qx` works", {

  # run function
  dt <- testthat::expect_silent(gen_u5_ax_from_qx(dt, id_cols))

  # invert function (go mx --> ax and see if you get the same thing)
  dt[, mx := qx_ax_to_mx(qx = qx, ax = ax, age_length = age_end - age_start)]
  setnames(dt, "ax", "ax_from_qx")
  dt <- gen_u5_ax(dt, id_cols)

  testthat::expect_equivalent(
    dt$ax, dt$ax_from_qx, tolerance = 0.001
  )

})



