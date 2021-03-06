
input_dt <- data.table::data.table(
  sex = "male",
  age_start = c(0, 1, 2, 3, 4, 1, 0),
  age_end = c(1, 2, 3, 4, 5, 5, 5),
  qx = c(0.03, 0.015, 0.005, 0.004, 0.001, 0.03, 0.05)
)

expected_dt <- data.table::data.table(
  sex = "male",
  age_start = c(0, 1, 2, 3, 4, 1, 0),
  age_end = c(1, 2, 3, 4, 5, 5, 5),
  qx = c(0.025320566, 0.015126059, 0.005127338, 0.004127466, 0.001127850,
         0.025320566, 0.05)
)

test_that("test that `scale_qx` gives expected output", {
  output_dt <- scale_qx(input_dt, id_cols = c("sex", "age_start", "age_end"))
  setorderv(output_dt, cols = c("age_end", "age_start"))
  setorderv(expected_dt, cols = c("age_end", "age_start"))
  testthat::expect_equivalent(output_dt, expected_dt, tolerance = 0.00001)
})

test_that("test that `scale_qx` works with non-integer age vals", {

  # create test intput
  input_dt2 <- copy(input_dt)
  input_dt2[, age_start := age_start / 10]
  input_dt2[, age_end := age_end / 10]

  # run function for both age sets
  dt <- scale_qx(input_dt2, id_cols = c("sex", "age_start", "age_end"))
  compare <- scale_qx(input_dt, id_cols = c("sex", "age_start", "age_end"))

  # check output qx is the same for integer and non-integer
  testthat::expect_equal(dt$qx, compare$qx)

})


