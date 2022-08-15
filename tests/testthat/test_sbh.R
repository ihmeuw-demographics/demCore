

# Table 53 from Manual X
expected_dt <- data.table(
  sex = c(rep("all", 7), rep("female", 7), rep("male", 7)),
  age_start = 0,
  age_end = c(1, 2, 3, 5, 10, 15, 20),
  nqx = c(0.0766, 0.0514, 0.0652, 0.0718, 0.0967, 0.1088, 0.1309,
          0.0590, 0.0440, 0.0595, 0.0676, 0.0909, 0.0965, 0.1309,
          0.0952, 0.0580, 0.0707, 0.0757, 0.1021, 0.1201, 0.1308),
  tx = c(1.05, 2.37, 4.32, 6.64, 9.19, 11.92, 14.86,
         1.14, 2.41, 4.24, 6.41, 8.80, 11.41, 14.33,
         0.97, 2.33, 4.39, 6.86, 9.58, 12.43, 15.36)
)
setkeyv(expected_dt, c("sex", "age_start", "age_end"))

test_that("test that `calc_nqx_brass` gives expected output", {
  output_dt <- calc_nqx_brass(
    dt = sbh_panama_1976,
    id_cols = c("sex", "age_start", "age_end")
  )

  testthat::expect_equal(
    object = output_dt,
    expected = expected_dt,
    tolerance = 0.001
  )
})

input_dt <- data.table(
  age_start = 50,
  age_end = 55,
  n_women = 800,
  sex = c("male", "female", "all"),
  ceb = c(2500, 2500, 5000),
  ced = c(350, 350, 700)
)
input_dt[, Pi := ceb / n_women]
input_dt[, Di := ced / ceb]
input_dt <- rbind(input_dt, sbh_panama_1976)

test_that("test that `calc_nqx_brass` gives expected output when extra age groups are supplied", {
  output_dt <- calc_nqx_brass(
    dt = input_dt,
    id_cols = c("sex", "age_start", "age_end")
  )
  testthat::expect_equal(
    object = output_dt,
    expected = expected_dt,
    tolerance = 0.001
  )
})
