testthat::test_that("check `matrix_dt_formatting functions work", {

  # check with age and sex-specific data
  output_matrix <- dt_to_matrix(thailand_initial_estimates$survival)
  output_dt <- matrix_to_dt(output_matrix, year_right_most_endpoint = 2000)
  data.table::setindexv(thailand_initial_estimates$survival, NULL)
  testthat::expect_equal(thailand_initial_estimates$survival, output_dt)

  # check with age and sex-specific data with only the terminal age group
  input_dt <- thailand_initial_estimates$survival[is.infinite(age_end)]
  output_matrix <- dt_to_matrix(input_dt)
  output_dt <- matrix_to_dt(output_matrix, year_right_most_endpoint = 2000)
  data.table::setindexv(input_dt, NULL)
  testthat::expect_equal(input_dt, output_dt)

  # check with non age or sex-specific data
  output_matrix <- dt_to_matrix(thailand_initial_estimates$srb,
                                id_cols = c("year_start", "year_end"))
  output_dt <- matrix_to_dt(output_matrix, year_right_most_endpoint = 2000)
  data.table::setindexv(thailand_initial_estimates$srb, NULL)
  testthat::expect_equal(thailand_initial_estimates$srb, output_dt)

  # check with non sex-specific data
  output_matrix <- dt_to_matrix(thailand_initial_estimates$asfr,
                                id_cols = c("year_start", "year_end",
                                            "age_start", "age_end"))
  output_dt <- matrix_to_dt(output_matrix, year_right_most_endpoint = 2000,
                            age_right_most_endpoint = 50)
  data.table::setindexv(thailand_initial_estimates$asfr, NULL)
  testthat::expect_equal(thailand_initial_estimates$asfr, output_dt)

  # check with non age or sex-specific data
  output_matrix <- dt_to_matrix(thailand_initial_estimates$srb,
                                id_cols = c("year_start", "year_end"))
  rownames(output_matrix) <- NULL
  colnames(output_matrix) <- NULL
  testthat::expect_error(
    matrix_to_dt(output_matrix, year_right_most_endpoint = 2000),
    regexp = "rownames"
  )

})
