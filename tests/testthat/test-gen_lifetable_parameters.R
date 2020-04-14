library(data.table)

# ============================================================================
# gen_lx_from_qx

# set up standard input data.table
input_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age_start = rep(c(0, 1, seq(5, 20, 5)), 2),
  age_end = rep(c(1, seq(5, 20, 5), Inf), 2),
  qx = c(rep(.1, 5), 1, rep(.2, 5), 1)
)
id_cols <- c("sex", "age_start", "age_end")

# set up expected output table
expected_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age_start = rep(c(0, 1, seq(5, 20, 5)), 2),
  age_end = rep(c(1, seq(5, 20, 5), Inf), 2),
  qx = c(rep(.1, 5), 1, rep(.2, 5), 1),
  lx = c(1, 0.9, 0.81, 0.729, 0.6561, 0.59049,
         1, 0.8, 0.64, 0.512, 0.40960, 0.32768)
)

test_that("test that `gen_lx_from_qx` basic functionality works", {
  output_dt <- copy(input_dt)
  gen_lx_from_qx(output_dt, id_cols)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("test that `gen_lx_from_qx` errors are thrown for different cases", {
  # Check error thrown when wrong argument types are given
  testthat::expect_error(gen_lx_from_qx(input_dt, "hello", T))
  testthat::expect_error(gen_lx_from_qx(input_dt, T, id_cols))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(gen_lx_from_qx(non_unique_input_dt, id_cols = id_cols))
})

test_that("test that resulting lx is monotonic by age", {
  output_dt <- copy(input_dt)
  gen_lx_from_qx(output_dt, id_cols)
  output_females <- output_dt[sex == "female"]$lx
  testthat::expect_equal(T, all(output_females == cummin(output_females)))
})

# ============================================================================
# gen_dx_from_lx

# set up standard input data.table
input_dt <- data.table(
  sex = rep("both", 4),
  age_start = c(0, 5, 10, 15),
  age_end = c(5, 10, 15, Inf),
  lx = c(1, 0.9, 0.7, 0.2)
)
id_cols <- c("sex", "age_start", "age_end")

# set up expected output table
expected_dx <- c(0.1, 0.2, 0.5, 0.2)
expected_dt <- copy(input_dt)
expected_dt$dx <- expected_dx

test_that("test that `gen_dx_from_lx` basic functionality works", {
  output_dt <- copy(input_dt)
  gen_dx_from_lx(output_dt, id_cols)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("test that `gen_dx_from_lx` errors are thrown for different cases", {
  # check error thrown when wrong argument types are given
  testthat::expect_error(gen_dx_from_lx(as.data.frame(input_dt), id_cols, T))
  testthat::expect_error(gen_dx_from_lx(input_dt, "hello", T))
  testthat::expect_error(gen_dx_from_lx(input_dt, id_cols, 15))

  # check error thrown when missing cols
  testthat::expect_error(gen_dx_from_lx(input_dt[, c("sex", "lx")], id_cols, T))
  testthat::expect_error(gen_dx_from_lx(input_dt[, c("sex", "age_start", "age_end")],
                                  id_cols, T))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(gen_dx_from_lx(non_unique_input_dt, id_cols, T))
})


# ============================================================================
# gen_qx_from_lx

# set up standard input data.table
input_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age_start = rep(c(0, 1, seq(5, 20, 5)), 2),
  age_end = rep(c(1, seq(5, 20, 5), Inf), 2),
  lx = c(1, 0.9, 0.81, 0.729, 0.6561, 0.59049,
         1, 0.8, 0.64, 0.512, 0.40960, 0.32768)
)
id_cols <- c("sex", "age_start", "age_end")

# set up expected output table
expected_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age_start = rep(c(0, 1, seq(5, 20, 5)), 2),
  age_end = rep(c(1, seq(5, 20, 5), Inf), 2),
  lx = c(1, 0.9, 0.81, 0.729, 0.6561, 0.59049,
         1, 0.8, 0.64, 0.512, 0.40960, 0.32768),
  qx = c(rep(.1, 5), 1, rep(.2, 5), 1)
)

test_that("test that `gen_qx_from_lx` basic functionality works", {
  output_dt <- copy(input_dt)
  gen_qx_from_lx(output_dt, id_cols)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("test that `gen_qx_from_lx` errors are thrown for duplicate data", {
  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(gen_qx_from_lx(non_unique_input_dt, id_cols, T))
})


# ============================================================================
# gen_nLx

# set up standard input data.table
dt <- data.table::data.table(
  sex = rep("both", 4),
  age_start = c(0, 5, 10, 15),
  age_end = c(5, 10, 15, Inf),
  age_length = c(5, 5, 5, Inf),
  mx = c(0.1, 0.2, 0.3, 0.4),
  ax = c(2.5, 2.5, 2.5, 2.5)
)
dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
gen_lx_from_qx(dt, id_cols = c("sex", "age_start", "age_end"))
gen_dx_from_lx(dt, id_cols = c("sex", "age_start", "age_end"))
id_cols <- c("sex", "age_start", "age_end")

# set up expected output (rounded)
expected_nLx <- c(4.00, 2.00, 0.57, 0.07)

test_that("test that `gen_nLx` basic functionality works", {
  output_dt <- copy(dt)
  gen_nLx(output_dt, id_cols)
  output_nLx <- round(output_dt$nLx, 2)
  testthat::expect_equal(output_nLx, expected_nLx)
})

test_that("test that `gen_nLx` errors are thrown for different cases", {
  # check error thrown when wrong argument types are given
  testthat::expect_error(gen_nLx(dt, "hello"))
  testthat::expect_error(gen_nLx("hello", id_cols))
  testthat::expect_error(gen_nLx(dt, id_cols, "hello"))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(dt, dt)
  testthat::expect_error(gen_nLx(non_unique_input_dt, id_cols = id_cols))
})


# ============================================================================
# gen_Tx

# set up standard input data.table
dt <- data.table(
  sex = rep("male", 3),
  age_start = c(70, 75, 80),
  age_end = c(75, 80, Inf),
  nLx = c(0.6, 0.5, 0.4)
)

# set up expected output table
expected_dt <- data.table(
  sex = rep("male", 3),
  age_start = c(70, 75, 80),
  age_end = c(75, 80, Inf),
  nLx = c(0.6, 0.5, 0.4),
  Tx = c(1.5, 0.9, 0.4)
)

test_that("test that `gen_Tx` basic functionality works", {
  gen_Tx(dt, id_cols = c("age_start", "age_end", "sex"))
  testthat::expect_equal(dt, expected_dt)
})

test_that("test that `gen_Tx` errors are thrown for different cases", {
  # not data.table
  testthat::expect_error(gen_Tx(as.data.frame(dt)))
  # missing nLx
  testthat::expect_error(gen_Tx(dt[, .(age_start, age_end, sex)]))
})


# ============================================================================
# gen_ex

# set up standard input data.table
dt <- data.table(
  age_start = c(70, 75, 80),
  age_end = c(75, 80, Inf),
  lx = c(0.6, 0.5, 0.4),
  Tx = c(30, 20, 10)
)

# set up expected output table
expected_dt <- data.table(
  age_start = c(70, 75, 80),
  age_end = c(75, 80, Inf),
  lx = c(0.6, 0.5, 0.4),
  Tx = c(30, 20, 10),
  ex = c(50, 40, 25)
)

test_that("test that `gen_ex` basic functionality works", {
  gen_ex(dt) # modifies dt in place
  testthat::expect_equal(dt, expected_dt)
})

test_that("test that `gen_ex` errors are thrown for different cases", {
  # not data.table
  testthat::expect_error(gen_ex(as.data.frame(dt)))
  # missing lx or Tx
  testthat::expect_error(gen_ex(dt[, .(age_start, age_end, Tx)]))
  testthat::expect_error(gen_ex(dt[, .(age_start, age_end, lx)]))
})

