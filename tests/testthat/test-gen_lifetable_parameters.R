library(data.table)

# ============================================================================
# qx_to_lx

# set up standard input data.table
input_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age = rep(c(0, 1, seq(5, 20, 5)), 2),
  qx = c(rep(.1, 6), rep(.2, 6))
)
id_cols <- c("sex", "age")

# set up expected output table
expected_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age = rep(c(0, 1, seq(5, 20, 5)), 2),
  qx = c(rep(.1, 6), rep(.2, 6)),
  lx = c(1, 0.9, 0.81, 0.729, 0.6561, 0.59049,
         1, 0.8, 0.64, 0.512, 0.40960, 0.32768)
)

test_that("check `qx_to_lx()` basic functionality works", {
  output_dt <- qx_to_lx(input_dt, id_cols)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("check `qx_to_lx()` errors are thrown for different cases", {
  # Check error thrown when wrong argument types are given
  testthat::expect_error(qx_to_lx(input_dt, "hello", T))
  testthat::expect_error(qx_to_lx(input_dt, T, id_cols))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(qx_to_lx(non_unique_input_dt, id_cols = id_cols))
})

test_that("check resulting lx is monotonic by age", {
  output_dt <- qx_to_lx(input_dt, id_cols)
  output_females <- output_dt[sex == "female"]$lx
  testthat::expect_equal(T, all(output_females == cummin(output_females)))
})

# ============================================================================
# lx_to_dx

# set up standard input data.table
input_dt <- data.table(
  sex = rep("both", 4),
  age = c(0, 5, 10, 15),
  lx = c(1, 0.9, 0.7, 0.2)
)
id_cols <- c("sex", "age")

# set up expected output table
expected_dx <- c(0.1, 0.2, 0.5, 0.2)

test_that("check `lx_to_dx()` basic functionality works", {
  output_dt <- lx_to_dx(input_dt, terminal_age = 15, id_cols)
  testthat::expect_equal(output_dt$dx, expected_dx)
})

test_that("check `qx_to_lx()` errors are thrown for different cases", {
  # check error thrown when wrong argument types are given
  testthat::expect_error(lx_to_dx(as.data.frame(input_dt), id_cols, 15, T))
  testthat::expect_error(lx_to_dx(input_dt, "hello", 15, T))
  testthat::expect_error(lx_to_dx(input_dt, id_cols, T, T))
  testthat::expect_error(lx_to_dx(input_dt, id_cols, 15, 15))

  # check error thrown when missing cols
  testthat::expect_error(lx_to_dx(input_dt[, c("sex", "lx")], id_cols, 15, T))
  testthat::expect_error(lx_to_dx(input_dt[, c("sex", "age")], id_cols, 15, T))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(lx_to_dx(non_unique_input_dt, id_cols, 15, T))

  # check error thrown if terminal_age < oldest age in data
  testthat::expect_error(lx_to_dx(input_dt, id_cols, 5))
})


# ============================================================================
# gen_nLx

# set up standard input data.table
dt <- data.table::data.table(
  sex = rep("both", 4),
  age = c(0, 5, 10, 15),
  age_length = c(5, 5, 5, 120),
  mx = c(0.1, 0.2, 0.3, 0.4),
  ax = c(2.5, 2.5, 2.5, 2.5)
)
dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
dt <- qx_to_lx(dt, id_cols = c("sex", "age"))
dt <- lx_to_dx(dt, id_cols = c("sex", "age"), terminal_age = 15)
id_cols <- c("sex", "age")

# set up expected output (rounded)
expected_nLx <- c(4.00, 2.00, 0.57, 0.07)

test_that("check `gen_nLx()` basic functionality works", {
  output_dt <- gen_nLx(dt, id_cols, terminal_age = 15)
  output_nLx <- round(output_dt$nLx, 2)
  testthat::expect_equal(output_nLx, expected_nLx)
})

test_that("check `gen_nLx()` errors are thrown for different cases", {
  # check error thrown when wrong argument types are given
  testthat::expect_error(gen_nLx(dt, "hello", 15))
  testthat::expect_error(gen_nLx(dt, id_cols, T))

  # check error thrown when terminal_age < max age in data
  testthat::expect_error(gen_nLx(dt, id_cols, 10))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(dt, dt)
  testthat::expect_error(gen_nLx(non_unique_input_dt, id_cols = id_cols, 15))
})


# ============================================================================
# gen_Tx

# set up standard input data.table
dt <- data.table(
  sex = rep("male", 3),
  age = c(70, 75, 80),
  nLx = c(0.6, 0.5, 0.4)
)

# set up expected output table
expected_dt <- data.table(
  sex = rep("male", 3),
  age = c(70, 75, 80),
  nLx = c(0.6, 0.5, 0.4),
  Tx = c(1.5, 0.9, 0.4)
)

test_that("check `gen_Tx` basic functionality works", {
  gen_Tx(dt, id_cols = c("age", "sex")) # modifies dt in place
  testthat::expect_equal(dt, expected_dt)
})

test_that("check `gen_Tx` errors are thrown for different cases", {
  # not data.table
  testthat::expect_error(gen_Tx(as.data.frame(dt)))
  # missing nLx
  testthat::expect_error(gen_Tx(dt[, .(age, sex)]))
})


# ============================================================================
# gen_ex

# set up standard input data.table
dt <- data.table(
  age = c(70, 75, 80),
  lx = c(0.6, 0.5, 0.4),
  Tx = c(30, 20, 10)
)

# set up expected output table
expected_dt <- data.table(
  age = c(70, 75, 80),
  lx = c(0.6, 0.5, 0.4),
  Tx = c(30, 20, 10),
  ex = c(50, 40, 25)
)

test_that("check `gen_ex` basic functionality works", {
  gen_ex(dt) # modifies dt in place
  testthat::expect_equal(dt, expected_dt)
})

test_that("check `gen_ex` errors are thrown for different cases", {
  # not data.table
  testthat::expect_error(gen_ex(as.data.frame(dt)))
  # missing lx or Tx
  testthat::expect_error(gen_ex(dt[, .(age, Tx)]))
  testthat::expect_error(gen_ex(dt[, .(age, lx)]))
})

