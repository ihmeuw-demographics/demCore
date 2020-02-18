testthat::context("gen_ex tests")

library(data.table)

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
