test_that("MACB works", {

  asfr_data <- data.table(
    asfr = c(.051, .196, .208, .147, .075, .024, .004),
    age_start = seq(15, 45, 5),
    age_end = seq(20, 50, 5)
  )

  asfr_data2 <- data.table(
    nfx = c(.2, .2),
    age_start = c(20, 25),
    age_end = c(25, 30)
  )

  id_cols <- c("age_start", "age_end")

  macb_dt <- macb_from_nfx(asfr_data, id_cols = id_cols, nfx_col = "asfr")
  macb_dt2 <- macb_from_nfx(asfr_data2, id_cols = id_cols)

  testthat::expect_equal(macb_dt$macb, 28.11702, tolerance = 1e-5)
  testthat::expect_equal(macb_dt2$macb, 25)

})
