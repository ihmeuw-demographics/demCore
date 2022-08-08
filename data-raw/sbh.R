# Example ceb data from Manual X handbook table 49 on page 78.
# Survey in Panama between August and October 1976

library(data.table)

sbh_panama_1976 <- data.table(
  age_start = seq(15, 45, 5),
  age_end = seq(20, 50, 5),
  n_women = c(2695, 2095, 1828, 1605, 1362, 1128, 930),
  sex = c(rep("male", 7), rep("female", 7)),
  ceb = c(278, 1380, 2395, 3097, 3444, 3274, 2682, 279, 1253, 2362, 2988, 3278, 3093, 2594),
  ced = c(24, 77, 172, 236, 348, 394, 354, 16, 53, 140, 199, 288, 292, 335)
)
sbh_panama_1976_all <- sbh_panama_1976[
  ,
  list(sex = "all", ceb = sum(ceb), ced = sum(ced)),
  by = c("age_start", "age_end", "n_women")
]
sbh_panama_1976 <- rbind(sbh_panama_1976, sbh_panama_1976_all)
sbh_panama_1976[, Pi := ceb / n_women]
sbh_panama_1976[, Di := ced / ceb]

usethis::use_data(sbh_panama_1976, overwrite = TRUE)

# Example ceb data from Manual X handbook table 47 & 48 on pages 77-78.
# Survey in Panama between August and October 1976

sbh_trussell_coeffs <- list(
  data.table(
    model = "North",
    type = "q(x)/D(x)",
    age_start = seq(15, 45, 5),
    age_end = seq(20, 50, 5),
    age_start_nqx = 0,
    age_end_nqx = c(1, 2, 3, 5, 10, 15, 20),
    a_i = c(1.1119, 1.2390, 1.1884, 1.2046, 1.2586, 1.2240, 1.1772),
    b_i = c(-2.9287, -0.6865, 0.0421, 0.3037, 0.4236, 0.4222, 0.3486),
    c_i = c(0.8507, -0.2745, -0.5156, -0.5656, -0.5898, -0.5456, -0.4624)
  ),
  data.table(
    model = "West",
    type = "q(x)/D(x)",
    age_start = seq(15, 45, 5),
    age_end = seq(20, 50, 5),
    age_start_nqx = 0,
    age_end_nqx = c(1, 2, 3, 5, 10, 15, 20),
    a_i = c(1.1415, 1.2563, 1.1851, 1.1720, 1.1865, 1.1746, 1.1639),
    b_i = c(-2.7070, -0.5381, 0.0633, 0.2341, 0.3080, 0.3314, 0.3190),
    c_i = c(0.7663, -0.2637, -0.4177, -0.4272, -0.4452, -0.4537, -0.4435)
  ),
  data.table(
    model = "North",
    type = "t(x)",
    age_start = seq(15, 45, 5),
    age_end = seq(20, 50, 5),
    age_start_nqx = 0,
    age_end_nqx = c(1, 2, 3, 5, 10, 15, 20),
    a_i = c(1.0921, 1.3207, 1.5996, 2.0779, 2.7705, 4.1520, 6.9650),
    b_i = c(5.4732, 5.3751, 2.6268, -1.7908, -7.3403, -12.2448, -13.9160),
    c_i = c(-1.9672, 0.2133, 4.3701, 9.4126, 14.9352, 19.2349, 19.9542)
  ),
  data.table(
    model = "West",
    type = "t(x)",
    age_start = seq(15, 45, 5),
    age_end = seq(20, 50, 5),
    age_start_nqx = 0,
    age_end_nqx = c(1, 2, 3, 5, 10, 15, 20),
    a_i = c(1.0970, 1.3062, 1.5305, 1.9991, 2.7632, 4.3468, 7.5242),
    b_i = c(5.5628, 5.5677, 2.5528, -2.4261, -8.4065, -13.2436, -14.2013),
    c_i = c(-1.9956, 0.2962, 4.8962, 10.4282, 16.1787, 20.1990, 20.0162)
  )
)
sbh_trussell_coeffs <- rbindlist(sbh_trussell_coeffs)
usethis::use_data(sbh_trussell_coeffs, overwrite = TRUE)
