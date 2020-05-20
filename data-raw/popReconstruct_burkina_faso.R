# Reformats example Burkina Faso data from the original "popReconstruct" R
# package by Mark Wheldon.

library(popReconstruct)
data(burkina_faso_females)

# 1.05 was the default used
burkina.faso.data.srb <- matrix(
  data = 1.05,
  nrow = 1,
  ncol = ncol(burkina.faso.females$fertility.rates)
)
colnames(burkina.faso.data.srb) <- colnames(burkina.faso.females$fertility.rates)
rownames(burkina.faso.data.srb) <- 0

burkina_faso_data <- list(
  population = matrix_to_dt(
    mdt = list(female = burkina.faso.females$census.pop.counts),
    year_right_most_endpoint = NULL
  )[, list(year = year_start, sex, age_start, age_end, value)]
)

burkina_faso_initial_estimates <- list(
  srb = matrix_to_dt(
    mdt = burkina.faso.data.srb,
    year_right_most_endpoint = 2005
  ),
  asfr = matrix_to_dt(
    mdt = burkina.faso.females$fertility.rates,
    year_right_most_endpoint = 2005
  )[between(age_start, 15, 45)],
  baseline = matrix_to_dt(
    mdt = list(female = burkina.faso.females$baseline.pop.counts),
    year_right_most_endpoint = NULL
  )[, list(year = year_start, sex, age_start, age_end, value)],
  survival = matrix_to_dt(
    mdt = list(female = burkina.faso.females$survival.proportions),
    year_right_most_endpoint = 2005
  ),
  net_migration = matrix_to_dt(
    mdt = list(female = burkina.faso.females$migration.proportions),
    year_right_most_endpoint = 2005
  )
)

usethis::use_data(burkina_faso_data,
                  burkina_faso_initial_estimates,
                  overwrite = T)
