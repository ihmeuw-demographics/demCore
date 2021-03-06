# reformats example Thailand data from the Bayesian Reconstruction github repo
# (https://github.com/markalava/Bayesian-Reconstruction)
repo_path <- "~/Documents/Bayesian-Reconstruction/"
load(paste0(repo_path, "Thailand_Example/data/thai_initial_ests.RData"))
load(paste0(repo_path, "Thailand_Example/data/thai_propvars.RData"))

rownames(srbTHAI.mat) <- 0

thailand_data <- list(
  population = matrix_to_dt(
    mdt = list(female = censusTHAI.mat$female,
               male = censusTHAI.mat$male),
    year_right_most_endpoint = NULL
  )[, list(year = year_start, sex, age_start, age_end, value)]
)

thailand_initial_estimates <- list(
  srb = matrix_to_dt(
    mdt = srbTHAI.mat,
    year_right_most_endpoint = 2000
  ),
  asfr = matrix_to_dt(
    mdt = asFertTHAI.mat,
    year_right_most_endpoint = 2000
  )[between(age_start, 15, 45)],
  baseline = matrix_to_dt(
    mdt = list(female = baselineTHAI.mat$female,
               male = baselineTHAI.mat$male),
    year_right_most_endpoint = NULL
  )[, list(year = year_start, sex, age_start, age_end, value)],
  survival = matrix_to_dt(
    mdt = list(female = asSurvTHAI.mat$female,
               male = asSurvTHAI.mat$male),
    year_right_most_endpoint = 2000
  ),
  net_migration = matrix_to_dt(
    mdt = list(female = asMigTHAI.mat$female,
               male = asMigTHAI.mat$male),
    year_right_most_endpoint = 2000
  )
)

usethis::use_data(thailand_data,
                  thailand_initial_estimates,
                  overwrite = T)
