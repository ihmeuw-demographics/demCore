## Regression parameters to predict single-year lx from abridged lx

## Details:
## (1) Prep Human Mortality Database lx series
## (2) Generate regression fit to expand abridged to full life tables

# setup -------------------------------------------------------------------

library(data.table)
library(readr)

# directory where 1x1 HMD life tables are downloaded to
hmd_dir <- "FILEPATH"

lt_keys <- c("location", "year", "sex")

# load HMD ----------------------------------------------------------------

# load in HMD life tables
lt_full <- lapply(1L:2L, function(s) {
  sex_var <- ifelse(s == 1, "male", "female")
  sex_abr <- ifelse(s == 1, "m", "f")

  lt_path <- paste0(hmd_dir, "lt_", sex_var, "/", sex_abr, "ltper_1x1/")
  files <- grep("1x1", list.files(lt_path), value = T)

  # get all 1X1 lifetables for specified location
  files <- data.table(fpath = files)
  files[, location := gsub(paste0(".", sex_abr, "ltper_1x1.txt"), "", fpath)]

  # read in lifetable
  lt_data <- lapply(1:nrow(files), function(i) {
    fpath <- files[i, fpath]
    location <- files[i, location]
    fpath <- paste0(hmd_dir, "lt_", sex_var, "/", sex_abr, "ltper_1x1/", fpath)
    lt_data <- readr::read_fwf(
      file = fpath,
      fwf_cols(
        year = c(3, 6),
        age = c(16, 18),
        mx = c(24, 30),
        qx = c(33, 39),
        ax = c(42, 45)
      ),
      skip = 4
    )
    lt_data <- data.table(lt_data)
    lt_data[, c("sex_id", "location") := list(s, location)]
    return(lt_data)
  })
  lt_data <- rbindlist(lt_data)
})
lt_full <- rbindlist(lt_full)

# prep HMD ----------------------------------------------------------------

# prep sex variable
lt_full[sex_id == 1, sex := "male"]
lt_full[sex_id == 2, sex := "female"]
lt_full[, sex_id := NULL]

# drop years where data are indicated by HMD as problematic
lt_full <- lt_full[!(location == "TWN" & year < 1980)]
lt_full <- lt_full[!(location == "UKR" & year < 1970)]
lt_full <- lt_full[!(location == "BLR" & year < 1970)]
lt_full <- lt_full[!(location == "EST" & year > 2000)]
lt_full <- lt_full[!(location == "IRL" & between(year, 1950, 1985))]
lt_full <- lt_full[!(location == "ITA" & between(year, 1872, 1905))]
lt_full <- lt_full[!(location == "LVA" & between(year, 1959, 1969))]
lt_full <- lt_full[!(location == "LTV" & between(year, 1959, 1969))]
lt_full <- lt_full[!(location == "PRT" & between(year, 1940, 1970))]
lt_full <- lt_full[!(location == "RUS" & between(year, 1959, 1969))]
lt_full <- lt_full[!(location == "SVK" & between(year, 1950, 1961))]
lt_full <- lt_full[!(location == "ESP" & between(year, 1908, 1960))]
lt_full <- lt_full[!(location == "FIN" & year == 1918)] # civil war
lt_full <- lt_full[!(location == "FIN" & between(year, 1939, 1949))] # war with Soviet Union
lt_full <- lt_full[!(between(year, 1914, 1920))] # Spanish Flu
lt_full <- lt_full[!(between(year, 1940, 1945))] # World War 2
lt_full <- lt_full[!(year < 1900)]

# only keep lx
lt_full[, c("mx", "ax") := NULL]

# determine abridged lt age groups
abridged_ages <- c(0, 1, seq(5, 110, 5))
lt_full[, abridged_age := cut(age,
  breaks = c(abridged_ages, Inf),
  labels = abridged_ages,
  right = F,
  include.lowest = T
)]
lt_full[, abridged_age := as.integer(as.character(abridged_age))]

# set column order and key
setcolorder(lt_full, c("location", "year", "sex", "age", "abridged_age", "qx"))
setkeyv(lt_full, c("location", "year", "sex", "age", "abridged_age"))

# aggregate to abridged ages using px
lt_full[, px := 1 - qx]
lt_full[, px_abridged := prod(px), by = c(lt_keys, "abridged_age")]
lt_full[, qx_abridged := 1 - px_abridged]

# don't include age 0 or terminal age (110)
lt_full <- lt_full[age > 0 & age < 110]

# set age length (n)
lt_full[, n := 5]
lt_full[abridged_age == 1, n := 4]

# log-space
lt_full[, log_qx := log(qx)]
lt_full[, log_qx_abridged := log(qx_abridged)]
lt_full <- lt_full[!is.infinite(log_qx) & !is.infinite(log_qx_abridged)]

# Fit all the linear models -----------------------------------------------

log_fits <- lapply(unique(lt_full$age), function(a) {
  fits <- lapply(c("male", "female"), function(s) {

    # subset by age and sex
    fit_data <- lt_full[age == a & sex == s]

    # fit linear model log single-year qx from log abridged qx
    fit <- lm(formula = log_qx ~ log_qx_abridged, data = fit_data)

    # extract fit
    coef <- summary(fit)$coefficients[, 1]
    fit <- data.table(sex = s, age = a, intercept = coef[1], slope = coef[2])

    return(fit)
  })
  fits <- rbindlist(fits)
  return(fits)
})

fullLTpars <- rbindlist(log_fits)

# save --------------------------------------------------------------------

usethis::use_data(fullLTpars, overwrite = TRUE)
