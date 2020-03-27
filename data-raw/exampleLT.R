library(data.table)

# create baseline with age, age_length, deaths, and population
exampleLT <- data.table(
  age_start = c(0, 1, seq(5, 95, 5)),
  age_end = c(1, seq(5, 95, 5), Inf),
  age_length = c(1, 4, rep(5, 18), Inf),
  deaths = c(72, 17, 12, 14, 31, 36, 42, 59, 101, 157, 240, 368, 525,
             861, 1019, 1252, 2004, 3396, 4972, 3848, 2245),
  population = c(29931, 118479, 145941, 153298, 156399, 150368, 151322,
                 156045, 175432, 177788, 162723, 156276, 145485, 144338,
                 107574, 83885, 74092, 65481, 50605, 22853, 7728)
)

# add on mx, ax, qx
exampleLT[, mx := deaths / population]
exampleLT[, ax := mx_to_ax(mx, age_length)]
exampleLT[, qx := mx_ax_to_qx(mx, ax, age_length)]

# terminal age group
exampleLT[age_length == Inf, qx := 1]
exampleLT[age_length == Inf, ax := mx_qx_to_ax(mx, qx, 200)]

# remove age_lenth
exampleLT[, age_length := NULL]

# save
usethis::use_data(exampleLT, overwrite = TRUE)
