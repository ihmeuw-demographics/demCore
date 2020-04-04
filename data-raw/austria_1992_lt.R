# Example life table from Preston book pg 49
# Austria, males, 1992
# Original data source cited by Preston: United Nations, 1994

# Combination of copied-over values and calculated values. When values are
# calculated, they are cross-referenced with the values in the book to confirm
# equivalency.

# Use for testing because generation does not require running functions from
# this package.

# age info: age (x) and age length (n)
age_start <- c(0, 1, seq(5, 85, 5))
age_length <- c(1, 4, rep(5, 16), Inf)
age_end <- age_start + age_length

# population (nNx) and deaths (nDx)
pop <- c(47925, 189127, 234793, 238790, 254996, 326831, 355086, 324222, 269963,
         261971, 238011, 261612, 181385, 187962, 153832, 105169, 73694, 57512,
         32248)
deaths <- c(419, 70, 36, 46, 249, 420, 403, 441, 508, 769, 1154, 1866, 2043,
            3496, 4366, 4337, 5279, 6460, 6146)

# mx
mx <- deaths / pop

# ax
ax <- c(0.068, 1.626, 2.5, 3.143, 2.724, 2.520, 2.481, 2.601, 2.701, 2.663,
        2.698, 2.676, 2.645, 2.624, 2.619, 2.593, 2.518, 2.423, 5.247)

# qx
qx <- (age_length * mx) / (1 + (age_length - ax) * mx)
qx[19] <- 1

# px
px <- 1 - qx

# lx
lx <- c(100000, 99133, 98986, 98910, 98815, 98334, 97704, 97151, 96492, 95588,
        94195, 91937, 88711, 83845, 76377, 66225, 53803, 37441, 21134)

# dx
dx <- c(867, 147, 76, 95, 481, 630, 553, 659, 904, 1393, 2258, 3225, 4867, 7467,
        10152, 12422, 16362, 16307, 21134)

# nLx
nLx <- c(99192, 396183, 494741, 494375, 492980, 490106, 487127, 484175, 480384,
         474686, 465777, 452188, 432096, 401480, 357713, 301224, 228404, 145182,
         110889)

# Tx
Tx <- rev(cumsum(rev(nLx)))

# ex
ex <- Tx / lx

# combine
austria_1992_lt <- data.table::data.table(
  age_start, age_end, age_length, deaths, pop, mx, ax, qx, px, lx, dx, nLx, Tx, ex
)

# use radix (l0) 1 instead of 100,000
cols <- c("lx", "dx", "nLx", "Tx")
austria_1992_lt[ , (cols) := lapply(.SD, function(x) x / 100000), .SDcols = cols]

# save
usethis::use_data(austria_1992_lt, overwrite = TRUE)
