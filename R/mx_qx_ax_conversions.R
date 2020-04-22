#' @title Convert between mx, qx, and ax values
#'
#' @description Convert between mx and qx values given the length of an age
#'   interval. If ax (the average number of years lived in the x to x + n age
#'   interval by those dying in the interval) exists, use the *_ax_to_*
#'   functions. The mx_to_qx and qx_to_mx functions assume smooth mortality
#'   across the interval.
#'
#' @param mx \[`numeric()`\]\cr mx (mortality death rate)
#' @param qx \[`numeric()`\]\cr qx (probability of death)
#' @param ax \[`numeric()`\]\cr ax (average number of years lived in the age
#'   interval of those who died)
#' @param age_length \[`numeric()`\]\cr The length of the age interval (e.g. 5
#'   for age 5-9)
#'
#' @return \[`numeric()`\]\cr The converted mx, qx, or ax value(s).
#'
#' @details
#' **mx_to_qx:** qx = 1 - exp((-1 * age_length) * mx)
#'
#' **mx_ax_to_qx:** qx = (age_length * mx) / (1 + (age_length - ax) * mx)
#'
#' **qx_to_mx:** mx = log(1 - qx) / (-1 * age_length)
#'
#' **mx_qx_to_ax:** ax = (qx + (mx * age_length * (qx - 1))) / (mx * qx)
#'
#' **qx_ax_to_mx:** mx = qx / (age_length - (age_length * qx) + (ax * qx))
#'
#' **mx_to_ax:** ax = age_length + (1 / mx) - age_length / (1 - exp(- age_length * mx))
#'
#' For all functions calculating qx, if age_length = Inf return qx = 1.
#' For all functions computing mx or ax, if age_length = Inf modify age_length
#' to arbitrarily large value (200) to get real number result.
#'
#' @seealso
#' \itemize{
#'   \item{Preston Demography textbook}
#'   \item \code{vignette("introduction_to_life_tables", package = "lifetableUtils")}
#' }
#'
#' @examples
#' mx_to_qx(0.25, 45)
#' mx_ax_to_qx(0.2, 3, 5)
#' qx_to_mx(0.5, 5)
#' mx_qx_to_ax(0.005, 0.0247, 5)
#' qx_ax_to_mx(0.1, 3, 5)
#' mx_to_ax(0.2, 5)
#' @name mx_qx_ax_conversions
NULL

# ========================================
#' @rdname mx_qx_ax_conversions
#' @export
mx_to_qx <- function(mx, age_length) {

  # main result qx
  result <- 1 - exp((-1 * age_length) * mx)

  # terminal age qx
  terminal <- grep(Inf, age_length)
  result[terminal] <- 1

  return(result)
}

# ========================================
#' @rdname mx_qx_ax_conversions
#' @export
mx_ax_to_qx <- function(mx, ax, age_length) {

  # main result qx
  result <- (age_length * mx) / (1 + (age_length - ax) * mx)

  # terminal age qx
  terminal <- grep(Inf, age_length)
  result[terminal] <- 1

  return(result)
}

# ========================================
#' @rdname mx_qx_ax_conversions
#' @export
qx_to_mx <- function(qx, age_length) {

  # terminal age:
  # set arbitrary non-infinite length
  # check qx = 1
  terminal <- grep(Inf, age_length)
  age_length[terminal] <- 200
  if(any(qx[terminal] != 1)) stop("terminal qx should be 1")

  # main result mx
  result <- log(1 - qx) / (-1 * age_length)

  return(result)
}

# ========================================
#' @rdname mx_qx_ax_conversions
#' @export
mx_qx_to_ax <- function(mx, qx, age_length) {

  # terminal age:
  # set arbitrary non-infinite length
  # check qx = 1
  terminal <- grep(Inf, age_length)
  age_length[terminal] <- 200
  if(any(qx[terminal] != 1)) stop("terminal qx should be 1")

  # main result ax
  result <- (qx + (mx * age_length * (qx - 1))) / (mx * qx)

  return(result)
}

# ========================================
#' @rdname mx_qx_ax_conversions
#' @export
qx_ax_to_mx <- function(qx, ax, age_length) {

  # terminal age:
  # set arbitrary non-infinite length
  # check qx = 1
  terminal <- grep(Inf, age_length)
  age_length[terminal] <- 200
  if(any(qx[terminal] != 1)) stop("terminal qx should be 1")

  # main result mx
  result <- qx / (age_length - (age_length * qx) + (ax * qx))

  return(result)
}

# ========================================
#' @rdname mx_qx_ax_conversions
#' @export
mx_to_ax <- function(mx, age_length) {

  # terminal age:
  # set arbitrary non-infinite length
  terminal <- grep(Inf, age_length)
  age_length[terminal] <- 200

  # main result ax
  result <- age_length + (1 / mx) -
            age_length / (1 - exp(- age_length * mx))

  return(result)
}
