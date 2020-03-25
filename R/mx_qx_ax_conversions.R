#' @title Convert between mx, qx, and ax values
#'
#' @description Convert between mx and qx values given the length of an age
#'   interval. If ax (the average number of years lived in the x to x + n age
#'   interval by those dying in the interval) exists, use the *_ax_to_*
#'   functions. The mx_to_qx and qx_to_mx functions assume smooth mortality
#'   across the interval.
#'
#' @param mx \[`numeric()`\] mx (mortality death rate)
#' @param qx \[`numeric()`\] qx (probability of death)
#' @param ax \[`numeric()`\] ax (average number of years lived in the age
#'   interval of those who died)
#' @param age_length \[`numeric()`\] the length of the age interval (e.g. 5 for
#'   age 5-9)
#'
#' @return Numeric value representing the converted mx, qx, or ax value.
#'
#' @examples
#' mx_to_qx(0.25, 45)
#' mx_ax_to_qx(.2, 3, 5)
#' qx_to_mx(.5, 5)
#' mx_qx_to_ax(.5, .25, 5)
#' qx_ax_to_mx(.1, 3, 5)
#' mx_to_ax(0.2, 5)
#' @name mx_qx_ax_conversions
NULL

#' @rdname mx_qx_ax_conversions
#' @export
mx_to_qx <- function(mx, age_length) {
  result <- 1 - exp((-1 * age_length) * mx)
  if(age_length == Inf) result <- 1
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
mx_ax_to_qx <- function(mx, ax, age_length) {
  result <- (age_length * mx) / (1 + (age_length - ax) * mx)
  if(age_length == Inf) result <- 1
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
qx_to_mx <- function(qx, age_length) {
  result <- log(1 - qx) / (-1 * age_length)
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
mx_qx_to_ax <- function(mx, qx, age_length) {
  result <- (qx + (mx * age_length * (qx - 1))) / (mx * qx)
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
qx_ax_to_mx <- function(qx, ax, age_length) {
  result <- qx / (age_length - (age_length * qx) + (ax * qx))
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
mx_to_ax <- function(mx, age_length) {
  result <- age_length + 1 / mx - age_length / (1 - exp(- age_length * mx))
  return(result)
}
