#' @title Convert between mx, qx, and ax values
#'
#' @description Convert between mx and qx values given a time for an age
#'   interval. If ax (the average number of years lived in the x to x + n age
#'   interval by those dying in the interval) exists, use the *_ax_to_*
#'   functions. The mx_to_qx and qx_to_mx functions assume smooth mortality
#'   across the interval.
#'
#' @param mx \[`numeric()`\] mx (mortality death rate)
#' @param qx \[`numeric()`\] qx (probability of death)
#' @param ax \[`numeric()`\] ax (average number of years lived in the age
#'   interval of those who died)
#' @param length \[`numeric()`\] the length of the age interval (e.g. 5 for
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
mx_to_qx <- function(mx, length) {
  result <- 1 - exp((-1 * length) * mx)
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
mx_ax_to_qx <- function(mx, ax, length) {
  result <- (length * mx) / (1 + (length - ax) * mx)
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
qx_to_mx <- function(qx, length) {
  result <- log(1 - qx) / (-1 * length)
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
mx_qx_to_ax <- function(mx, qx, length) {
  result <- (qx + (mx * length * (qx - 1))) / (mx * qx)
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
qx_ax_to_mx <- function(qx, ax, length) {
  result <- qx / (length - (length * qx) + (ax * qx))
  return(result)
}

#' @rdname mx_qx_ax_conversions
#' @export
mx_to_ax <- function(mx, length) {
  result <- length + 1 / mx - length / (1 - exp(-length * mx))
  return(result)
}
