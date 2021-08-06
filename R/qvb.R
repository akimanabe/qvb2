# R script for fitting the generalized q-VBGF

#' Generalized q - von Bertalanffy growth function
#' (Manabe et al. 2018. PLoS ONE 13(6):e0199346)
#'
#' @param age Numerical age value(s) in year to estimate the length
#' @param p List of growth parameters
#'  \describe{
#'    \item{Lhat}{Growth parameter that set the size at inflection point}
#'    \item{r}{Growth paramter that shapes the initial growth pattern}
#'    \item{q}{Energy allocation parameter for reproduction.
#'    Determinate growth is described when q < 1 and Indeterminate growth is
#'    described when q > 1. In addition, q must not be 1.}
#'    \item{tau}{Timing of maturity coefficient.}
#'    \item{t_0}{Theoretical body size at 0}
#'  }
#'
#' @return Estimated length at age in vector
#' @export
#'
#' @examples
#' \dontrun{
#' qvb(age = seq(0, 5, 0.2), p = list(Lhat = 300, r = 0.75,
#' q = 1.2, tau = 2, t0 = -0.01))
#' }

qvb <-
  function(age, p = list(Lhat, r, q, tau, t_0)) {

    assertthat::assert_that(length(p) == 5,
                            msg = "All five parameters needs to have a value.")

    assertthat::assert_that(is.numeric(age),
                            msg = "Age must be numeric.")

    assertthat::assert_that(is.numeric(unlist(p)),
                            msg = "Parameters must be numeric.")

    names(p) <- c("Lhat", "r", "q", "tau", "t_0")

    p$Lhat * p$tau^p$r * (1 - (pmax(
      0, 1 - (1 - p$q) * (age - p$t_0) / p$tau)) ^ (1 / (1 - p$q)))^p$r

  }
