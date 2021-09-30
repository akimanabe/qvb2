#' Generalized q - von Bertalanffy growth function
#' (Manabe et al. 2018. PLoS ONE 13(6):e0199346)
#'
#' @param L Length
#' @param p List of growth parameters
#'  \describe{
#'    \item{Lhat}{Growth parameter that set the size at inflection point}
#'    \item{r}{Growth paramter that shapes the initial growth pattern}
#'    \item{q}{Energy allocation parameter for reproduction.
#'    Determinate growth is described when q < 1 and Indeterminate growth is
#'    described when q > 1. In addition, q must not be 1.}
#'    \item{tau}{Timing of maturity coefficient.}
#'    \item{c}{Conversion factor that converts reproductive investment to GW}
#'  }
#' @param a a of Length-weight relationship W = a * L ^ b
#' @param b b of Length-weight relationship W = a * L ^ b
#'
#' @return Estimated gonad weight
#' @export
#'
#' @examples
#' \dontrun{
#' qvb_gonad(
#' L = 250,
#' p = list(300, 0.8, 1.2, 2, 0.005),
#' a = 0.0001,
#' b = 3.00)
#' }
qvb_gonad <-
  function(L, p = list(Lhat, r, q, tau, c), a, b){

    assertthat::assert_that(length(p) == 5,
                            msg = "All five parameters needs to have a value.")

    assertthat::assert_that(is.numeric(L),
                            msg = "Length must be numeric.")

    assertthat::assert_that(is.numeric(unlist(p)),
                            msg = "Parameters must be numeric.")

    assertthat::assert_that(p[[3]] != 1,
                            msg = "Parameter 'q' must not be 1.")

    assertthat::assert_that(p[[5]] > 0,
                            msg = "Conversion factor c msut be greater than 0.")

    names(p) <- c("Lhat", "r", "q", "tau", "c")
    unname(p$c * b * a * L ^ b * p$r * ((L / p$Lhat) ^ (-1 / p$r)) *
             (1 - pmax(0, 1-(1 / p$tau)*((L / p$Lhat) ^ p$q)) ^ p$q))
  }
