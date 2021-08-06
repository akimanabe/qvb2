# R script for fitting the generalized q-VBGF (Manabe et al. 2018. PLoS ONE 13(6):e0199346)

#' Generalized q - von Bertalanffy growth function
#'
#' @param Lhat Initial parameter for L_hat; Growth scale factors
#' @param r Initial parameter for r; Growth exponents
#' @param q Initial parameter for q; Growth indeterminacy timing parameter
#' @param tau Initial parameter for tau; Maturation timing parameter
#' @param t0 Initial parameter for t0; Theoretical age at size zero
#'
#'
#' @return vector of size at given age
#' @export
#'
#' @examples
#' \dontrun{
#' qvb(age = seq(0, 5, 0.2), p = list(Lhat = 300, r = 0.75,
#' q = 1.2, tau = 2, t0 = -0.01))
#' }

qvb <-
  function(age, p = list(Lhat, r, q, tau, t_0)){

    names(p) <- c("Lhat", "r", "q", "tau", "t_0")

    p$Lhat * p$tau^p$r * (1- (pmax(0,1-(1-p$q)*(age-p$t_0)/p$tau))^(1/(1-p$q)) )^p$r

  }
