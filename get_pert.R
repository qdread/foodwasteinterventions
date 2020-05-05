# Function to get PERT min and max parameters, if 5%, 50%, and 95% quantiles are known. This assumes shape parameter of 4

get_pert <- function(p, q, fit.weights = NULL) {
  if (is.null(fit.weights)) fit.weights <- rep(1, length(p))
  minimize <- function(theta) {
    summand <- suppressWarnings(mc2d::ppert(q = q, min = theta[1], 
                                            mode = theta[2], max = theta[3], shape = 4) - 
                                  p)
    summand <- summand * fit.weights
    sum(summand^2)
  }
  start_value <- q * c(0.9, 1, 1.1)
  result <- optim(par = start_value, minimize, method = 'BFGS')
  return(data.frame(min = result$par[1], mode = result$par[2], max = result$par[3]))
}
