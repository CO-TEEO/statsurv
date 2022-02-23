mc_pvalue <- function(observed, replicates) {
  if (length(replicates) == 0) {
    return(NULL)
  }
  else {
    f <- Vectorize(function(y) {
      (1 + sum(replicates > y)) / (1 + length(replicates))
    })
    return(f(observed))
  }
}


gumbel_pvalue <- function(observed, replicates, method = "ML", ...) {
  if (length(replicates) < 2) {
    stop("Need at least 2 observations to fit Gumbel distribution.")
  }
  gumbel_mu <- NA
  gumbel_sigma <- NA
  if (method == "ML") {
    gum_fit <- ismev::gum.fit(replicates, show = FALSE, ...)
    gumbel_mu <- gum_fit$mle[1]
    gumbel_sigma <- gum_fit$mle[2]
  }
  else {
    gumbel_sigma <- sqrt(6 * stats::var(replicates) / pi^2)
    gumbel_mu <- mean(replicates) + digamma(1) * gumbel_sigma
  }
  pvalue <- reliaR::pgumbel(observed, gumbel_mu, gumbel_sigma, lower.tail = FALSE)
  return(list(pvalue = pvalue, gumbel_mu = gumbel_mu, gumbel_sigma = gumbel_sigma))
}
