#' Plot QQ
#'
#' Plots Q plot of P-value vector
#'
#' @param P A P-value vector (n) representing importance of each gene
#'
#' @export
plotQQ <- function(P)
{
  p.obs=P
  m2=length(p.obs)
  p.uni=runif(m2,0,1)
  order.obs=order(p.obs)
  order.uni=order(p.uni)

  plot(-log10(p.uni[order.uni]),
       -log10(p.obs[order.obs]), ylim=c(0,7))
  abline(a = 0, b = 1, col = "red")
}
