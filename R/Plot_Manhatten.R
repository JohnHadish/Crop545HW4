#' Plot Manhatten
#'
#' Plots manhatten plot of P values
#'
#' @param P A P-value vector (n) representing importance of each gene
#' @param main Name of plot
#' @export
plotManhatten <- function(P, main)
{
  color.vector <- rep(c("deepskyblue","orange","forestgreen","indianred3"),10)
  m=nrow(myPosition)
  plot(t(-log10(P))~seq(1:m),col=color.vector[myPosition[,2]],main=main)
}
