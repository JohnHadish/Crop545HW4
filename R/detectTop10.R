#' detectTop10
#'
#' Finds how many simulated genes made it into top ten
#'
#' @param P vector of p-values of all genes
#' @param QTN.position position of the simulated QTNs
#' @return number of QTN detected in top 10
#' @export
detectTop10 <- function(P, QTN.position)
{
  index=order(P)
  top10=index[1:10]
  detected=intersect(top10, QTN.position)
  falsePositive=setdiff(top10, QTN.position)
  return(length(detected))
}
