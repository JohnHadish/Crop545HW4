#' GWAS by Cor
#'
#' Performs GWAS by Correlation
#'
#' @param X A Genotype Matrix (n x m)
#' @param Y A Phenotype Matrix (n x 1)
#'
#' @return P A P-value vector (n) representing importance of each gene
#' @export
GWASbyCor=function(X,Y)
{
  n=nrow(X)
  r=cor(Y,X)
  n=nrow(X)
  t=r/sqrt((1-r^2)/(n-2))
  P=2*(1-pt(abs(t),n-2))
  zeros=P==0
  P[zeros]=1e-10
  return(P)
}
