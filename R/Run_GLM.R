#' Run_GLM
#'
#' Performs GLM on a Genotype, Phenotype and Covariate Matrix.
#'
#' @param X A Genotype Matrix (n x m)
#' @param Y A Phenotype Matrix (n x 1)
#' @param C A Covariate Matrix (n x c)
#'
#' @return P A P-value vector (n) representing importance of each gene
#' @export
Run_GLM <- function(myGD, y, PC)
{
  G=myGD[,-1]
  n=nrow(G)
  m=ncol(G)
  P=matrix(NA,1,m)
  for (i in 1:m){
    x=G[,i]
    if(max(x)==min(x)){
      p=1}else{
        X=cbind(1, PC[,1:3],x)
        LHS=t(X)%*%X
        C=solve(LHS)
        RHS=t(X)%*%y
        b=C%*%RHS
        yb=X%*%b
        e=y-yb
        n=length(y)
        ve=sum(e^2)/(n-1)
        vt=C*ve
        t=b/sqrt(diag(vt))
        p=2*(1-pt(abs(t),n-2))
      } #end of testing variation
    P[i]=p[length(p)]
  } #end of looping for markers
return(P)
}
