#' GWASbyGLM
#'
#' Performs GLM on a Genotype, Phenotype and Covariate Matrix.
#'
#' @param X A Genotype Matrix (n x m)
#' @param Y A Phenotype Matrix (n x 1)
#' @param C A Covariate Matrix (n x c)
#' @return P A P-value vector (n) representing importance of each gene
#' @export
GWASbyGLM <- function(X, y, C)
{
  G=X[,-1]
  n=nrow(G)
  m=ncol(G)
  P=matrix(NA,1,m)
  PC_Col = ncol(C)
  for (i in 1:m){
    x=G[,i]
    if(max(x)==min(x)){
      p=1}else{
        XL=cbind(1, C[,1:PC_Col],x)
        LHS=t(XL)%*%XL
        CL=solve(LHS)
        RHS=t(XL)%*%y
        b=CL%*%RHS
        yb=XL%*%b
        e=y-yb
        n=length(y)
        ve=sum(e^2)/(n-1)
        vt=CL*ve
        t=b/sqrt(diag(vt))
        p=2*(1-pt(abs(t),n-2))
      } #end of testing variation
    P[i]=p[length(p)]
  } #end of looping for markers
return(P)
}
z
