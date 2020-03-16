#' G2P
#'
#' Genotype 2 Phenotype function.
#' Taken from Lecture, using for simulation.
#'
#' @param X A Genotype Matrix (n x m)
#' @param h2
#' @param alpha
#' @param NQTN
#' @param distrubution
#' @return
#' @export
G2P=function(X,h2,alpha,NQTN,distribution)
{
  n=nrow(X)
  m=ncol(X)

  #Sampling QTN
  QTN.position=sample(m,NQTN,replace=F)
  SNPQ=as.matrix(X[,QTN.position])
  QTN.position

  #QTN effects
  if(distribution=="norm")
  {addeffect=rnorm(NQTN,0,1)
  }else
  {addeffect=alpha^(1:NQTN)}

  #Simulate phenotype
  effect=SNPQ%*%addeffect
  effectvar=var(effect)
  residualvar=(effectvar-h2*effectvar)/h2
  residual=rnorm(n,0,sqrt(residualvar))
  y=effect+residual
  return(list(addeffect = addeffect, y=y, add = effect, residual = residual, QTN.position=QTN.position, SNPQ=SNPQ))
}
