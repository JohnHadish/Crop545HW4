#' Choosing and removing dependent PC's
#'
#' pcCoVariance() Determines principle components (PC) that are in linear dependendence to the
#' covariates (C).Also,removes PC's that are in linear dependence with the covariates
#
#' @details Determines the principle components (PC) that are not in linear dependendence to the covariates (C).
#' @param PC matrix(n x p): p Principle components for n samples
#' @param  C  matrix(n x c):c Covariates for n samples
#' @param  threshold numeric: the value above which one determines the variables to have high corelation
#' @return PC_C matrix(n x (p + c - linear dependent)):Principle Components and Covariates which are not dependent
#' @export
pcCoVariance <- function(PC, C, threshold)
{
  corMatrix <- cor(PC, myCovariate[,-1])
  highCor <- apply(corMatrix, 1, function(r) any(r > threshold))

  print("Removing PCs:")
  print(which(highCor))
  print("Due to high linear dependencies with users covariates")

  df <- as.matrix(data.frame(PC[,!highCor], C[,-1]))

  return(df)
}

