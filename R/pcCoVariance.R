#'
#' pcCoVariance
#'
#' Determines is covariance between vovariates and principle components
#'
#' @param PC (n x p) p Principle components for n samples
#' @param C  (n x c) c Covariates for n samples
#'
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

