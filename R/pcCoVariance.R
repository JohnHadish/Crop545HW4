#'
#' pcCoVariance
#'
#' Determines principle components (PC) that are in linear dependent to the covariates (C).
#' Removes PC that are linear dependent
#'
#' @param PC (n x p) p Principle components for n samples
#' @param C  (n x c) c Covariates for n samples
#' @return PC_C (n x (p + c - linear dependent)) Principle Components and Covariates which are independent
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

