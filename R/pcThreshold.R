pcThreshold <- function(PC, threshold = 0.014)
{
  summaryPCA=summary(PC)

  pcano=min(which(summaryPCA$importance[2,] < threshold))

  pcasum=PC$x[,1:pcano]
  return(pcasum)
}
