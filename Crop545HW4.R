library(plotly)
library(ggplot2)
library(gridExtra)
library(knitr)
#Plots the Scree/Elbow plot,the Proprtion of the variance explained by the PC's and the Cumulative Proportion of the variance explained by the PC's
graphPCA <- function(allPC)
{
  # Plotting var_exp_plot
  pca_comp_plot_data <- data.frame(allPC$x)
  summaryPCA=summary(allPC)
  var_exp_plot_data <- data.frame(t(summaryPCA$importance))
  names(var_exp_plot_data) <- c("sdev", "var_exp", "cum_var_exp")
  var_exp_plot_data$pca_index <- 1:nrow(var_exp_plot_data)
  var_exp_plot <- ggplot(data = var_exp_plot_data, aes(x = pca_index,y = var_exp)) +
    geom_line() +
    geom_point() +
    labs(x = "PCA component index", y = "Variance explained", title = "GAPIT Demo GT: Variance Explained")
  # Plotting cumulative variance
  cum_var_exp_plot <- ggplot(data = var_exp_plot_data, aes(x = pca_index, y = cum_var_exp)) +
    geom_line() +
    geom_point() +
    labs(x = "PCA component index", y = "Cumulative Variance explained", title = "GAPIT Demo GT: Cumulative Variance Explained")
  pca_comp_plot_12 <-
    ggplot(data = pca_comp_plot_data, aes(x = PC1, y = PC2)) +
    geom_point()
  pca_comp_plot_13 <-
    ggplot(data = pca_comp_plot_data, aes(x = PC1, y = PC3)) +
    geom_point()
  pca_comp_plot_23 <-
    ggplot(data = pca_comp_plot_data, aes(x = PC2, y = PC3)) +
    geom_point()
  return(screeplot(allPC,npcs =pcThreshold(allPC), type = "lines"))# Scree Plot
  return(var_exp_plot)
  return(cum_var_exp_plot)
  return(grid.arrange(pca_comp_plot_12, pca_comp_plot_13, pca_comp_plot_23, nrow = 2, ncol = 2, top = "GAPIT Demo GT: Principal Components"))
}
