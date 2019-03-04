#
#  Risk Analysis using Principal Component Analysis - 3D graph using rgl package
#
#  Plot the first three Principal Components to assess risk exposures.
#
#  Choose the look-back period [num_months], the volatility target [vol_target] and number of groups [num_groups]
#
#

pca_risk_analysis_data_3D  <- function(dataset, num_months = 48, vol_target = 0.05, num_groups = 3){
  require(rgl)
  raw_data_returns <- na.omit(xts::last(dataset, num_months))
  vol_adj_returns  <- apply(raw_data_returns, 2, function(x) x * (vol_target / sqrt(12)) / sd(x))
  pca              <- princomp(vol_adj_returns, cor = T)
  pca.df           <- data.frame(PC1 = pca$loadings[,1],
                                 PC2 = pca$loadings[,2],
                                 PC3 = pca$loadings[,3])
  fit              <- hclust(dist(pca.df, method = "euclidean"), method = "ward.D")
  groups           <- cutree(fit, num_groups)
  pca.df           <- cbind(pca.df, groups)
  plot3d(pca.df$PC1, pca.df$PC2, pca.df$PC3, "PC 1", "PC 2", "PC 3", type = "s", size = 2, lit=TRUE, col = pca.df$groups)
  texts3d(pca.df$PC1, pca.df$PC2, pca.df$PC3, rownames(pca.df), adj = c(0.5,2.0)) 
}

# rod_morley@me.com
