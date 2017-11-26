



fussed_lasso_bump_hunter <- function(data,
                                     response,
                                     by_cols = NULL,
                                     max_int = 2) {
  
  #----------------------------------------------------------------------------#
  # Section 0. Check required libraries ----
  #----------------------------------------------------------------------------#
  
  require(genlasso)
  
  #----------------------------------------------------------------------------#
  # Section 1. Input checking ----
  #----------------------------------------------------------------------------#
  
  if (!response %in% colnames(data)) {
    
    stop("response in not in data")
    
  }
  
  if (is.null(by_cols)) {
    
    by_cols <- setdiff(colnames(data), response)
    
  }
  
  if (max_int < 1 | max_int > 3) {
    
    stop("max_int must be between 1 and 2")
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 2.    ----
  #----------------------------------------------------------------------------#
  
  
  
  
  
  
}







set.seed(1)
a <- 16
b <- 16
y = matrix(runif(a*b), a, b)
i = (row(y) - 8.5)^2 + (col(y) - 8.5)^2 <= 4^2
y[i] = y[i] + 1
out = fusedlasso2d(y)
co = coef(out, nlam=5)

par(mfrow=c(2,3))
cols = terrain.colors(ncol(y))
zlim = range(c(range(y),range(co$beta)))
image(y,col=cols,zlim=zlim,axes=FALSE)
for (i in 1:5) {
  image(matrix(co$beta[,i],nrow=nrow(y)),col=cols,zlim=zlim, axes=FALSE)
  mtext(bquote(lambda==.(sprintf("%.3f",co$lambda[i]))))
}



par(mfrow=c(1,1))
gr = graph(edges=get_2d_matrix_edges(y, TRUE), directed = FALSE)
plot(gr)


out2 = fusedlasso(y, graph = gr)
co2 = coef(out2, nlam=5)

par(mfrow=c(2,3))
cols = terrain.colors(ncol(y))
zlim = range(c(range(y),range(co$beta)))
image(y,col=cols,zlim=zlim,axes=FALSE)
for (i in 1:5) {
  image(matrix(co2$beta[,i],nrow=nrow(y)),col=cols,zlim=zlim, axes=FALSE)
  mtext(bquote(lambda==.(sprintf("%.3f",co2$lambda[i]))))
}









