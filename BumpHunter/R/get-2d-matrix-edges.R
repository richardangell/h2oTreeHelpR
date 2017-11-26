
get_2d_matrix_edges <- function(mat, include_diagonals = FALSE) {
  
  n_row <- nrow(mat)
  n_col <- ncol(mat)
  
  mat_idx <- matrix(1:(n_row * n_col), nrow = n_row)
  
  results <- list()
  
  i <- 0
  
  if (n_row > 1) {
    
    for (i in 1:(n_row - 1)) {
      
      results[[i]] <- c(rbind(mat_idx[i, ], mat_idx[i + 1, ]))
      
    }
    
  }
  
  if (n_col > 1) {
    
    for (j in 1:(n_col - 1)) {
      
      results[[i + j]] <- c(rbind(mat_idx[ , j], mat_idx[ , j + 1]))
      
    }
    
  }
  
  k <- length(results)
  
  if (include_diagonals && n_row > 1 && n_col > 1) {
    
    for (j in 1:(n_col - 1)) {
      
      k <- k + 1
      
      results[[k]] <- c(rbind(mat_idx[-n_row, j], mat_idx[-1, j + 1]))
      
      k <- k + 1
      
      results[[k]] <- c(rbind(mat_idx[-1, j], mat_idx[-n_row, j + 1]))
      
    }
    
  }
  
  return(unlist(results))
  
}


