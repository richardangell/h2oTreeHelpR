#------------------------------------------------------------------------------#
# Project      | R-Examples/bump-hunting
#------------------------------------------------------------------------------#
# File         | 2017-11-26_Bump-Hunting.R
#------------------------------------------------------------------------------#
# Description  | Fussed lasso application to bump hunting.
#              |
#------------------------------------------------------------------------------#
# Layout       | Section 0. Load libraries
#              | Section 1. Get adult dataset from uci ml repo
#              | Section 2. Build two different GLMs
#              | Section 3. Get difference between model predictions
#------------------------------------------------------------------------------#
# Inputs /     | data.table
# Dependencies | genlasso
#------------------------------------------------------------------------------#
# Outputs      |
#              |
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Section 0. Load libraries ----
#------------------------------------------------------------------------------#

library(data.table)
library(genlasso)

#source()

#------------------------------------------------------------------------------#
# Section 1. Get adult dataset from uci ml repo ----
#------------------------------------------------------------------------------#

adult <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

adult_test <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test")

adult_columns <- c("age",
                   "workclass",
                   "fnlwgt",
                   "education",
                   "education_num",
                   "marital_status",
                   "occupation",
                   "relationship",
                   "race",
                   "sex",
                   "capital_gain",
                   "capital_loss",
                   "hours_per_week",
                   "native_country",
                   "response")

colnames(adult) <- adult_columns

colnames(adult_test) <- adult_columns

adult[["response"]] <- as.factor(adult[["response"]])

#------------------------------------------------------------------------------#
# Section 2. Build two different GLMs ----
#------------------------------------------------------------------------------#

glm1 <- glm(reformulate(c("age",
                          "workclass",
                          "fnlwgt",
                          "education",
                          "education_num",
                          "marital_status",
                          "occupation",
                          "relationship",
                          "race"),
                        "response"),
            family = binomial(link = "logit"),
            data = adult)


glm2 <- glm(reformulate(c("age",
                          "workclass",
                          "fnlwgt",
                          "education",
                          "education_num",
                          "marital_status",
                          "occupation",
                          "relationship",
                          "race",
                          "sex",
                          "capital_gain",
                          "capital_loss",
                          "hours_per_week",
                          "native_country"),
                        "response"),
            family = binomial(link = "logit"),
            data = adult)

#------------------------------------------------------------------------------#
# Section 3. Get difference between model predictions ----
#------------------------------------------------------------------------------#

adult$glm_pred_difference <- glm1$fitted.values / glm2$fitted.values





#------------------------------------------------------------------------------#
# get_2d_matrix_edges fcn ----
#------------------------------------------------------------------------------#

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


#------------------------------------------------------------------------------#
# fussed_lasso_bump_hunter testing ----
#------------------------------------------------------------------------------#

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
