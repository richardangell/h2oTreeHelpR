#------------------------------------------------------------------------------#
# Project      | R-Examples
#------------------------------------------------------------------------------#
# File         | 2017-11-26_Bump-Hunting.R
#------------------------------------------------------------------------------#
# Description  | Description of program.
#              |
#------------------------------------------------------------------------------#
# Layout       | Section 0. Get adult dataset from uci machine learning repository
#              | Section 1. bbb
#------------------------------------------------------------------------------#
# Inputs /     | data.table
# Dependencies | 
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
# Section 1. Get adult dataset from uci machine learning repository ----
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
# Section 3. Get difference between predictions ----
#------------------------------------------------------------------------------#

adult$glm_pred_difference <- glm1$fitted.values / glm2$fitted.values







