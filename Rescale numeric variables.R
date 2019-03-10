# Standardization (rescaling x so that mean(x) = 0 and sd(x) = 1:
# (x - mean(x)) / sd(x)                 # x normally distributed
# <numeric variable> <- 
#   scale(<numeric variable>, 
#         center = TRUE, 
#         scale = TRUE)
# (x - median(x)) / IQR(x)              # x not normally distributed (IQR = Q3 - Q1)
# <numerical variable> <- 
#   scale(<numerical variable>, 
#         center = median(<numerical variable>), 
#         scale = IQR(<numerical variable>))

standardizeNumericVariablesIQR <- function(x) {
  y <- x[, c(3, 4, 7, 8, 9)]
  y <- as.data.frame(apply(y, 2, function(x) scale(x, center = median(x), scale = IQR(x))))
  y
}

# Normalization - transforming the variable values to the range [0,1]: 
# (x - min(x))/(max(x) - min(x))
# library(clusterSim)
# <dataframe with numeric columns> <-                       # works with vectors and matrices as well
#   data.Normalization(<dataframe with numeric columns>,
#                      type = "n4",                         # normalization: (x - min(x)) / (max(x) - min(x))
#                      normalization = "column")            # normalization by columns

normalizeNumericVariables <- function(x) {
  y <- x[, c(3, 4, 7, 8, 9)]
  # library(clusterSim)
  # <dataframe with numeric columns> <-                       # works with vectors and matrices as well
  #   data.Normalization(<dataframe with numeric columns>,
  #                      type = "n4",                         # normalization: (x - min(x)) / (max(x) - min(x))
  #                      normalization = "column")            # normalization by columns
  library(clusterSim)
  y <- as.data.frame(apply(y, 2, function(x) data.Normalization(x, type = "n4", normalization = "column")))
  
  y <- cbind(y, x[, c(1, 2, 5, 6, 10)])                       # numeric columns normalized, now add the other ones
  y <- y[names(x)]                                            # reorder columns to match the original order in x
  y
}

# Transform binary variables into numeric:
# <dataset>$<binary variable> <- as.integer(<dataset>$<binary variable>)  # <binary variable>: factor with 2 levels

binToNum <- function(x) {
  y <- x
  y$Cover <- as.integer(y$Cover)
  y
}

# Transform factor variables into numeric (without using dummy variables, for simplicity):
# <dataset>$<factor variable> <- as.numeric(<dataset>$<factor variable>)

factorToNumNoDummies <- function(x) {
  y <- x
  y$Year <- as.numeric(y$Year)
  y$Single.certification <- as.numeric(y$Single.certification)
  # !!! If the other variables are normalized, should I normalize these two now as well?
  y
}

rescaleNumericVariables <- function(x) {
  y <- normalizeNumericVariables(x)
  y <- binToNum(y)
  y <- factorToNumNoDummies(y)
  y
}
