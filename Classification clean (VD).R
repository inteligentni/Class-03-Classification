# 
# Classification
# Vladan Devedzic, Sep 07, 2017
# 


# (Installing and) Loading the required R packages:
# install.packages('ggplot2')
library(ggplot2)


###########
# Dataset #
###########

# The starting (expanded) dataset:
# <dataframe> <- read.csv("<filename>", 
#   stringsAsFactors = FALSE)
# str(<dataframe>)                       # structure of <dataframe>, all variables/columns
# summary(<dataframe>)                   # summarizing column values in the <dataframe>
the.beatles.songs <- 
  read.csv("The Beatles songs dataset, v3.csv", stringsAsFactors = FALSE)
str(the.beatles.songs)
summary(the.beatles.songs)


##################
# Decision trees #
##################

# Reading the dataset

# A modified version of the dataset has been saved earlier using:
# saveRDS(object = <dataframe or another R object>, file = "<filename>")  # save R object for the next session
# Restoring the dataset from the corresponding RData file:
# <dataframe or another R object> <- readRDS(file = "<filename>")         # restore R object in the next session
# The Beatles songs dataset has been saved earlier using:
# saveRDS(object = the.beatles.songs, file = "The Beatles songs dataset, v3.2.RData")
the.beatles.songs <- readRDS("The Beatles songs dataset, v3.2.RData")
summary(the.beatles.songs)

# Binary classification - 
# decide on the output variable and make appropriate adjustments

# Examine the Top.50.Billboard variable more closely:
the.beatles.songs$Top.50.Billboard

# Output variable: 
# a song is among Billboard's top 50 Beatles songs or it isn't

# Create a new varible to represent the output variable:
the.beatles.songs$Top.50.BB <- "No"
the.beatles.songs$Top.50.BB
the.beatles.songs$Top.50.BB[the.beatles.songs$Top.50.Billboard > 0] <- "Yes"
the.beatles.songs$Top.50.BB

# Turn the new variable into a factor:
the.beatles.songs$Top.50.BB <- as.factor(the.beatles.songs$Top.50.BB)
head(the.beatles.songs$Top.50.BB)

# Save this version of the dataset as well, for future reuse:
# saveRDS(object = <dataframe or another R object>, file = "<filename>")  # save R object for the next session
saveRDS(object = the.beatles.songs, file = "The Beatles songs dataset, v3.3.RData")

# Examine the distribution of the two factor values more precisely:
# table(<dataset>$<output variable>)
# prop.table(table(<dataset>$<output variable>))
# round(prop.table(table(<dataset>$<output variable>)), digits = 2)
table(the.beatles.songs$Top.50.BB)
prop.table(table(the.beatles.songs$Top.50.BB))
round(prop.table(table(the.beatles.songs$Top.50.BB)), digits = 2)

# Split the dataset into train and test sets:
# install.packages("caret")
# library(caret)
# set.seed(<n>)
# <train dataset indices> <-                            # stratified partitioning: 
#     createDataPartition(<dataset>$<output variable>,  # the same distribution of the output variable in both sets
#                         p = .80,                      # 80/20% of data in train/test sets
#                         list = FALSE)                 # don't make a list of results, make a matrix
# <train dataset> <- <dataset>[<train dataset indices>, ]
# <test dataset>  <- <dataset>[-<train dataset indices>, ]
library(caret)
set.seed(444)
# set.seed(333) - results in a different split, and different tree and eval. metrics
train.data.indices <- createDataPartition(the.beatles.songs$Top.50.BB, p = 0.80, list = FALSE)
train.data <- the.beatles.songs[train.data.indices, ]
test.data <- the.beatles.songs[-train.data.indices, ]

# Build the model / decision tree:
# install.packages("rpart")
# library(rpart)
# <decision tree> <- rpart(<output variable> ~                                     # build the tree
#                          <predictor variable 1> + <predictor variable 2> + ...,  # . to include all variables
#                          data = <train dataset>,
#                          method = "class")                                       # build classification tree
# print(<decision tree>)                                                           # default textual representation
library(rpart)
top.50.tree.1 <- rpart(Top.50.BB ~ Single.certification + Covered.by + Year, 
                       data = train.data,
                       method = "class")
print(top.50.tree.1)

# Depict the model:
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# library(rattle)
# library(rpart.plot)
# library(RColorBrewer)
# fancyRpartPlot(<decision tree>)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(top.50.tree.1)

# Make predictions:
# <predictions> <- predict(object = <decision tree>, 
#                          newdata = <test dataset>, 
#                          type = "class")
# <predictions>[<i1>:<ik>]                            # examine some of the predictions
# <predictions dataframe> <-
#   data.frame(<observation ID> = <test dataset>$<observation ID column>, 
#              <another relevant feature> = <test dataset>$<another relevant feature column>, 
#              ..., 
#              <output feature> = <test dataset>$<output variable>, 
#              <predictions feature> = <predictions>)
top.50.predictions.1 <- predict(top.50.tree.1, newdata = test.data, type = "class")
top.50.predictions.1[1:20]
top.50.predictions.1.dataframe <- data.frame(Song = test.data$Title, 
                                             Top.50.Billboard = test.data$Top.50.Billboard,
                                             Top.50.BB = test.data$Top.50.BB,
                                             Prediction = top.50.predictions.1)

# How good are the predictions?
# Compute confusion matrix:
# <cm> <- table(True = <predictions dataframe>$<output variable>, 
#               Predicted = <predictions dataframe>$<predictions>)
# Compute evaluation metrics:
# accuracy = (TP + TN) / N
# precision = TP / (TP + FP)
# recall = TP / (TP + FN)
# F1 = (2 * precision * recall) / (precision + recall)
# Note: precision and recall are inversely proportional to each other.
# <evaluation metrics vector> <- <user-specified function>(<cm>)
#   accuracy = sum(diag(cm)) / sum(cm)
#   precision <- TP / (TP + FP)
#   recall <- TP / (TP + FN)
#   F1 <- (2 * precision * recall) / (precision + recall)

cm.1 <- table(True = test.data$Top.50.BB, Predicted = top.50.predictions.1)
cm.1
# alternatively:
# cm.1 <- table(True = top.50.predictions.1.dataframe$Top.50.BB, 
#               Predicted = top.50.predictions.1.dataframe$Prediction)
# cm.1
source("Evaluation metrics.R")
eval.1 <- getEvaluationMetrics(cm.1)
eval.1

# Try another tree, using fewer and/or different predictors 
# (e.g., Duration + Covered.by, Duration + Year, etc.). 
# In practice, strong predictors from the previous tree are kept. 
# Also try changing the seed in splitting the dataset into 
# train and test sets.
top.50.tree.2 <- rpart(Top.50.BB ~ Duration + Covered.by, 
                       data = train.data, 
                       method = "class")
# top.50.tree.2 <- rpart(Top.50.BB ~ .,       # use almost all variables, excluding some specific ones
#                        data = subset(train.data, select = -c(Title, Top.50.Billboard)),
#                        method = "class")
print(top.50.tree.2)
fancyRpartPlot(top.50.tree.2)
top.50.predictions.2 <- predict(top.50.tree.2, newdata = test.data, type = "class")
top.50.predictions.2[1:20]
top.50.predictions.2.dataframe <- data.frame(Song = test.data$Title, 
                                             Top.50.Billboard = test.data$Top.50.Billboard,
                                             Top.50.BB = test.data$Top.50.BB,
                                             Prediction = top.50.predictions.2)
cm.2 <- table(True = test.data$Top.50.BB, Predicted = top.50.predictions.2)
cm.2
eval.2 <- getEvaluationMetrics(cm.2)
eval.2

# Controlling rpart parameters:
# cp (complexity parameter) - don't split at a node if the split does not improve the model by at least cp (default: 0.01)
# minsplit - don't attempt a split at a node if the number of observations is not higher than minsplit (default: 20)
# install.packages("rpart")
# library(rpart)
# <decision tree> <- rpart(<output variable> ~                                     # build the tree
#                          <predictor variable 1> + <predictor variable 2> + ...,  # . to include all variables
#                          data = <train dataset>,
#                          method = "class",                                       # build classification tree
#                          control = rpart.control(minsplit = <n>, cp = <q>))      # decrease both for larger tree
# print(<decision tree>)                                                           # default textual representation
top.50.tree.3 <- rpart(Top.50.BB ~ Single.certification + Covered.by + Year, 
                       data = train.data,
                       method = "class", 
                       control = rpart.control(minsplit = 10, cp = 0.001))
print(top.50.tree.3)
fancyRpartPlot(top.50.tree.3)
top.50.predictions.3 <- predict(top.50.tree.3, newdata = test.data, type = "class")
top.50.predictions.3[1:20]
top.50.predictions.3.dataframe <- data.frame(Song = test.data$Title, 
                                             Top.50.Billboard = test.data$Top.50.Billboard,
                                             Top.50.BB = test.data$Top.50.BB,
                                             Prediction = top.50.predictions.3)
cm.3 <- table(True = test.data$Top.50.BB, Predicted = top.50.predictions.3)
cm.3
eval.3 <- getEvaluationMetrics(cm.3)
eval.3

# Compare the results (the corresponding models/trees):
# data.frame(rbind(<evaluation metrics 1>, <evaluation metrics 2>), 
#            row.names = c("<tree 1>", "<tree 2>"))
data.frame(rbind(eval.1, eval.3), 
           row.names = c("top.50.tree.1", "top.50.tree.3"))
# Model 3 exhibits overfitting. It is a frequent case with large trees.

# Cross-validate the model - find the optimal value for cp (the most important parameter), 
# in order to avoid overfitting the model to the training data:
# install.packages("e1071")                                     # relevant caret functions need e1071
# install.packages("caret")
# library(e1071)
# library(caret)
# <folds> = trainControl(method = "cv", number = <k>)           # define <k>-fold cross-validation parameters
# <cpGrid> = expand.grid(.cp =                                  # specify the range of the cp values to examine
#                        seq(from = <start value>, to = <end value>, by = <step>))
# set.seed(<seed>)
# train(<output variable> ~                                     # find the optimal value for cp
#       <predictor variable 1> + <predictor variable 2> + ...,  # . to include all variables
#       data = <train dataset>,
#       method = "rpart",                                       # use rpart() to build multiple classification trees
#       control = rpart.control(minsplit = 10), 
#       trControl = <folds>, tuneGrid = <cpGrid>)               # <folds> and <cpGrid> from above
library(e1071)
library(caret)
folds = trainControl(method = "cv", number = 10)                # 10-fold cross-validation
cpGrid = expand.grid(.cp = seq(from = 0.001, to = 0.05, by = 0.001))
set.seed(11)
train(Top.50.BB ~ Single.certification + Covered.by + Year, 
      data = train.data,
      method = "rpart",
      control = rpart.control(minsplit = 10),
      trControl = folds, tuneGrid = cpGrid)

# Prunning a complex tree using the optimal cp value:
# <prunned decision tree> <- prune(<decision tree>, cp = <optimal cp value>)
# print(<prunned decision tree>)
# fancyRpartPlot(<prunned decision tree>)
top.50.tree.5 <- prune(top.50.tree.3, cp = 0.013)    # cp value found in the previous step (train())
print(top.50.tree.5)
fancyRpartPlot(top.50.tree.5)

# Make predictions with the prunned tree:
top.50.predictions.5 <- predict(top.50.tree.5, newdata = test.data, type = "class")
top.50.predictions.5[1:20]
top.50.predictions.5.dataframe <- data.frame(Song = test.data$Title, 
                                             Top.50.Billboard = test.data$Top.50.Billboard,
                                             Top.50.BB = test.data$Top.50.BB,
                                             Prediction = top.50.predictions.5)
cm.5 <- table(True = test.data$Top.50.BB, Predicted = top.50.predictions.5)
cm.5
eval.5 <- getEvaluationMetrics(cm.5)
eval.5

# Compare all relevant models:
# data.frame(rbind(<evaluation metrics 1>, <evaluation metrics 2>, ...), 
#            row.names = c("<tree 1>", "<tree 2>", ...))
data.frame(rbind(eval.1, eval.3, eval.5), 
           row.names = c("top.50.tree.1", "top.50.tree.3", "top.50.tree.5"))


#######
# KNN #
#######

# Reading the dataset

# Restoring the dataset from the corresponding RData file:
# <dataframe or another R object> <- readRDS(file = "<filename>")         # restore R object in the next session
# The Beatles songs dataset has been saved earlier using:
# saveRDS(object = the.beatles.songs, file = "The Beatles songs dataset, v3.4.RData")
the.beatles.songs <- readRDS("The Beatles songs dataset, v3.4.RData")

# Adapting the dataset

# Eliminate Top.50.Billboard from the dataset. 
# Top.50.BB has been created from Top.50.Billboard, hence Top.50.Billboard predicts Top.50.BB 100%. 
# Still, store Top.50.Billboard in a separate vector for possible later use.
top.50.billboard <- the.beatles.songs$Top.50.Billboard
the.beatles.songs$Top.50.Billboard <- NULL

# Save this version of the dataset as well, for future reuse:
saveRDS(object = the.beatles.songs, file = "The Beatles songs dataset, v3.5.RData")

# Rescaling of numeric variables needed?
# summary(<dataframe>)                    # examine the ranges of numeric variables
summary(the.beatles.songs)

# Check if numeric variables follow normal distribution:
  # summary(<numeric variable>)           # the mean and the median values similar: probably normal distribution
  # plot(density((<numeric variable>))    # visual inspection
  # hist(<numeric variable>)              # visual inspection
  # qqnorm(<numeric variable>)            # values lie more or less along the diagonal (straight line)
  # shapiro.test(<numeric variable>)      # good for small sample sizes, e.g. n < ~2000; H0: normal distribution
plot(density(the.beatles.songs$Covered.by))
  # ...                                   # check the distributions of other variables as well

source("Rescale numeric variables.R")
the.beatles.songs.rescaled <- rescaleNumericVariables(the.beatles.songs)

# Split the dataset into train and test sets:
# install.packages("caret")
# library(caret)
# set.seed(<n>)
# <train dataset indices> <-                            # stratified partitioning: 
#     createDataPartition(<dataset>$<output variable>,  # the same distribution of the output variable in both sets
#                         p = .80,                      # 80/20% of data in train/test sets
#                         list = FALSE)                 # don't make a list of results, make a matrix
# <train dataset> <- <dataset>[<train dataset indices>, ]
# <test dataset>  <- <dataset>[-<train dataset indices>, ]
library(caret)
set.seed(444)
# set.seed(333) - results in a different split, and different results and eval. metrics
train.data.indices <- createDataPartition(the.beatles.songs.rescaled$Top.50.BB, p = 0.80, list = FALSE)
train.data <- the.beatles.songs.rescaled[train.data.indices, ]
test.data <- the.beatles.songs.rescaled[-train.data.indices, ]

# Build the model:
# library(class)
# <knn model> <- knn(train = <training dataset>,        # training data without the output (class) variable
#                    test = <test dataset>,             # test data without the output (class) variable
#                    cl = <class values for training>,  # output (class) variable is specified here
#                    k = <n>)                           # <n>: random guess, or obtained from cross-validation 
# head(<knn model>)
library(class)
top.50.knn.1 <- knn(train = train.data[, -c(1, 10)],    # eliminate Title (non-numeric) and output/class (Top.50.BB)
                    test = test.data[, -c(1, 10)],      # eliminate Title (non-numeric) and output/class (Top.50.BB)
                    cl = train.data$Top.50.BB,          # output (class) variable
                    k = 5)                              # k = 5: random value to start with
head(top.50.knn.1)                                      # these are already the predictions, i.e. no predict() etc.
which(test.data$Top.50.BB != top.50.knn.1)

# Evaluate the model
# Compute confusion matrix:
# <cm> <- table(True = <test dataset>$<output variable>, 
#               Predicted = <test dataset>$<predictions>)
knn.cm.1 <- table(True = test.data$Top.50.BB, Predicted = top.50.knn.1)
knn.cm.1

# Compute evaluation metrics:
# accuracy = (TP + TN) / N
# precision = TP / (TP + FP)
# recall = TP / (TP + FN)
# F1 = (2 * precision * recall) / (precision + recall)
# Note: precision and recall are inversely proportional to each other.
# <evaluation metrics vector> <- <user-specified function>(<cm>)
#   accuracy = sum(diag(cm)) / sum(cm)
#   precision <- TP / (TP + FP)
#   recall <- TP / (TP + FN)
#   F1 <- (2 * precision * recall) / (precision + recall)
source("Evaluation metrics.R")
eval.knn.1 <- getEvaluationMetrics(knn.cm.1)
eval.knn.1

# What if k had a different value?
# Cross-validate the model - find the optimal value for k (the most important parameter), 
# in order to avoid overfitting the model to the training data:
# install.packages("e1071")                                     # relevant caret functions need e1071
# install.packages("caret")
# library(e1071)
# library(caret)
# <folds> = trainControl(method = "cv", number = <k>)           # define <k>-fold cross-validation parameters
# <cpGrid> = expand.grid(.k =                                   # specify the range of the (odd) values to examine
#                        seq(from = <start value>, to = <end value>, by = <step>))
# set.seed(<seed>)
# <knn cv> <- train(<output variable> ~                         # find the optimal value for k
#       <predictor variable 1> + <predictor variable 2> + ...,  # . to include all variables
#       data = <train dataset>,
#       method = "knn",                                         # use knn() to build multiple classification models
#       trControl = <folds>, tuneGrid = <cpGrid>)               # <folds> and <cpGrid> from above
library(e1071)
library(caret)
knn.folds = trainControl(method = "cv", number = 10)            # 10-fold cross-validation
knn.cpGrid = expand.grid(.k = seq(from = 3, to = 25, by = 2))
set.seed(11)
knn.cv <- train(Top.50.BB ~ . - Title, 
                data = train.data,
                method = "knn",
                trControl = knn.folds, tuneGrid = knn.cpGrid)
knn.cv

# Plot the cross-validation results (accuracy for different values of k):
# plot(<knn model>)   # the model obtained by <knn model> <- train(...)
plot(knn.cv)

# Build the model and compute confusion matrix and evaluation metrics for another value of k:
top.50.knn.2 <- knn(train = train.data[, -c(1, 10)],    # eliminate Title (non-numeric) and output/class (Top.50.BB)
                    test = test.data[, -c(1, 10)],      # eliminate Title (non-numeric) and output/class (Top.50.BB)
                    cl = train.data$Top.50.BB,          # output (class) variable
                    k = 7)                              # k = 7: another random value to test and compare
knn.cm.2 <- table(True = test.data$Top.50.BB, Predicted = top.50.knn.2)
knn.cm.2
eval.knn.2 <- getEvaluationMetrics(knn.cm.2)
eval.knn.2

# Compare the evaluation metrics of the two classifiers:
# data.frame(rbind(<evaluation metrics 1>, <evaluation metrics 2>, ...), 
#            row.names = c("<model 1>", "<model 2>", ...))
data.frame(rbind(eval.knn.1, eval.knn.2), 
           row.names = c("eval.knn.1", "eval.knn.2"))


###############
# Naive Bayes #
###############

# Read an appropriate version of the dataset:
# <dataframe or another R object> <- readRDS(file = "<filename>")         # restore R object from another session
# The Beatles songs dataset, v3.5.RData (without duplicated song names),
# has been saved in the session on KNN,
# assuming that the.beatles.songs$Top.50.BB (a factor variable) is the output variable:
# saveRDS(object = the.beatles.songs, file = "The Beatles songs dataset, v3.5.RData")
# The same output variable is assumed in this session.
the.beatles.songs <- readRDS("The Beatles songs dataset, v3.5.RData")
str(the.beatles.songs)

# NB essentials:
# * typically used with categorical (factor) variables
# * numeric variables (if any) should be either:
#     - represented as probabilities, if they follow normal distribution, or 
#     - discretized (split either into equal-length or equal-frequency intervals (the latter is used more often))

# Check numeric variables in the dataset for normality assumption:
# apply(<numeric dataframe>,  # <original dataframe>[, c(<num. col. 1>, <num. col. 1>, ...]
#       MARGIN = 2,           # apply FUN by columns
#       FUN = shapiro.test)   # Shapiro-Wilks' test of normality; good for small no. of observations (< 2000)
apply(the.beatles.songs[, c(3:4, 7:9)], MARGIN = 2, FUN = shapiro.test)   # no normally distributed numeric vars

# Discretize numeric variables (using bnlearn::discretize()):
# library(bnlearn)
# ?discretize()
# <new dataframe with discretized variables> <- 
#   discretize(<numeric dataframe>,               # <original dataframe>[, c(<num. col. 1>, <num. col. 1>, ...]
#              method = "quantile" |              # use equal-frequency intervals (default)
#              method = "interval" |              # use equal-length intervals
#              method = "hartemink",              # use Hartemink's algorithm
#              breaks = c(<n1>, <n2>, ..., <ncol>))       # no. of discrete intervals for each column
library(bnlearn)
discretized.features <- discretize(the.beatles.songs[, c(3:4, 7:9)], 
                                   method = "interval", 
                                   breaks = c(5, 5, 5, 5, 5))
summary(discretized.features)

# Re-compose the dataframe with the discretized features:
# <dataframe> <- cbind(<dataframe 1>[, c(<col11>, <col12>, ...)], <dataframe 2>[, c(<col21>, <col22>, ...)])
# <dataframe> <- <dataframe>[, c(<col i>, <col k>, ...)]            # rearrange columns (optional)
# Alternatively:
# <dataframe> <- <dataframe>[, names(<original dataframe>)]         # rearrange columns (optional)
the.beatles.songs.nb <- cbind(the.beatles.songs[, c(1, 2, 5, 6, 10)], discretized.features)
the.beatles.songs.nb <- the.beatles.songs.nb[, names(the.beatles.songs)]

# Split the dataset into train and test sets:
# install.packages("caret")
# library(caret)
# set.seed(<n>)
# <train dataset indices> <-                            # stratified partitioning: 
#     createDataPartition(<dataset>$<output variable>,  # the same distribution of the output variable in both sets
#                         p = .80,                      # 80/20% of data in train/test sets
#                         list = FALSE)                 # don't make a list of results, make a matrix
# <train dataset> <- <dataset>[<train dataset indices>, ]
# <test dataset>  <- <dataset>[-<train dataset indices>, ]
library(caret)
set.seed(1)
# set.seed(333) - results in a different split, and different results and eval. metrics
train.data.indices <- createDataPartition(the.beatles.songs.nb$Top.50.BB, p = 0.80, list = FALSE)
train.data <- the.beatles.songs.nb[train.data.indices, ]
test.data <- the.beatles.songs.nb[-train.data.indices, ]

# Build the model:
# library(e1071)
# ?naiveBayes
# <model> <- naiveBayes(<output variable> ~ .,          # include all predictors from the training set
#                       data = <training dataset>)
# <model> <- naiveBayes(<output variable> ~ 
#                       <var 1> + <var 2> + ...,        # include only selected predictors from the training set
#                       data = <training dataset>)
# print<model>                                          # shows P(o/c) for factor vars, mean() and sd() for numeric
library(e1071)
top.50.nb.1 <- naiveBayes(Top.50.BB ~ ., 
                          data = train.data[, -1])
print(top.50.nb.1)

# Make predictions:
# <predictions> <- predict(object = <NB model>, 
#                          newdata = <test dataset>, 
#                          type = "class")
# <predictions>[<i1>:<ik>]                            # examine some of the predictions
# <predictions dataframe> <-
#   data.frame(<observation ID> = <test dataset>$<observation ID column>, 
#              <another relevant feature> = <test dataset>$<another relevant feature column>, 
#              ..., 
#              <output feature> = <test dataset>$<output variable>, 
#              <predictions feature> = <predictions>)
top.50.nb.predictions.1 <- predict(top.50.nb.1, newdata = test.data[, -1], type = "class")
top.50.nb.predictions.1[1:20]
top.50.nb.predictions.1.dataframe <- data.frame(Song = test.data$Title, 
                                                Top.50.BB = test.data$Top.50.BB,
                                                Prediction = top.50.nb.predictions.1)

# Evaluate the model
# Compute confusion matrix:
# <cm> <- table(True = <test dataset>$<output variable>, 
#               Predicted = <test dataset>$<predictions>)
nb.cm.1 <- table(True = test.data$Top.50.BB, Predicted = top.50.nb.predictions.1)
nb.cm.1

# Compute evaluation metrics:
# accuracy = (TP + TN) / N
# precision = TP / (TP + FP)
# recall = TP / (TP + FN)
# F1 = (2 * precision * recall) / (precision + recall)
# Note: precision and recall are inversely proportional to each other.
# <evaluation metrics vector> <- <user-specified function>(<cm>)
#   accuracy = sum(diag(cm)) / sum(cm)
#   precision <- TP / (TP + FP)
#   recall <- TP / (TP + FN)
#   F1 <- (2 * precision * recall) / (precision + recall)
source("Evaluation metrics.R")
eval.nb.1 <- getEvaluationMetrics(nb.cm.1)
eval.nb.1

# Build another model, using only selected predictors, and compute confusion matrix and evaluation metrics:
library(e1071)
top.50.nb.2 <- naiveBayes(Top.50.BB ~ Year + Duration + Other.releases, 
                          data = train.data[, -1])
print(top.50.nb.2)
top.50.nb.predictions.2 <- predict(top.50.nb.2, newdata = test.data[, -1], type = "class")
top.50.nb.predictions.2[1:20]
top.50.nb.predictions.2.dataframe <- data.frame(Song = test.data$Title, 
                                                Top.50.BB = test.data$Top.50.BB,
                                                Prediction = top.50.nb.predictions.2)
nb.cm.2 <- table(True = test.data$Top.50.BB, Predicted = top.50.nb.predictions.2)
nb.cm.2
source("Evaluation metrics.R")
eval.nb.2 <- getEvaluationMetrics(nb.cm.2)
eval.nb.2

# Compare the evaluation metrics of the two classifiers:
# data.frame(rbind(<evaluation metrics 1>, <evaluation metrics 2>, ...),
#            row.names = c("<model 1>", "<model 2>", ...))
data.frame(rbind(eval.nb.1, eval.nb.2),
           row.names = c("eval.nb.1", "eval.nb.2"))

# ROC curve (Receiver Operating Characteristic)
# sensitivity (True Positive Rate, TPR) - the proportion of correctly identified positive cases (same as recall)
#   TPR = TP / (TP + FN)
# specificity (True Negative Rate, TNR) - the proportion of correctly identified negative cases
#   TNR = TN / (TN + FP)
# False Positive Rate, FPR - the proportion of incorrectly identified negative cases
#   FPR = 1 - TNR = FP / (TN + FP)
# ROC curve is the plot representing the function: TPR = f(FPR).
# It can be used to select a probability threshold for the classifier (it does not have to be 0.5). 
# To do this, making predictions is done by computing probabilities for each class value (suc as 'Yes' and 'No').

# Build yet another model, to be used for plotting the ROC curve:
# library(e1071)
# ?naiveBayes
# <model> <- naiveBayes(<output variable> ~ .,          # include all predictors from the training set
#                       data = <training dataset>)
# <model> <- naiveBayes(<output variable> ~ 
#                       <var 1> + <var 2> + ...,        # include only selected predictors from the training set
#                       data = <training dataset>)
# print<model>                                          # shows P(o/c) for factor vars, mean() and sd() for numeric
library(e1071)
top.50.nb.3 <- naiveBayes(Top.50.BB ~ Year + Duration + Other.releases,      # can be the same as for top.50.nb.2
                          data = train.data[, -1])

# Make predictions as probabilities:
# <predictions> <- predict(object = <NB model>, 
#                          newdata = <test dataset>, 
#                          type = "raw")              # the value "raw" means "compute probabilities, not classes"
# <predictions>[<i1>:<ik>]                            # examine some of the predictions
# <predictions dataframe> <-
#   data.frame(<observation ID> = <test dataset>$<observation ID column>, 
#              <another relevant feature> = <test dataset>$<another relevant feature column>, 
#              ..., 
#              <output feature> = <test dataset>$<output variable>, 
#              <predictions feature> = <predictions>)
top.50.nb.predictions.3 <- predict(top.50.nb.3, newdata = test.data[, -1], type = "raw")
top.50.nb.predictions.3[1:20, ]
top.50.nb.predictions.3.dataframe <- data.frame(Song = test.data$Title, 
                                                Top.50.BB = test.data$Top.50.BB,
                                                Prediction.probability.No = top.50.nb.predictions.3[, 1], 
                                                Prediction.probability.Yes = top.50.nb.predictions.3[, 2])

# Compute ROC curve parameters, AUC, and plot the curve:
# library(pROC)
# <ROC curve parameters> <-                               # compute ROC curve parameters
#   roc(response = <test dataset>$<output variable>, 
#       predictor = <predicted probabilities>[, <1 | 2>]) # col. no. of the "positive class" (can be the No class!)
# <ROC curve parameters>$auc                              # extract and show AUC 
library(pROC)
top.50.nb.predictions.3.roc <- 
  roc(response = test.data$Top.50.BB, 
      predictor = top.50.nb.predictions.3[, 2])
top.50.nb.predictions.3.roc$auc

# Plot the ROC curve:
# plot.roc(<ROC curve parameters>,              # computed in the previous step
#          print.thres = TRUE,                  # show the probability threshold (cut-off point) on the plot
#          print.thres.best.method = 
#             "youden" |          # maximize the sum of sensitivity and specificity (the distance to the diag. line)
#             "closest.topleft")  # minimize the distance to the top-left point of the plot
plot.roc(top.50.nb.predictions.3.roc, 
         print.thres = TRUE, 
         print.thres.best.method = "youden")

# Getting the probability threshold and other ROC curve parameters using pROC::coords(): 
# <ROC coords> <- coords(<ROC curve parameters>,                            # computed in the previous step 
#                        ret = c("accuracy", "spec", "sens", "thr", ...),   # ROC curve parameters to return
#                        x =                    # the coordinates to look for:
#                            "local maximas" |  # local maximas of the ROC curve
#                            "best" | ...)      # the point with the best sum of sensitivity and specificity, i.e. 
#                                               # the same as the one shown on the ROC curve
# <ROC coords>
top.50.nb.predictions.3.coords <- 
  coords(top.50.nb.predictions.3.roc,
         ret = c("accuracy", "spec", "sens", "thr"),
         x = "local maximas")
top.50.nb.predictions.3.coords


###################################
# Resources, readings, references #
###################################

# 10-fold cross-validation: https://www.openml.org/a/estimation-procedures/1
