library(h2oEnsemble)  # This will load the `h2o` R package as well
h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # Clean slate - just in case the cluster was already running
memory.limit(100000)
#
#
#### Load Data into H2O Cluster
#
#First, import a sample binary outcome train and test set into the H2O cluster.
train <- h2o.importFile(path = normalizePath("C:/Avito/ItemInfo_test.csv"))
test <- h2o.importFile(path = normalizePath("C:/SFCrime/test.csv"))
y <- "Category"
x <- setdiff(names(train), y)
#
#For binary classification, the response should be encoded as factor (also known as the [enum](https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html) type in Java).  The user can specify column types in the `h2o.importFile` command, or you can convert the response column as follows:
#
train[,y] <- as.factor(train[,y])  
test[,y] <- as.factor(test[,y])
#
#
#### Specify Base Learners & Metalearner
#For this example, we will use the default base learner library for `h2o.ensemble`, which includes the default H2O GLM, Random Forest, GBM and Deep Neural Net (all using default model parameter values).  We will also use the default metalearner, the H2O GLM.
#
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.gbm.wrapper"
#
#
#### Train an Ensemble
#Train the ensemble (using 5-fold internal CV) to generate the level-one data.  Note that more CV folds will take longer to train, but should increase performance.
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train, 
                    #family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))
#
#
#### Predict 
#Generate predictions on the test set.
pred <- predict(fit, test)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(test[,y])[,1]
#
#The `predict` method for an `h2o.ensemble` fit will return a list of two objects.  The `pred$pred` object contains the ensemble predictions, and `pred$basepred` is a matrix of predictions from each of the base learners.  In this particular example where we used four base learners, the `pred$basepred` matrix has four columns.  Keeping the base learner predictions around is useful for model inspection and will allow us to calculate performance of each of the base learners on the test set (for comparison to the ensemble).
#
#
#### Model Evaluation
#
#Since the response is binomial, we can use Area Under the ROC Curve ([AUC](https://www.kaggle.com/wiki/AUC)) to evaluate the model performance.  We first generate predictions on the test set and then calculate test set AUC using the [cvAUC](https://cran.r-project.org/web/packages/cvAUC/) R package.
#
##### Ensemble test set AUC
library(cvAUC)
cvAUC::AUC(predictions = predictions, labels = labels)
# 0.7888723
#
##### Base learner test set AUC
#We can compare the performance of the ensemble to the performance of the individual learners in the ensemble.  Again, we use the `AUC` utility function to calculate performance.
#
L <- length(learner)
auc <- sapply(seq(L), function(l) cvAUC::AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels)) 
data.frame(learner, auc)
#                    learner       auc
# 1          h2o.glm.wrapper 0.6871288
# 2 h2o.randomForest.wrapper 0.7711654
# 3          h2o.gbm.wrapper 0.7817075
# 4 h2o.deeplearning.wrapper 0.7425813
#
#So we see the best individual algorithm in this group is the GBM with a test set AUC of 0.782, as compared to 0.789 for the ensemble.  At first thought, this might not seem like much, but in many industries like medicine or finance, this small advantage can be highly valuable. 
#
#To increase the performance of the ensemble, we have several options.  One of them is to increase the number of internal cross-validation folds using the `cvControl` argument.  The other options are to change the base learner library or the metalearning algorithm.
#
#Note that the ensemble results above are not reproducible since `h2o.deeplearning` is not reproducible when using multiple cores, and we did not set a seed for `h2o.randomForest.wrapper`.
#
#Additional note: In a future version, performance metrics such as AUC will be computed automatically, as in the other H2O algos.
#
#
#### Specifying new learners
#
#Now let's try again with a more extensive set of base learners.  The **h2oEnsemble** packages comes with four functions by default that can be customized to use non-default parameters. 
#
#Here is an example of how to generate a custom learner wrappers:
#
h2o.glm.1 <- function(..., alpha = 0.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 50, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 200, sample_rate = 0.75, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 200, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.4 <- function(..., ntrees = 200, nbins = 50, balance_classes = TRUE, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, balance_classes = balance_classes, seed = seed)
h2o.gbm.1 <- function(..., ntrees = 100, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed)
h2o.gbm.2 <- function(..., ntrees = 100, nbins = 50, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.gbm.3 <- function(..., ntrees = 100, max_depth = 10, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.8, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.5 <- function(..., ntrees = 100, col_sample_rate = 0.7, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.6 <- function(..., ntrees = 100, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.7 <- function(..., ntrees = 100, balance_classes = TRUE, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, balance_classes = balance_classes, seed = seed)
h2o.gbm.8 <- function(..., ntrees = 100, max_depth = 3, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.3 <- function(..., hidden = c(500,500), activation = "RectifierWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, balance_classes = TRUE, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes, seed = seed)
h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.6 <- function(..., hidden = c(50,50), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.7 <- function(..., hidden = c(100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
#
#
#Let's grab a subset of these learners for our base learner library and re-train the ensemble.
#
#### Customized base learner library
learner <- c("h2o.glm.wrapper",
             "h2o.randomForest.1", "h2o.randomForest.2",
             "h2o.gbm.1", "h2o.gbm.6", "h2o.gbm.8",
             "h2o.deeplearning.1", "h2o.deeplearning.6", "h2o.deeplearning.7")
#
#Train with new library:
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train,
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

# Generate predictions on the test set:
pred <- predict(fit, test)
predictions <- as.data.frame(pred$pred)[,3]
labels <- as.data.frame(test[,y])[,1]
#
#Evaluate the test set performance: 
cvAUC::AUC(predictions = predictions , labels = labels)
# 0.7904223
#We see an increase in performance by including a more diverse library.
#
#Base learner test AUC (for comparison)
L <- length(learner)
auc <- sapply(seq(L), function(l) cvAUC::AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels)) 
data.frame(learner, auc)
