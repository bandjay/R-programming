####################################################################
########## DISCRIMINANT ANALYSIS ###################################
####################################################################

# fucntion for calculating covariance matrix
my_cov<-function(x,y)   
{
  n_col=ncol(x)
  n_row=nrow(x)
  cov_mat=matrix(NA,n_col,n_col)
  for (c in 1:n_col)
  {
    for (i in 1:n_col)
    {
      sum=0
      x_mean=mean(x[,c],na.rm=TRUE)
      y_mean=mean(y[,i],na.rm=TRUE)
      sum = sum((x[,c] - x_mean) * (y[,i] - y_mean))
      cov_mat[c,i]=(sum/(n_row-1))
    }
  }
  return (cov_mat)
}


# quadratic discriminant fucntion for model fitting
# Learning decision boundary between classes using MAP
quad_dis <- function(x, response, ...)    
{
  x=as.matrix(x) # converting the predictors into the matrix
  n_obs <- nrow(x)   # obtaining no.of observations
  p_num <- ncol(x)   # obtaining no.of predictors
  response <- as.factor(response) # Converting the resposne to factor 
  response_levels <- levels(response) # obtaining levels of response factor
  response_counts <- as.vector(table(response)) # obtaining counts of response
  names(response_counts) <- response_levels     # naming the response counts with factor levels. 
  prior_probs <- response_counts/length(response_levels)  # Obtaining prior probabilities for each class.
  prior_len <- length(prior_probs)   # length of prior probabilities
  group_means_classes <- tapply(x, list(rep(response, ncol(x)), col(x)), mean) # calculating group means for centriod.
  log_det <- rep(0,prior_len) #  initialising log of determinant for covariance matrix
  cov_mat<-array(dim=c(p_num,p_num,prior_len)) # initialising the 3-D matrix for the covariance terms for each of the class.
  x_muK_sigmaK <- matrix(0, n_obs, prior_len) # initialising x_muK_sigmaK_sqr
  Log_det <- matrix(0, n_obs, prior_len) # initialising log of determinant for dataset of all classes.
  # scaling the predictors with centered around group_mean ,populating the covariance matrix, 
  # computing logarithm of determinant , x_muK_sigmaK_sqr
  for (i in 1L:prior_len){
    class_k_count <- response_counts[i] # count of no.of observations for class K
    # scaling the predictors with centered around group_mean of class K
    X <- scale(x[unclass(response) == i, ], center=group_means_classes[i, ], scale=FALSE)
    cov_mat[,,i]<-my_cov(X,X)  # obtaining covariance matrix for observations of class K.
    log_det[i] <- determinant(cov_mat[,,i])[[1]][1] # calculating log determinant for class K
    mean_mat<-matrix(group_means_classes[i, ], nrow(x),p_num, byrow = TRUE) # mean matrix popualted with class K group means
    x_muK_sigmaK[,i]<- diag((x - mean_mat) %*% as.matrix(solve(cov_mat[,,i])) %*%t((x - mean_mat)))
    # caluclating x_muK_sigmaK for class K
    Log_det[, i] <- log_det[i] # calulating log determinant for all the classes.
  }
  log_prior<-matrix(log(prior_probs),n_obs,prior_len, byrow=TRUE) # caluclating the log of prior probabilites
  MAP_bound <-  -0.5* Log_det -0.5* x_muK_sigmaK+ log_prior # Caluclating MAP decision descriminant boundary
  posterior <- exp(MAP_bound)/(1+exp(MAP_bound))  # obtaining posterior probabilites
  cl <- factor(max.col(posterior), levels=seq_along(response_levels), labels=response_levels) 
  # obtaining class of observations based on posterior probabilites 
  dimnames(posterior) <- list(rownames(x), response_levels)
  return(list(class = cl, posterior = posterior,group_means_classes=group_means_classes,
              cov_mat=cov_mat,log_det=log_det,prior=prior_probs)) # returning list of outcomes from model fitting.
}

## Predict function to obtain reposne for the testing data set

Pred_quad_dis<- function(object,new_x,new_response,...)  # Quadratic discriminant fucntion for model prediction
{
  new_x=as.matrix(new_x) # converting the predictors into the matrix
  n_obs <- nrow(new_x)   # obtaining no.of observations
  p_num <- ncol(new_x)   # obtaining no.of predictors
  new_response <- as.factor(new_response) # Converting the resposne to factor 
  response_levels <- levels(new_response) # obtaining levels of response factor
  response_counts <- as.vector(table(new_response)) # obtaining counts of response
  names(response_counts) <- response_levels     # naming the response counts with factor levels. 
  prior_probs <- response_counts/length(response_levels)  # Obtaining prior probabilities for each class.
  prior_len <- length(prior_probs)   # length of prior probabilities
  x_muK_sigmaK <- matrix(0, n_obs, prior_len) # initialising x_muK_sigmaK
  Log_det <- matrix(0, n_obs, prior_len) # initialising log of determinant for dataset of all classes.
  for (i in 1L:prior_len){
    mean_mat<-matrix(object$group_means_classes[i, ], nrow(new_x),p_num, byrow = TRUE) # mean matrix from model training
    cov_mat<-object$cov_mat[,,i] # covariance matrix from model training
    x_muK_sigmaK[,i]<- diag((new_x - mean_mat) %*% as.matrix(solve(cov_mat)) %*%t((new_x - mean_mat)))
    #calculatig x_muK_sigmaK for class K in the new dataset.
    Log_det[, i] <- object$log_det[i] # log determinant from model training.
  }
  log_prior<-matrix(log(object$prior),n_obs,prior_len, byrow=TRUE) # prior probabilites from model training.
  MAP_bound <-  - 0.5*x_muK_sigmaK- 0.5* Log_det + log_prior  # calculating MAP decision boundary for new data
  posterior <- exp(MAP_bound)/(1+exp(MAP_bound)) # posterior for new data
  cl <- factor(max.col(posterior), levels=seq_along(response_levels), labels=response_levels)
  # obtaining class for observations in the new data set.
  dimnames(posterior) <- list(rownames(new_x), response_levels)
  return(list(class = cl, posterior = posterior)) # returning class and posterior for observations in new data.
}

## Reading Datasets
tr=read.table("C:/Users/Jay/Desktop/STAT 542/Assignment 2/vowel.train.data")
te=read.table("C:/Users/Jay/Desktop/STAT 542/Assignment 2/vowel.test.data")
dim(tr)
dim(te)
# QDA user function fit
x=tr[,-1]
response=tr$y
my_qda_fit<-quad_dis(x,response)
my_qda_tr_pred<-Pred_quad_dis(my_qda_fit,x,response)
conf_mat=confusionMatrix(my_qda_tr_pred$class,response)
cat("QDA implementation train error:",(1-conf_mat$overall[1]))
new_x=te[,-1]
new_response=te$y
my_qda_te_pred<-Pred_quad_dis(my_qda_fit,new_x,new_response)
conf_mat=confusionMatrix(my_qda_te_pred$class,new_response)
cat("QDA implementation test error:",(1-conf_mat$overall[1]))
