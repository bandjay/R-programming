### Naive Bayes Implementation #######

trainNB = function(target, data, threshold=0.001) {
  n = nrow(data)
  y = data[,target]
  classes = levels(y)
  k = length(classes)
  featnames = setdiff(colnames(data), target)
  feats = data[, featnames]
  
  # prior calculation
  priors = setNames(numeric(k), classes)
  probs = vector("list", k)
  
    # loop thru classes
  for (cl in classes) {
    # subset to class
    feats2 = feats[y == cl,]
    priors[cl] = nrow(feats2) / n
    # for each feature calculate proportions of levels
    # NAs are disregarded automatically
    probs[[cl]] = lapply(feats2, function(x) {
      p = prop.table(table(x))
      p = setNames(as.numeric(p), names(p))
      # if we have some cells with 0 entries replace them by threshold
      ifelse(p == 0, threshold, p)
    })
  }
  list(target=target, classes=classes, priors=priors, probs=probs)
}



predictNB = function(model, newdata) {
  n = nrow(newdata)
  newdata[, model$target] = NULL
  cls = model$classes
  # result object
  pred = matrix(NA, nrow=n, ncol=length(cls))
  colnames(pred) = cls
  # loop thru new observations
  for (i in 1:nrow(newdata)) {
    # feature vector as char vector
    x = sapply(newdata[i,], as.character)
    # loop thru classes and get prob for each class
    for (cl in cls) {
      # index our prob tables
      p = model$probs[[cl]]
      q = sapply(names(x), function(n) p[[n]][x[n]])
      # multiply probs and prior prob
      pred[i, cl] = prod(q, na.rm=TRUE) * model$priors[[cl]]
    }
  }
  # normalize
  pred = t(apply(pred, 1, function(x) x/sum(x)))
  return(pred)
}

# test method on HouseVotes (in mlbench package)
data(HouseVotes84, package = "mlbench")
HV=HouseVotes84

m1 = trainNB("Class", HV)
p1 = predictNB(m1, HV)

