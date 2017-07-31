#' Overall Local Accuracy
#'
#' A dynamic selection method
#'
#' @param form formula
#' @param mod a list comprising the individual models
#' @param v.data validation data
#' @param t.data test data, with the instances to predict
#' @param k the number of nearest neighbors. Defaults to 5.
OLA <- function (form, mod, v.data, t.data, k = 5) {
  class <- get_target(form)
  predictions <- rep(NA, nrow(t.data))

  pred <- sapply(mod, function(x) predict(x, t.data))

  dyna.instances <- which(lapply(apply(pred,1,unique),length)>1)
  test.instances <- 1:nrow(t.data)
  bag.instances <- setdiff(test.instances, dyna.instances)

  if (length(bag.instances) == 1 ) {
    predictions[bag.instances] <- majority_voting(pred[bag.instances,])
  }

  if (length(bag.instances) > 1 ) {
    predictions[bag.instances] <- apply(pred[bag.instances,], 1, majority_voting)
  }

  pred.val <- sapply(mod, function(x) predict(x, v.data))

  if (length(dyna.instances) != 0 ) {
    for (i in dyna.instances) {
      knn.instances <- order(sapply(1:nrow(v.data),function(x){cluster::daisy(rbind(t.data[i,c(setdiff(colnames(t.data),class))],v.data[x,c(setdiff(colnames(v.data),class))]))}))[1:k]
      cs <- which.max(apply(pred.val[knn.instances,] == v.data[knn.instances,class],2,sum) / length(knn.instances))
      predictions[i] <- pred[i,cs]
    }
  }
  predictions
}

#' K-Nearest-ORAcle-Eliminate
#'
#' A dynamic selection method
#'
#' @inheritParams OLA
KNORA.E <- function(form, mod, v.data, t.data, k = 5) {
  class <- get_target(form)
  pred <- sapply(mod, function(x) predict(x, t.data))

  pred.val <- sapply(mod, function(x) predict(x, v.data))

  predictions <- NULL

  kk <- k
  for (l in 1:nrow(t.data)) {
    k <-  kk
    knor <- 0
    cs <- NA

    knn.instances <- order(sapply(1:nrow(v.data),function(x){cluster::daisy(rbind(t.data[l,c(setdiff(colnames(t.data),class))],v.data[x,c(setdiff(colnames(v.data),class))]))}))[1:k]
    while (sum(knor) == 0 && k != 0 && is.na(cs)) {
      knn.instances.aux <- knn.instances[1:k]

      if (k == 1) {
        knor <- sum(pred.val[knn.instances.aux,] == v.data[knn.instances.aux,class]) == k
        cs <- if (sum(knor)==0) {NA} else {which(knor==TRUE)}
        k <- k-1
      }
      if (k > 1) {
        knor <- apply(pred.val[knn.instances.aux,] == v.data[knn.instances.aux,class],2,sum) == k
        cs <- if (sum(knor)==0) {NA} else {which(knor==TRUE)}
        k <- k-1
      }
    }

    if (is.na(cs)[1]) {
      acc.classifiers <- apply(pred.val[knn.instances,] == v.data[knn.instances,class],2,sum) / length(knn.instances)
      cs <- which(max(acc.classifiers) == acc.classifiers)
    }
    predictions[l] <- majority_voting(pred[l,cs])
  }
  predictions
}
