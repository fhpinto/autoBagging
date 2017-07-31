#' Boosting-based pruning of models
#'
#' @param form formula
#' @param preds predictions in training data
#' @param data training data
#' @param cutPoint ratio of the total number of models to cut off
bb <- function (form, preds, data, cutPoint) {
  class <- get_target(form)
  prunedN <- ceiling(ncol(preds) - (ncol(preds) * cutPoint))
  weights <- rep(1/nrow(data), nrow(data))
  ordem <- NULL

  for (l in 1:prunedN) {
    errors <- apply(preds, 2, function(x) {sum(((!(x == data[,class])) * 1) * weights)})

    # hammer time! works fine, though
    errors[ordem] <- max(errors) * 2

    ordem[l] <- which.min(errors)

    errorU <- min(errors)
    predU <- preds[,ordem[l]] == data[,class]

    if (errorU > 0.5) {

      weights <- rep(1/nrow(data), nrow(data))

    } else {

      for (w in 1:length(weights)) {
        weights[w] <- ifelse(predU[w], weights[w] / (2*errorU) , weights[w] / (2 * (1-errorU)) )
      }
    }
    weights <- sapply(weights, function(x) {
	  ifelse(x > 10.000e+300, 10.000e+300, x)
    })
  }
  return(ordem)
}


#' Margin Distance Minimization
#'
#' @inheritParams bb
mdsq <- function (form, preds, data, cutPoint) {
  class <- get_target(form)

  prunedN <- ceiling(ncol(preds) - (ncol(preds) * cutPoint))

  ordem <- as.vector(NULL)

  pred <- ifelse(preds == data[,class], 1, -1)
  ens <- rep(0, length(data[,class]))
  colnames(pred) <- 1:ncol(pred)
  o <- rep(0.075, length(data[,class]))

  for (l in 1:prunedN) {
    dist <- apply(as.matrix(pred), 2, function (x) {sqrt( sum( ( ((x+as.vector(ens))/l) - o )^2 ) )})
    ens <- as.matrix(ens) + as.matrix(pred[,c(names(which.min(dist)))])
    pred <- as.matrix(pred[,setdiff(colnames(pred),names(which.min(dist)))])
    ordem[l] <- as.integer(names(which.min(dist)))
  }
  return(ordem)
}
