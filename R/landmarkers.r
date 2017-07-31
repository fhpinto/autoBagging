#' Naive Bayes Landmarker
#'
#'
#' @param dataset train data for the landmarker
#' @param data.char dc
#' @param k number of folds in cross-validation for estimating the accuracy
#' of the landmarker
#'
#' @import e1071
nb.landmarker <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  nb.acc <- numeric(k)
  for (e in 1:k) {
    m.nb = naiveBayes(formul, data=dataf[dataf$fold != e,])
    prevs <- predict(m.nb, dataf[dataf$fold == e,], type="class")
    m.conf <- table(dataf[dataf$fold==e, "class"], prevs)
    nb.acc[e] <- sum(diag(m.conf))/sum(m.conf)
  }
  mean(nb.acc)
}

#' dstump.landmarker_d1
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d1 <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=1))
    prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    m.conf <- table(dataf[dataf$fold==e, "class"], prevs)
    ds.acc[e] <- sum(diag(m.conf))/sum(m.conf)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d2
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d2 <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=2))
    prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    m.conf <- table(dataf[dataf$fold==e, "class"], prevs)
    ds.acc[e] <- sum(diag(m.conf)) / sum(m.conf)
  }
  mean(ds.acc)
}


#' dstump.landmarker_d3
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d3 <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=3))
    prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    m.conf <- table(dataf[dataf$fold==e, "class"], prevs)
    ds.acc[e] <- sum(diag(m.conf))/sum(m.conf)
  }
  mean(ds.acc)
}

#' classmajority.landmarker
#'
#' @inheritParams nb.landmarker
classmajority.landmarker <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])

  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- names(which.max(table(dataf$class)))
    prevs <- rep(m.ds, nrow(dataf[dataf$fold == e,]))

    ds.acc[e] <- sum(prevs == dataf[dataf$fold == e, "class"]) / sum(nrow(dataf[dataf$fold == e,]))
  }
  mean(ds.acc)
}

#' nb.landmarker.correlation
#'
#' @inheritParams nb.landmarker
#'
#' @import e1071
nb.landmarker.correlation <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr,
                             dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  nb.acc <- numeric(k)
  for (e in 1:k) {
    m.nb = naiveBayes(formul, data=dataf[dataf$fold != e,])
    prevs <- predict(m.nb, dataf[dataf$fold == e,], type="class")
    truth <- dataf[dataf$fold == e, "class"]
    prevs <- as.numeric(prevs)
    truth <- as.numeric(truth)

    nb.acc[e] <- cor(prevs, truth)
  }
  mean(nb.acc)
}

#' lda.landmarker.correlation
#'
#' @inheritParams nb.landmarker
#'
#' @import MASS
lda.landmarker.correlation <- function (dataset, data.char, k = 10) {
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    dat <- dataf[dataf$fold != e,]
    vars <- dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)]

    new.vars <- vars[-caret::nearZeroVar(dat)]
    if (length(new.vars)==0) {
      formul <- create_formula(dataset[[1]]$attributes$target.attr, vars)
    } else {
      formul <- create_formula(dataset[[1]]$attributes$target.attr, new.vars)
    }
    m.ds <- lda(formul, data=dat)
    prevs <- predict(m.ds, dataf[dataf$fold == e,])$class
    truth <- dataf[dataf$fold == e,c("class")]
    prevs <- as.numeric(prevs)
    truth <- as.numeric(truth)

    ds.acc[e] <- cor(prevs, truth)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d1.correlation
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d1.correlation <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e,c("class")]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=1))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
      truth <- dataf[dataf$fold == e,c("class")]
      prevs <- as.numeric(prevs)
      truth <- as.numeric(truth)
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e,c("class")]), nrow(dataf[dataf$fold == e,]) )
      truth <- dataf[dataf$fold == e,c("class")]
      prevs <- as.numeric(prevs)
      truth <- as.numeric(truth)
    }
    ds.acc[e] <- cor(prevs, truth)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d2.correlation
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d2.correlation <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e,c("class")]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=2))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
      truth <- dataf[dataf$fold == e,c("class")]
      prevs <- as.numeric(prevs)
      truth <- as.numeric(truth)
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e,c("class")]), nrow(dataf[dataf$fold == e,]) )
      truth <- dataf[dataf$fold == e,c("class")]
      prevs <- as.numeric(prevs)
      truth <- as.numeric(truth)
    }
    ds.acc[e] <- cor(prevs, truth)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d3.correlation
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d3.correlation <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e, "class"]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=3))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
      truth <- dataf[dataf$fold == e, "class"]
      prevs <- as.numeric(prevs)
      truth <- as.numeric(truth)
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
      truth <- dataf[dataf$fold == e, "class"]
      prevs <- as.numeric(prevs)
      truth <- as.numeric(truth)
    }
    ds.acc[e] <- cor(prevs, truth)
  }
  mean(ds.acc)
}

#' classmajority.landmarker.correlation
#'
#' @inheritParams nb.landmarker
classmajority.landmarker.correlation <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])

  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- names(which.max(table(dataf$class)))
    prevs <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    truth <- dataf[dataf$fold == e, "class"]
    prevs <- as.numeric(prevs)
    truth <- as.numeric(truth)

    ds.acc[e] <- cor(prevs, truth)
  }
  mean(ds.acc)
}

#' nb.landmarker.entropy
#'
#' @inheritParams nb.landmarker
#'
#' @import e1071
nb.landmarker.entropy <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  nb.acc <- numeric(k)
  for (e in 1:k) {
    m.nb = naiveBayes(formul, data=dataf[dataf$fold != e,])
    prevs <- predict(m.nb, dataf[dataf$fold == e,], type="class")
    nb.acc[e] <- entropy::entropy(table(prevs))
  }
  mean(nb.acc)
}

#' dstump.landmarker_d1.entropy
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d1.entropy <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc = numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e, "class"]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=1))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
    }
    ds.acc[e] <- entropy::entropy(table(prevs))
  }
  mean(ds.acc)
}

#' dstump.landmarker_d2.entropy
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d2.entropy <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e, "class"]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=2))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
    }
    ds.acc[e] <- entropy::entropy(table(prevs))
  }
  mean(ds.acc)
}

#' dstump.landmarker_d3.entropy
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d3.entropy <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e,c("class")]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=3))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
    }
    ds.acc[e] <- entropy::entropy(table(prevs))
  }
  mean(ds.acc)
}

#' classmajority.landmarker.entropy
#'
#' @inheritParams nb.landmarker
classmajority.landmarker.entropy <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])

  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc = numeric(k)
  for (e in 1:k) {
    m.ds <- names(which.max(table(dataf$class)))
    prevs <- rep(m.ds, nrow(dataf[dataf$fold == e,]))
    ds.acc[e] <- entropy::entropy(table(prevs))
  }
  mean(ds.acc)
}

#' nb.landmarker.interinfo
#'
#' @inheritParams nb.landmarker
#'
#' @import e1071
nb.landmarker.interinfo <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  nb.acc <- numeric(k)
  for (e in 1:k) {
    m.nb = naiveBayes(formul, data=dataf[dataf$fold != e,])
    prevs <- predict(m.nb, dataf[dataf$fold == e,], type = "class")
    truth <- dataf[dataf$fold == e, "class"]
    mat <- cbind(prevs, truth)

    m.ds <- names(which.max(table(dataf[dataf$fold != e, "class"])))
    baseline <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    mat <- cbind(mat, baseline)

    nb.acc[e] <- infotheo::interinformation(mat)
  }
  mean(nb.acc)
}

#' dstump.landmarker_d1.interinfo
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d1.interinfo <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)

  for (e in 1:k) {
    m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=1))
    prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    truth <- dataf[dataf$fold == e, "class"]
    mat <- cbind(prevs, truth)

    m.ds <- names(which.max(table(dataf[dataf$fold != e, "class"])))
    baseline <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    mat <- cbind(mat, baseline)

    ds.acc[e] <- infotheo::interinformation(mat)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d2.interinfo
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d2.interinfo <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=2))
    prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    truth <- dataf[dataf$fold == e, "class"]
    mat <- cbind(prevs, truth)

    m.ds <- names(which.max(table(dataf[dataf$fold != e, "class"])))
    baseline <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    mat <- cbind(mat, baseline)

    ds.acc[e] <- infotheo::interinformation(mat)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d3.interinfo
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d3.interinfo <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=3))
    prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
    truth <- dataf[dataf$fold == e, "class"]
    mat <- cbind(prevs, truth)

    m.ds <- names(which.max(table(dataf[dataf$fold != e, "class"])))
    baseline <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    mat <- cbind(mat, baseline)

    ds.acc[e] <- infotheo::interinformation(mat)
  }
  mean(ds.acc)
}

#' classmajority.landmarker.interinfo
#'
#' @inheritParams nb.landmarker
classmajority.landmarker.interinfo <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])

  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc = numeric(k)
  for (e in 1:k) {
    m.ds <- names(which.max(table(dataf[dataf$fold != e,c("class")])))
    prevs <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    truth <- dataf[dataf$fold == e,c("class")]
    mat <- cbind(prevs, truth)

    baseline <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    mat <- cbind(mat, baseline)

    ds.acc[e] <- infotheo::interinformation(mat)
  }
  mean(ds.acc)
}


#' nb.landmarker.mutual.information
#'
#' @inheritParams nb.landmarker
#'
#' @import e1071
nb.landmarker.mutual.information <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  nb.acc = numeric(k)
  for (e in 1:k) {
    m.nb = naiveBayes(formul, data=dataf[dataf$fold != e,])
    prevs <- predict(m.nb, dataf[dataf$fold == e,], type="class")
    truth <- dataf[dataf$fold == e, "class"]
    mat <- rbind(prevs, truth)

    nb.acc[e] <- entropy::mi.empirical(mat)
  }
  mean(nb.acc)
}

#' dstump.landmarker_d1.mutual.information
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d1.mutual.information <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e, "class"]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=1))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
      truth <- dataf[dataf$fold == e, "class"]
      mat <- rbind(prevs, truth)
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
      truth <- dataf[dataf$fold == e, "class"]
      mat <- rbind(prevs, truth)
    }
    ds.acc[e] <- entropy::mi.empirical(mat)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d2.mutual.information
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d2.mutual.information <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e, "class"]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=2))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
      truth <- dataf[dataf$fold == e, "class"]
      mat <- rbind(prevs, truth)
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
      truth <- dataf[dataf$fold == e, "class"]
      mat <- rbind(prevs, truth)
    }
    ds.acc[e] <- entropy::mi.empirical(mat)
  }
  mean(ds.acc)
}

#' dstump.landmarker_d3.mutual.information
#'
#' @inheritParams nb.landmarker
#'
#' @import rpart
dstump.landmarker_d3.mutual.information <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])
  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    if (length(unique(dataf[dataf$fold != e, "class"]))!=1) {
      m.ds <- rpart(formul, data=dataf[dataf$fold != e,], control=rpart.control(maxdepth=3))
      prevs <- predict(m.ds, dataf[dataf$fold == e,], type="class")
      truth <- dataf[dataf$fold == e,c("class")]
      mat <- rbind(prevs, truth)
    } else {
      prevs <- rep(unique(dataf[dataf$fold != e, "class"]), nrow(dataf[dataf$fold == e,]) )
      truth <- dataf[dataf$fold == e, "class"]
      mat <- rbind(prevs, truth)
    }
    ds.acc[e] <- entropy::mi.empirical(mat)
  }
  mean(ds.acc)
}

#' classmajority.landmarker.mutual.information
#'
#' @inheritParams nb.landmarker
classmajority.landmarker.mutual.information <- function (dataset, data.char, k = 10) {
  formul <- create_formula(dataset[[1]]$attributes$target.attr, dataset[[1]]$attributes$attr.name[-length(dataset[[1]]$attributes$attr.name)])

  dataf <- dataset[[2]]$frame
  dataf$fold <- caret::createFolds(dataf$class, k=k, list=F)

  ds.acc <- numeric(k)
  for (e in 1:k) {
    m.ds <- names(which.max(table(dataf$class)))
    prevs <- as.factor(rep(m.ds, nrow(dataf[dataf$fold == e,])))
    truth <- dataf[dataf$fold == e,c("class")]
    mat <- rbind(prevs, truth)

    ds.acc[e] <- entropy::mi.empirical(mat)
  }
  mean(ds.acc)
}
