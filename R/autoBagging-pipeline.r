data(sysdata, envir=environment())

#' autoBagging
#'
#' Learning to Rank Bagging Workflows with Metalearning
#'
#' @param form formula. Currently supporting only categorical target
#' variables (classification tasks)
#' @param data training dataset with a categorical target variable
#'
#'
#' @seealso \code{\link{bagging}} for the bagging pipeline with a specific
#' workflow; \code{\link{baggedtrees}} for the bagging implementation;
#' \code{\link{abmodel-class}} for the returning class object.
#'
#' @return an \code{abmodel} class object
#'
#' @references Pinto, F., Cerqueira, V., Soares, C., Mendes-Moreira, J.:
#' "autoBagging: Learning to Rank Bagging Workflows
#' with Metalearning" arXiv preprint arXiv:1706.09367 (2017).
#'
#' @examples
#' \dontrun{
#' # splitting an example dataset into train/test:
#' train <- iris[1:(.7*nrow(iris)), ]
#' test <- iris[-c(1:(.7*nrow(iris))), ]
#' # then apply autoBagging to the train, using the desired formula:
#' # autoBagging will compute metafeatures on the dataset
#' # and apply a pre-trained ranking model to recommend a workflow.
#' model <- autoBagging(Species ~., train)
#' # predictions are produced with the standard predict method
#' preds <- predict(model, test)
#' }
#' @import xgboost
#'
#' @export
autoBagging <- function(form, data) {
  if (!class(data[, get_target(form)]) %in% c("factor", "character")) {
    stop("autoBagging currently only supports classification tasks.
       Check your target variable in the formula provided.", call. = FALSE)
  }

  if (nrow(data) > 100000L)
    warning("Very large datasets are out of the scope
             of the experimental setup
             used to validate autoBagging.
             Check references for further information.",
            call. = FALSE)
  cat("Sit tight, relax and enjoy your coffee. autoBagging is working for you!\n\n")

  id <- sample(1:7, 1)
  if (id == 1) {
    cat("https://www.youtube.com/watch?v=n2_X4VTCoEo\n\n")
  } else if (id == 2) {
    cat("https://www.youtube.com/watch?v=hMr3KtYUCcI\n\n")
  } else if (id == 3) {
    cat("https://www.youtube.com/watch?v=ZzlgJ-SfKYE\n\n")
  } else if (id == 4) {
    cat("https://www.youtube.com/watch?v=2Gn9A-kdsRo\n\n")
  } else if (id == 5) {
    cat("https://www.youtube.com/watch?v=ZDwotNLyz10\n\n")
  } else if (id == 6) {
    cat("https://www.youtube.com/watch?v=g2N0TkfrQhY\n\n")
  } else if (id == 7) {
    cat("https://www.youtube.com/watch?v=gEPmA3USJdI\n\n")
  }

  #load("R/sysdata.rda")
  metafeatures_names <- sysdata$metafeatures_names
  MaxMinMetafeatures <- sysdata$MaxMinMetafeatures
  Xtest <- sysdata$Xtest
  metamodel <- sysdata$metamodel

  meta.example <- meta.dataframe(data, metafeatures_names)

  for (i in colnames(Xtest)[1:143]) {
    max_value <- MaxMinMetafeatures[MaxMinMetafeatures[, 1] == i, 2]
    min_value <- MaxMinMetafeatures[MaxMinMetafeatures[, 1] == i, 3]
    meta.example[[i]] <-  ifelse(is.finite(normalize01(meta.example[[i]],
                                                       max_value,
                                                       min_value)),
                                 normalize01(meta.example[[i]],
                                             max_value,
                                             min_value), -1)

    Xtest[,i] <- meta.example[[i]]
  }

  X <- xgboost::xgb.DMatrix(data = data.matrix(Xtest),  group = c(63), missing = -1)

  meta.model <- xgboost::xgb.load(metamodel)

  meta.pred <- cbind(as.data.frame(Xtest), predict(meta.model, X))
  colnames(meta.pred)[ncol(meta.pred)] <- "meta.pred"

  cat("Your recommended workflow is...\n\n")
  RecWF <- cleanRecWF(meta.pred[which.max(meta.pred$meta.pred),
                                (ncol(meta.pred)-13):ncol(meta.pred)])

  catWF(RecWF)
  cat('################################\n\n')
  cat('Training recommended workflow...\n\n')

  bagging(form = form,
          data = data,
          ntrees = RecWF$nTrees,
          pruning = RecWF$pruningMethods,
          dselection = RecWF$dynamicMethods,
          pruning_cp = RecWF$pruningCutPoint)
}
