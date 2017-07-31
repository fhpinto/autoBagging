#' bagging method
#'
#' @param form formula
#'
#' @param data training data
#'
#' @param ntrees ntrees
#'
#' @param pruning model pruning method. A character vector. Currently, the
#' following methods are supported:
#' \describe{
#'    \item{mdsq}{Margin-distance minimisation}
#'    \item{bb}{boosting based pruning}
#'    \item{none}{no pruning}
#' }
#'
#' @param dselection dynamic selection of the available models. Currently, the
#' following methods are supported:
#' \describe{
#'    \item{ola}{Overall Local Accuracy}
#'    \item{knora-e}{K-nearest-oracles-eliminate}
#'    \item{none}{no dynamic selection. Majority voting is used.}
#' }
#'
#' @param pruning_cp The pruning cutpoint for the \code{pruning} method
#' picked.
#'
#' @seealso \code{\link{baggedtrees}} for the implementation of the bagging model.
#'
#' @examples
#' # splitting an example dataset into train/test:
#' train <- iris[1:(.7*nrow(iris)), ]
#' test <- iris[-c(1:(.7*nrow(iris))), ]
#' form <- Species ~.
#' # a user-defined bagging workflow
#' m <- bagging(form, iris, ntrees = 5, pruning = "bb", pruning_cp = .5, dselection = "ola")
#' preds <- predict(m, test)
#' # a standard bagging workflow with 5 trees (5 trees for examplification purposes):
#' m2 <- bagging(form, iris, ntrees = 5, pruning = "none", dselection = "none")
#' preds2 <- predict(m2, test)
#'
#' @export
bagging <- function(form, data, ntrees, pruning, dselection, pruning_cp) {
  if (!pruning %in% c("mdsq", "bb", "none"))
    stop("Unknown pruning method.
         Type '?bagging' in your console
         to check the available methods", call. = FALSE)

  if (pruning != "none" & missing(pruning_cp))
    stop("Choose a pruning cutpoint by setting
         the 'pruning_cp' parameter.", call. = FALSE)

  if (!dselection %in% c("ola", "knora-e", "none"))
    stop("Unknown dynamic selection method.
         Type '?bagging' in your console
         to check the available methods", call. = FALSE)


  cat("Training an ensemble of", ntrees, "decision trees\n")
  BT <- baggedtrees(form, data, ntrees)

  # pruning dos modelos
  if (pruning != "none") {
    cat("Pruning the ensemble using method", pruning, "\n")
    Y_hat.tr <- sapply(BT, predict, data)
    Y_hat.tr1<<-Y_hat.tr
    data1<<-data
    IDs <- switch(pruning,
                  "bb" = {
                    bb(form, Y_hat.tr, data, pruning_cp)
                  },
                  "mdsq" = {
                    mdsq(form, Y_hat.tr, data, pruning_cp)
                  })
    BT <- BT[IDs]
    cat("Returning a pruned ensemble with", length(BT), "models\n")
  } else {
    cat("Returning the ensemble without pruning\n")
  }

  abmodel(BT, form, data, dselection)
}

#' bagged trees models
#'
#' The standard resampling with replacement (bootstrap) is used
#' as sampling strategy.
#'
#' @param form formula
#'
#' @param data training data
#'
#' @param ntree no of trees
#'
#' @examples
#' ensemble <- baggedtrees(Species ~., iris, ntree = 50)
#'
#' @export
baggedtrees <- function(form, data, ntree = 100) {
  n <- nrow(data)

  BT <- lapply(seq_len(ntree), function(o) {
    bs <- sample(n, n, replace = TRUE)
    do.call(party::ctree, list(form, data[bs, ]))
  })
  names(BT) <- paste0("M", seq_along(BT))

  BT
}
