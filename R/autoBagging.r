#' autoBagging: A package for learning to rank
#' bagging workflows with metalearning
#'
#' Machine Learning (ML) has been successfully applied to a
#' wide range of domains and applications. One of the techniques
#' behind most of these successful applications is Ensemble Learning (EL),
#' the field of ML that gave birth to methods such as Random Forests
#' or Boosting. The complexity of applying these techniques together
#' with the market scarcity on ML experts, has created the need for
#' systems that enable a fast and easy drop-in replacement for ML libraries.
#' Automated machine learning (autoML) is the field of ML that attempts
#' to answers these needs. Typically, these systems rely on optimization
#' techniques such as bayesian optimization to lead the search for the
#' best model. Our approach differs from these systems by making use of
#' the most recent advances on metalearning and a learning to rank
#' approach to learn from metadata. We propose autoBagging, an autoML
#' system that automatically ranks 63 bagging workflows by exploiting
#' past performance and dataset characterization. Results on 140
#' classification datasets from the OpenML platform show that autoBagging
#' can yield better performance than the Average Rank method and achieve
#' results that are not statistically different from an ideal model that
#' systematically selects the best workflow for each dataset.
#'
#' The underlying model leverages the performance of the workflows
#' in historical data. It ranks and recommends workflows for a given
#' classification task. A bagging workflow is comprised by the following steps:
#' \describe{
#'   \item{generation}{the number of trees to grow}
#'   \item{pruning}{the pruning of low performing trees in the ensemble}
#'   \item{pruning cut-point}{a parameter of the previous step}
#'   \item{dynamic selection}{the dynamic selection method used
#'   to aggregate predictions. If none is recommended, majority voting
#'   is used.}
#' }
#'
#' @docType package
#' @name autoBagging
NULL
