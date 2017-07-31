#' sysdata
#'
#' Meta data needed to run the \strong{autoBagging} method.
#'
#' @format a list comprising the following information
#' \describe{
#'   \item{avgRankMatrix}{the average rank data regarding each bagging
#'   workflow}
#'   \item{workflows}{metadata on the bagging workflows}
#'   \item{MaxMinMetafeatures}{range data on each metafeature}
#'   \item{metafeatures}{names and values of each metafeatures used
#'   to describe the datasets}
#'   \item{metamodel}{the xgboost ranking metamodel}
#' }
"sysdata"
