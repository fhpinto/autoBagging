#' abmodel-class
#'
#' \strong{abmodel} is an S4 class that contains the ensemble model.
#' Besides the base learning algorithms--\code{base_models} --
#' \strong{abmodel} class contains information about the
#' dynamic selection method to apply in new data.
#'
#' @slot base_models a list of decision tree classifiers
#'
#' @slot form formula
#' @slot data dataset used to train \code{base_models}
#'
#' @slot dynamic_selection the dynamic selection/combination method
#' to use to aggregate predictions. If \code{none}, majority vote is used.
#'
#' @seealso \code{\link{autoBagging}} function for the
#' method of automatic predicting of the best workflows.
#'
#' @export
setClass("abmodel",
         slots = c(base_models = "list",
                   form = "formula",
                   data = "data.frame",
                   dynamic_selection = "character")
)

#' abmodel
#'
#' @param base_models a list of decision tree classifiers
#' @param form formula
#' @param data dataset used to train \code{base_models}
#' @param dynamic_selection the dynamic selection/combination method
#' to use to aggregate predictions. If \code{none}, majority vote is used.
#'
#' @export
abmodel <- function(base_models, form, data, dynamic_selection) {
  if ( !dynamic_selection %in%  c("ola", "knora-e", "none"))
  	stop("Please choose a valid dynamic selection method", call. = FALSE)

  new("abmodel",
      base_models = base_models,
      form = form,
      data = data,
      dynamic_selection = dynamic_selection)
}
