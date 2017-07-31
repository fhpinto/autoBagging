setMethod("show",
          signature("abmodel"),
          function(object) {
            cat("Ensemble of bagged trees\n")
            cat("No of trees:", length(object@base_models), ".\n")
            cat("Target variable: ", get_target(object@form), ".\n")

            if (object@dynamic_selection == "none") {
              cat("Without dynamic selection.\n")
            } else {
              cat("With dynamic selection method:",
                  object@dynamic_selection, ".\n")
            }
          })

#' Predicting on new data with a \strong{abmodel} model
#'
#' This is a \code{predict} method for predicting new data points using a
#' \code{abmodel} class object - refering to an ensemble
#' of bagged trees
#'
#' @seealso \code{\link{abmodel-class}} for details about the bagging model;
#'
#' @param object A \strong{abmodel-class} object.
#' @param newdata New data to predict using an \code{abmodel} object
#'
#' @return predictions produced by an \code{abmodel} model.
#'
#' @import party
#'
#' @export
setMethod("predict",
          signature("abmodel"),
          function(object, newdata) {

            switch(object@dynamic_selection,
                "ola" = {
                  cat("Using OLA method to dynamically",
                      "predict new instances...\n")
                  OLA(object@form,
                      object@base_models,
                      object@data,
                      newdata,
                      5)
                  },
                "knora-e" = {
                  cat("Using KNORA-E method to dynamically",
                      "predict new instances...\n")
                  KNORA.E(object@form,
                          object@base_models,
                          object@data,
                          newdata,
                          5)
                  },
                "none" = {
                  cat("Using majority voting method",
                      "to predict new instances...\n")
                  Y_hat <- sapply(object@base_models, predict, newdata)
                  apply(Y_hat, 1, majority_voting)
                  })
          })
