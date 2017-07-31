data(sysdata, envir=environment())

#' Retrieve the value of a previously computed measure
#'
#' @param inDCName name of data characteristics
#' @param inDCSet set of data characteristics already computed
#' @param component.name name of component (e.g. time or value) to
#' retrieve; if NULL retrieve all.
#'
#' @return simple or structured value
#'
#' @note if measure is not available, stop execution with error
GetMeasure <- function(inDCName, inDCSet, component.name = "value") {
  if (is.null(inDCSet$value[[inDCName]]))
    stop(message = "WARNING: requires uncomputed measure (", inDCName, ")")

  if (is.null(component.name))
    inDCSet[[inDCName]]
  else
    inDCSet[[component.name]][[inDCName]]
}

#' Retrieve names of symbolic attributes (not including the target)
#'
#' @param dataset structure describing the data set, according
#' to \code{read_data.R}
#'
#' @seealso read_data.R
#'
#' @return list of strings
SymbAttrs <- function(dataset) {
  attDF <- dataset[[1]]$attributes

  cond <- (attDF$attr.type != "continuous") & (attDF$attr.name != attDF$target.attr)

  attDF$attr.name[cond]
  # dataset[[1]]$attributes$attr.name[(dataset[[1]]$attributes$attr.type != "continuous") &
  #                                   (dataset[[1]]$attributes$attr.name != dataset[[1]]$attributes$target.attr)]
}

#' Retrieve names of continuous attributes
#'
#' @inheritParams SymbAttrs
#'
#' @seealso read_data.R
#'
#' @return list of strings
ContAttrs <- function(dataset) {
  attDF <- dataset[[1]]$attributes

  cond <- (attDF$attr.type == "continuous") & (attDF$attr.name != attDF$target.attr)

  attDF$attr.name[cond]
  # dataset[[1]]$attributes$attr.name[(dataset[[1]]$attributes$attr.type == "continuous") &
  #                                   (dataset[[1]]$attributes$attr.name != dataset[[1]]$attributes$target.attr)]
}

#' FUNCTION TO TRANSFORM DATA FRAME INTO LIST WITH GSI REQUIREMENTS
#'
#' @param dat data frame
#'
#' @return a list containing components that describe
#' the names (see ReadtAttrsInfo) and the data (see ReadData) files
ReadDF <- function(dat) {
  # Determine attribute types
  wkNamesFile <- lapply(dat, function(attr) {
    if (is.numeric(attr)) {
      "continuous"
    } else if (is.factor(attr)) {
      levels(attr)
    } else {
      "error"
    }
  })

  names(wkNamesFile)[length(wkNamesFile)] <- c("class")

  wkDataset <- alist()
  class(wkDataset) <- "dataset"

  target.attr <- names(wkNamesFile)[length(wkNamesFile)]
  original.attr.name <- names(wkNamesFile)
  attr.name <- names(wkNamesFile)
  attr.type <- wkNamesFile
  problem.type <- "classification"

  wkNamesFile <- list(namesfile = c("datafile"),
                      attributes = list(target.attr = target.attr,
                                        problem.type = problem.type,
                                        attr.name = attr.name,
                                        original.name = original.attr.name,
                                        attr.type = attr.type))

  colnames(dat) <- original.attr.name
  rownames(dat) <- NULL

  return(list(wkNamesFile, list(data.file = "datafile", frame = dat)))
}


CharacterizeDF <- function(df, dc.measures) {
  wkDCSet <- list(value = list())
  wkDataSet <- ReadDF(df)

  for (wkMeasure in dc.measures$measures) {
    if (is.null(wkDCSet$value[[wkMeasure]])) {
      wkValue <- do.call(wkMeasure, list(wkDataSet, wkDCSet))
      wkDCSet$value[[wkMeasure]] <- wkValue
    }
  }
  wkDCSet
}

meta.dataframe <- function(dat, metaf) {
  CDF <- CharacterizeDF(dat, sysdata$kCompleteClassificationGSI)

  metaframe <- vector("list", length(metaf))
  for (i in seq_along(metaf)) {
    metaframe[i] <- CDF$value[grep(metaf[[i]], names(CDF$value))][1]
  }
  names(metaframe) <- metaf

  metaframe
}

