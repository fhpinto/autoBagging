#' majority voting
#'
#' @param x predictions produced by a set of models
#'
#' @export
majority_voting <- function(x) {
  unique.x <- unique(x)
  unique.x[which.max(tabulate(match(x, unique.x)))]
}

normalize01 <- function(x, max_value, min_value) {
	(x - min_value) / (max_value - min_value)
}

#' get target variable
#'
#' get the target variable from a formula
#'
#' @param form formula
#'
#' @export
get_target <- function(form) {
	unlist(strsplit(deparse(form), split = " ", fixed = TRUE), use.names = FALSE)[1]
}

split_by <- function(expr, split, ...) {
  expr <- strsplit(expr, split = split, fixed = FALSE, ...)

  unlist(expr, use.names = FALSE)
}

cleanRecWF <- function(o) {
  exprs <- "nTrees.|pruningMethods.|pruningCutPoint.|dynamicMethods."

  o$meta.pred <- NULL
  o <- as.list(o)
  o_selected <- o[o == 1]
  o_methods <- names(o_selected)

  methods_vals <- lapply(o_methods, function(j) split_by(j, exprs)[2])
  names(methods_vals) <- c("nTrees", "pruningMethods", "pruningCutPoint","dynamicMethods")
  methods_vals$nTrees <- as.numeric(methods_vals$nTrees)

  if (methods_vals$pruningMethods != "none")
    methods_vals$pruningCutPoint <- as.numeric(methods_vals$pruningCutPoint)

  methods_vals
}

catWF <- function(wf) {
  cat("Bagging ensemble of",wf$nTrees,"decision trees.\n\n")
  if (!wf$pruningMethods %in% "none") {
    cat("With a pruning cut of",
        wf$pruningCutPoint,
        "with method",wf$pruningMethods,"\n\n")
  } else {
    cat("Without tree pruning.")
  }

  if (!wf$dynamicMethods %in% "none") {
    cat("Predictions should be produced dynamically with method",
        wf$dynamicMethods,"\n\n")
  } else {
    cat("Predictions should be produced according to majority voting.")
  }
}

range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

Log2 <- function(values) sapply(values, log0)

log0 <- function(x) {
  if (x == 0.0 || is.na(x))
    0.0
  else
    log2(x)
}

gaussian.dist <- function(x,y,w) {
  exp(-sum((x-y)^2)/(2*w^2))
}

create_formula <- function(class, vars) {
  expr <- paste0(class, " ~ ", paste(vars, collapse=" + "))
  form <- eval(parse(text = expr))

  form
}

f.to.one.zero <- function(dat) {
  cbind(dat[,-c(which(sapply(dat, is.factor)))], model.matrix(eval(parse(text=paste("~"," 0 + ",paste(names(which(sapply(dat, is.factor))),collapse=" + "),sep=""))), model.frame(eval(parse(text=paste("~"," 0 + ",paste(names(which(sapply(dat, is.factor))),collapse=" + "),sep=""))),dat,na.action=function(x) x )) )
}

