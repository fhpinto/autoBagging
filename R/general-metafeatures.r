n.examples <- function(dataset, data.char) {
  nrow(dataset[[2]]$frame)
}

log.n.examples <- function(dataset, data.char) log10(n.examples(dataset, data.char))

n.attrs <- function(dataset, data.char) {
  ncol(dataset[[2]]$frame) - 1
}

n.examples.rel.n.attrs <- function(dataset, data.char) {
  nrow(dataset[[2]]$frame) / (ncol(dataset[[2]]$frame) - 1)
}

log.n.examples.rel.n.attrs <- function(dataset, data.char) {
  log10(n.examples.rel.n.attrs(dataset, data.char))
}

n.continuous.attrs <- function(dataset, data.char) {
  length(ContAttrs(dataset))
}

n.symbolic.attrs <- function(dataset, data.char) {
  length(SymbAttrs(dataset))
}

n.binary.attrs <- function(dataset, data.char) {
  sa <- SymbAttrs(dataset)
  nvals <- sapply(dataset[[1]]$attributes$attr.type[sa], length)
  length(nvals[nvals == 2])
}

prop.continuous.attrs <- function(dataset, data.char) {
  GetMeasure("n.continuous.attrs", data.char) / GetMeasure("n.attrs", data.char)
}

prop.symbolic.attrs <- function(dataset, data.char) {
  GetMeasure("n.symbolic.attrs", data.char) / GetMeasure("n.attrs", data.char)
}

prop.binary.attrs <- function(dataset, data.char) {
  GetMeasure("n.binary.attrs", data.char) / GetMeasure("n.attrs", data.char)
}

symb.value.freq <- function(dataset, data.char) {
  n.examples <- GetMeasure("n.examples", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 0) {
    value.freq <- list()
    for(wkAttr in wkSymbAttrs) {
      # Calculate frequencies of each attribute
      value.freq[[wkAttr]] <- table(dataset[[2]]$frame[ , wkAttr])[dataset[[1]]$attributes$attr.type[[wkAttr]]] / n.examples
    }
  } else {
    value.freq <- NA
  }

  value.freq
}

attr.entropy <- function(dataset, data.char) {
  symb.value.freq <- GetMeasure("symb.value.freq", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 0)
  {
    attr.entropy <- list()
    for(wkAttr in wkSymbAttrs)
    {
      attr.entropy[[wkAttr]] <- - sum(subset(symb.value.freq[[wkAttr]], symb.value.freq[[wkAttr]] != 0) * Log2(subset(symb.value.freq[[wkAttr]], symb.value.freq[[wkAttr]] != 0)))
    }
  }
  else
    attr.entropy <- NA

  attr.entropy
}

res.attr.entropy <- function(dataset, data.char) {
  symb.value.freq <- GetMeasure("symb.value.freq", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 0)
  {
    attr.entropy <- list()
    for(wkAttr in wkSymbAttrs)
    {
      attr.entropy[[wkAttr]] <- ( - sum(subset(symb.value.freq[[wkAttr]], symb.value.freq[[wkAttr]] != 0) * Log2(subset(symb.value.freq[[wkAttr]], symb.value.freq[[wkAttr]] != 0))) ) / Log2(length(subset(symb.value.freq[[wkAttr]], symb.value.freq[[wkAttr]] != 0)))
    }
  }
  else
    attr.entropy <- NA

  attr.entropy
}

avg.res.attr.entropy <- function(dataset, data.char) {
  mean(as.numeric(GetMeasure("res.attr.entropy", data.char)))
}

symb.pair.value.freq <- function(dataset, data.char) {
  n.examples <- GetMeasure("n.examples", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 1)
  {
    symb.value.pair.freq <- list()
    for(i in 1:(length(wkSymbAttrs) - 1))
    {
      # Identify first attribute values
      wkSymbAttrValues1 <- dataset[[1]]$attributes$attr.type[[wkSymbAttrs[i]]]

      symb.value.pair.freq[[wkSymbAttrs[i]]] <- list()
      for (j in (i+1):length(wkSymbAttrs))
      {
        # Identify second attribute values
        wkSymbAttrValues2 <- dataset[[1]]$attributes$attr.type[[wkSymbAttrs[j]]]

        # Calculate frequencies of each attribute value per class
        symb.value.pair.freq[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]] <- as.table(table(dataset[[2]]$frame[,c(wkSymbAttrs[i], wkSymbAttrs[j])])[wkSymbAttrValues1, wkSymbAttrValues2]) / n.examples

        # Set correct names
        dimnames(symb.value.pair.freq[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]]) <- list(attr1 = wkSymbAttrValues1, attr2 = wkSymbAttrValues2)
      }
    }
  }
  else
    symb.value.pair.freq <- NA

  symb.value.pair.freq
}

symb.pair.mutual.information <- function(dataset, data.char) {
  svf <- GetMeasure("symb.value.freq", data.char)
  spvf <- GetMeasure("symb.pair.value.freq", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 1) {
    spmi <- list()
    for(i in 1:(length(wkSymbAttrs) - 1))
    {
      spmi[[wkSymbAttrs[i]]] <- list()
      for (j in (i+1):length(wkSymbAttrs))
      {
        selected <- (as.vector(spvf[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]]) != 0)
        spmi[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]] <- sum(as.vector(spvf[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]])[selected] * as.vector(Log2(as.vector(spvf[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]])[selected] / (rep(svf[[wkSymbAttrs[i]]], ncol(spvf[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]]))[selected] * rep(svf[[wkSymbAttrs[j]]], each=nrow(spvf[[wkSymbAttrs[i]]][[wkSymbAttrs[j]]]))[selected]))))
      }
    }
  }
  else
    spmi <- NA

  spmi
}

n.classes <- function(dataset, data.char) {
  length(dataset[[1]]$attributes$attr.type[[dataset[[1]]$attributes$target.attr]])
}

class.abs.freq <- function(dataset, data.char) {
  c(table(dataset[[2]]$frame[,dataset[[1]]$attributes$target.attr])[dataset[[1]]$attributes$attr.type[[dataset[[1]]$attributes$target.attr]]])
}

class.rel.freq <- function(dataset, data.char) {
  n.examples <- GetMeasure("n.examples", data.char)

  class.count <- table(dataset[[2]]$frame[,dataset[[1]]$attributes$target.attr])[dataset[[1]]$attributes$attr.type[[dataset[[1]]$attributes$target.attr]]]

  class.freq <- alist()
  class.freq <- as.list(class.count / n.examples)

  class.freq
}

class.symb.value.freq <- function(dataset, data.char) {
  n.examples <- GetMeasure("n.examples", data.char)
  #symb.value.freq <- GetMeasure("symb.value.freq", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 0)
  {
    # Identify class values
    wkClassValues <- dataset[[1]]$attributes$attr.type[[dataset[[1]]$attributes$target.attr]]

    class.symb.value.freq <- list()
    for(wkAttr in wkSymbAttrs)
    {
      # Identify attribute values
      wkSymbAttrValues <- dataset[[1]]$attributes$attr.type[[wkAttr]]

      # Calculate frequencies of each attribute value per class
      class.symb.value.freq[[wkAttr]] <- table(dataset[[2]]$frame[,c(wkAttr, dataset[[1]]$attributes$target.attr)]) /n.examples

      # Set correct names
      #dimnames(class.symb.value.freq[[wkAttr]]) <- list(attr = wkSymbAttrValues, class = wkClassValues)
    }
  }
  else
    class.symb.value.freq <- NA

  class.symb.value.freq
}


mutual.information <- function(dataset, data.char) {
  class.rel.freq <- GetMeasure("class.rel.freq", data.char)
  symb.value.freq <- GetMeasure("symb.value.freq", data.char)
  class.symb.value.freq <- GetMeasure("class.symb.value.freq", data.char)

  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkSymbAttrs) > 0)
  {
    # Identify class values
    wkClassValues <- dataset[[1]]$attributes$attr.type[[dataset[[1]]$attributes$target.attr]]

    mutual.information <- list()
    for(wkAttr in wkSymbAttrs)
    {
      tmp <- 0

      for(wkClass in wkClassValues)
      {
        if (class.rel.freq[[wkClass]] > 0)
          tmp <- tmp + sum(class.symb.value.freq[[wkAttr]][symb.value.freq[[wkAttr]] > 0, wkClass] * Log2(class.symb.value.freq[[wkAttr]][symb.value.freq[[wkAttr]] > 0, wkClass]/(class.rel.freq[[wkClass]] * symb.value.freq[[wkAttr]][symb.value.freq[[wkAttr]] > 0])))
      }
      mutual.information[[wkAttr]] <- tmp
    }
  }
  else
    mutual.information <- NA

  mutual.information
}
