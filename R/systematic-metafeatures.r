attr.correlation <- function(dataset, data.char) {
  wkContAttrs <- ContAttrs(dataset)
  attr.correlation <- NA

  if (length(wkContAttrs) > 0) {
    cm <- cor(dataset[[2]]$frame[, wkContAttrs], use = "pairwise.complete.obs")
    xx <- rep(1:length(wkContAttrs), times=1:length(wkContAttrs)-1)
    yy <- unlist(sapply(2:length(wkContAttrs)-1, function(x) {1:x}))
    attr.correlation <- sapply(1:length(xx), function(x,m,xx,yy) {m[xx[x],yy[x]]}, cm, xx, yy)
    names(attr.correlation) <- sapply(1:length(xx), function(x, xx, yy) {paste(xx[x], yy[x], sep="_")}, xx, yy)
  }

  attr.correlation[is.na(attr.correlation)] <- 0
  attr.correlation
}

avg.abs.attr.correlation <- function(dataset, data.char) {
  ac <- GetMeasure("attr.correlation", data.char)
  mean(abs(ac[!is.na(ac)]))
}

min_abs.attr.correlation <- function(dataset, data.char) {
  ac <- GetMeasure("attr.correlation", data.char)
  min(abs(ac[! is.na(ac)]))
}

max_abs.attr.correlation <- function(dataset, data.char) {
  ac <- GetMeasure("attr.correlation", data.char)
  max(abs(ac[! is.na(ac)]))
}

sd_abs.attr.correlation <- function(dataset, data.char) {
  ac <- GetMeasure("attr.correlation", data.char)
  sd(abs(ac[! is.na(ac)]))
}

var_abs.attr.correlation <- function(dataset, data.char) {
  ac <- GetMeasure("attr.correlation", data.char)
  var(abs(ac[! is.na(ac)]))
}

hist_attr.correlation <- function(dataset, data.char) {
  x <- na.omit(range01(na.omit(as.numeric(GetMeasure("attr.correlation", data.char)))))
  aux <- NULL

  if (length(x) == 0) {
    aux <- NA
  } else {
    aux <- hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts
  }

  aux
}

hist_attr.correlation.bin1 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[1]
}

hist_attr.correlation.bin2 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[2]
}

hist_attr.correlation.bin3 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[3]
}

hist_attr.correlation.bin4 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[4]
}

hist_attr.correlation.bin5 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[5]
}

hist_attr.correlation.bin6 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[6]
}

hist_attr.correlation.bin7 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[7]
}

hist_attr.correlation.bin8 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[8]
}

hist_attr.correlation.bin9 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[9]
}

hist_attr.correlation.bin10 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.correlation", data.char))[10]
}

class.entropy <- function(dataset, data.char) {
  class.rel.freq <- GetMeasure("class.rel.freq", data.char)

  -sum(as.numeric(subset(class.rel.freq, class.rel.freq!=0)) * Log2(as.numeric(subset(class.rel.freq, class.rel.freq!=0))))
}

avg.attr.entropy <- function(dataset, data.char) {
  mean(as.numeric(GetMeasure("attr.entropy", data.char)))
}

max_attr.entropy <- function(dataset, data.char) {
  max(as.numeric(GetMeasure("attr.entropy", data.char)))
}

min_attr.entropy <- function(dataset, data.char) {
  min(as.numeric(GetMeasure("attr.entropy", data.char)))
}

var_attr.entropy <- function(dataset, data.char) {
  var(as.numeric(GetMeasure("attr.entropy", data.char)))
}

sd_attr.entropy <- function(dataset, data.char) {
  sd(as.numeric(GetMeasure("attr.entropy", data.char)))
}

hist_attr.entropy <- function(dataset, data.char) {
  x <- GetMeasure("attr.entropy", data.char)
  x <- range01(na.omit(as.numeric(x)))

  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

hist_attr.entropy.bin1 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[1]
}

hist_attr.entropy.bin2 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[2]
}

hist_attr.entropy.bin3 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[3]
}

hist_attr.entropy.bin4 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[4]
}

hist_attr.entropy.bin5 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[5]
}

hist_attr.entropy.bin6 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[6]
}

hist_attr.entropy.bin7 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[7]
}

hist_attr.entropy.bin8 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[8]
}

hist_attr.entropy.bin9 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[9]
}

hist_attr.entropy.bin10 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.entropy", data.char))[10]
}


eta.values <- function(dataset, data.char) {
  eta.values <- NULL

  wkContAttrs <- ContAttrs(dataset)
  if (length(wkContAttrs) > 0) {
    for (l in 1:length(wkContAttrs)) {
      anova2 <- aov(dataset[[2]]$frame[,wkContAttrs[l]] ~ dataset[[2]]$frame[,dataset[[1]]$attributes$target.attr])
      eta.values[l] <- lsr::etaSquared(anova2)[1]
    }
  } else {
    eta.values <- NA
  }
  eta.values
}

eta.avg <- function (dataset, data.char) {
  js <- GetMeasure("eta.values", data.char)
  mean(js)
}

eta.max <- function (dataset, data.char) {
  js <- GetMeasure("eta.values", data.char)
  max(js)
}

eta.min <- function (dataset, data.char) {
  js <- GetMeasure("eta.values", data.char)
  min(js)
}

eta.sd <- function (dataset, data.char) {
  js <- GetMeasure("eta.values", data.char)
  sd(js)
}

eta.var <- function (dataset, data.char) {
  js <- GetMeasure("eta.values", data.char)
  var(js)
}

eta.hist <- function (dataset, data.char) {
  js <- GetMeasure("eta.values", data.char)
  x <- na.omit(range01(na.omit(as.numeric(js))))
  aux <- NULL

  if (length(x) == 0) {
    aux <- NA
  } else {
    if (var(x) != 0) {
      aux <- hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts
    } else {
      aux <- hist(x, breaks=10, plot=FALSE)$counts
    }
  }
  aux
}

eta.hist1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[1]
}

eta.hist2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[2]
}

eta.hist3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[3]
}

eta.hist4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[4]
}

eta.hist5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[5]
}

eta.hist6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[6]
}

eta.hist7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[7]
}

eta.hist8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[8]
}

eta.hist9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[9]
}

eta.hist10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.hist", data.char))[10]
}

eta.pair.values <- function(dataset, data.char) {
  eta.values <- NULL

  wkContAttrs <- ContAttrs(dataset)
  wkSymbAttrs <- SymbAttrs(dataset)
  if (length(wkContAttrs) > 0 && length(wkSymbAttrs) > 0) {
    for (l in 1:length(wkContAttrs)) {
      for (k in 1:length(wkSymbAttrs)) {
        anova2 = tryCatch({
          aov(dataset[[2]]$frame[complete.cases(dataset[[2]]$frame),wkContAttrs[l]] ~ dataset[[2]]$frame[complete.cases(dataset[[2]]$frame),wkSymbAttrs[k]])
        }, error = function(e) {
          0
        })
        if (length(anova2) == 1) {
          eta.values <- c(eta.values, 0)
        } else {
          eta.values <- c(eta.values, lsr::etaSquared(anova2)[1])
        }
      }
    }
  } else {
    eta.values <- NA
  }
  eta.values
}

eta.pair.avg <- function (dataset, data.char) {
  js <- GetMeasure("eta.pair.values", data.char)
  mean(js)
}

eta.pair.max <- function (dataset, data.char) {
  js <- GetMeasure("eta.pair.values", data.char)
  max(js)
}

eta.pair.min <- function (dataset, data.char) {
  js <- GetMeasure("eta.pair.values", data.char)
  min(js)
}

eta.pair.sd <- function (dataset, data.char) {
  js <- GetMeasure("eta.pair.values", data.char)
  sd(js)
}

eta.pair.var <- function (dataset, data.char) {
  js <- GetMeasure("eta.pair.values", data.char)
  var(js)
}

eta.pair.hist <- function (dataset, data.char) {
  js <- GetMeasure("eta.pair.values", data.char)
  x <- na.omit(range01(na.omit(as.numeric(js))))
  aux <- NULL

  if (length(x) == 0) {
    aux <- NA
  } else {
    if (var(x) != 0) {
      aux <- hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts
    } else {
      aux <- hist(x, breaks=10, plot=FALSE)$counts
    }
  }
  aux
}

eta.pair.hist1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[1]
}

eta.pair.hist2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[2]
}

eta.pair.hist3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[3]
}

eta.pair.hist4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[4]
}

eta.pair.hist5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[5]
}

eta.pair.hist6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[6]
}

eta.pair.hist7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[7]
}

eta.pair.hist8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[8]
}

eta.pair.hist9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[9]
}

eta.pair.hist10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("eta.pair.hist", data.char))[10]
}

gain.ratio.weights <- function (dataset, data.char) {
  d <- dataset[[2]]$frame

  weights <- CORElearn::attrEval(class ~., data=d, estimator="GainRatio")
  weights <- weights[is.finite( (weights * 10) / 10 )]

  weights
}

gain.ratio.weights.avg <- function (dataset, data.char) {
  js <- GetMeasure("gain.ratio.weights", data.char)
  mean(js)
}

gain.ratio.weights.max <- function (dataset, data.char) {
  js <- GetMeasure("gain.ratio.weights", data.char)
  max(js)
}

gain.ratio.weights.min <- function (dataset, data.char) {
  js <- GetMeasure("gain.ratio.weights", data.char)
  min(js)
}

gain.ratio.weights.var <- function (dataset, data.char) {
  js <- GetMeasure("gain.ratio.weights", data.char)
  var(js)
}

gain.ratio.weights.sd <- function (dataset, data.char) {
  js <- GetMeasure("gain.ratio.weights", data.char)
  sd(js)
}

gain.ratio.weights.hist <- function (dataset, data.char) {
  js <- GetMeasure("gain.ratio.weights", data.char)
  x <- range01(na.omit(as.numeric(js)))
  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

gain.ratio.weights.hist1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[1]
}

gain.ratio.weights.hist2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[2]
}

gain.ratio.weights.hist3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[3]
}

gain.ratio.weights.hist4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[4]
}

gain.ratio.weights.hist5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[5]
}

gain.ratio.weights.hist6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[6]
}

gain.ratio.weights.hist7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[7]
}

gain.ratio.weights.hist8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[8]
}

gain.ratio.weights.hist9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[9]
}

gain.ratio.weights.hist10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("gain.ratio.weights.hist", data.char))[10]
}

attr.trio.interinfo <- function(dataset, data.char) {
  wkSymbAttrs <- SymbAttrs(dataset)
  interinfo <- NULL

  if (length(wkSymbAttrs) > 2) {
    trios <- combn(wkSymbAttrs,3)
    interinfo <- apply(trios,2,function(x){
      infotheo::interinformation(dataset[[2]]$frame[,x])
    })
  } else {
    interinfo <- NA
  }
  interinfo
}

avg.attr.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.trio.interinfo", data.char)
  mean(ac)
}

min_attr.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.trio.interinfo", data.char)
  min(ac)
}

max_attr.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.trio.interinfo", data.char)
  max(ac)
}

var_attr.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.trio.interinfo", data.char)
  var(ac)
}

sd_attr.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.trio.interinfo", data.char)
  sd(ac)
}

hist_attr.trio.interinfo <- function(dataset, data.char) {
  if (length(na.omit(GetMeasure("attr.trio.interinfo", data.char))) == 0) {
    return(NA)
  } else {
    return(hist(as.numeric(GetMeasure("attr.trio.interinfo", data.char)), breaks=10, plot=F)$counts)
  }
}

hist_attr.trio.interinfo.bin1 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[1]
}

hist_attr.trio.interinfo.bin2 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[2]
}

hist_attr.trio.interinfo.bin3 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[3]
}

hist_attr.trio.interinfo.bin4 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[4]
}

hist_attr.trio.interinfo.bin5 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[5]
}

hist_attr.trio.interinfo.bin6 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[6]
}

hist_attr.trio.interinfo.bin7 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[7]
}

hist_attr.trio.interinfo.bin8 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[8]
}

hist_attr.trio.interinfo.bin9 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[9]
}

hist_attr.trio.interinfo.bin10 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.trio.interinfo", data.char))[10]
}

attr.class.trio.interinfo <- function(dataset, data.char) {
  wkSymbAttrs <- SymbAttrs(dataset)
  interinfo <- NULL

  if (length(wkSymbAttrs) > 2) {
    trios <- combn(wkSymbAttrs,2)
    trios <- rbind(trios, rep("class", ncol(trios)))

    interinfo <- apply(trios,2,function(x){
      infotheo::interinformation(dataset[[2]]$frame[,x])
    })
  } else {
    interinfo <- NA
  }
  interinfo
}

avg.attr.class.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.class.trio.interinfo", data.char)
  mean(ac)
}

min_attr.class.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.class.trio.interinfo", data.char)
  min(ac)
}

max_attr.class.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.class.trio.interinfo", data.char)
  max(ac)
}

var_attr.class.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.class.trio.interinfo", data.char)
  var(ac)
}

sd_attr.class.trio.interinfo <- function(dataset, data.char) {
  ac <- GetMeasure("attr.class.trio.interinfo", data.char)
  sd(ac)
}

hist_attr.class.trio.interinfo <- function(dataset, data.char) {
  if (length(na.omit(GetMeasure("attr.class.trio.interinfo", data.char))) == 0) {
    return(NA)
  } else {
    return(hist(as.numeric(GetMeasure("attr.class.trio.interinfo", data.char)), breaks=10, plot=F)$counts)
  }
}

hist_attr.class.trio.interinfo.bin1 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[1]
}

hist_attr.class.trio.interinfo.bin2 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[2]
}

hist_attr.class.trio.interinfo.bin3 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[3]
}

hist_attr.class.trio.interinfo.bin4 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[4]
}

hist_attr.class.trio.interinfo.bin5 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[5]
}

hist_attr.class.trio.interinfo.bin6 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[6]
}

hist_attr.class.trio.interinfo.bin7 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[7]
}

hist_attr.class.trio.interinfo.bin8 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[8]
}

hist_attr.class.trio.interinfo.bin9 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[9]
}

hist_attr.class.trio.interinfo.bin10 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.class.trio.interinfo", data.char))[10]
}


attr.mic <- function(dataset, data.char) {
  wkContAttrs <- ContAttrs(dataset)
  attr.mic <- NA
  if (length(wkContAttrs) > 1) {
    cm <- minerva::mine(dataset[[2]]$frame[complete.cases(dataset[[2]]$frame[,wkContAttrs]), wkContAttrs])$MIC
    attr.mic <- cm[upper.tri(cm, diag=F)]
  }
  attr.mic[is.na(attr.mic)] <- 0
  attr.mic
}

avg.attr.mic <- function(dataset, data.char) {
  ac <- GetMeasure("attr.mic", data.char)
  mean(ac[! is.na(ac)])
}

min_attr.mic <- function(dataset, data.char) {
  ac <- GetMeasure("attr.mic", data.char)
  min(ac[! is.na(ac)])
}

max_attr.mic <- function(dataset, data.char) {
  ac <- GetMeasure("attr.mic", data.char)
  max(ac[! is.na(ac)])
}

var_attr.mic <- function(dataset, data.char) {
  ac <- GetMeasure("attr.mic", data.char)
  var(ac[! is.na(ac)])
}

sd_attr.mic <- function(dataset, data.char) {
  ac <- GetMeasure("attr.mic", data.char)
  sd(ac[! is.na(ac)])
}

hist_attr.mic <- function(dataset, data.char) {
  x <- GetMeasure("attr.mic", data.char)
  x <- range01(na.omit(as.numeric(x)))

  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

hist_attr.mic.bin1 <- function(dataset, data.char) {
  as.vector(GetMeasure("hist_attr.mic", data.char))[1]
}

hist_attr.mic.bin2 <- function(dataset, data.char) {
  as.vector(GetMeasure("hist_attr.mic", data.char))[2]
}

hist_attr.mic.bin3 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[3]
}

hist_attr.mic.bin4 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[4]
}

hist_attr.mic.bin5 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[5]
}

hist_attr.mic.bin6 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[6]
}

hist_attr.mic.bin7 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[7]
}

hist_attr.mic.bin8 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[8]
}

hist_attr.mic.bin9 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[9]
}

hist_attr.mic.bin10 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_attr.mic", data.char))[10]
}


avg.symb.pair.mutual.information <- function(dataset, data.char) {
  spmi <- GetMeasure("symb.pair.mutual.information", data.char)
  mean(unlist(spmi))
}

res.symb.pair.mutual.information <- function(dataset, data.char) {
  spmi <- GetMeasure("symb.pair.mutual.information", data.char)

  rspmi <- NA
  if (! is.na(spmi)) {
    attr.entr <- GetMeasure("attr.entropy", data.char)
    rspmi <- sapply(names(spmi), function(a1, mi, ae) {
      as.list(sapply(names(mi[[a1]]), function(a2, a1, mi, ae)
      {
        mi[[a1]][[a2]] / min(ae[[a1]], ae[[a2]])
      }, a1, mi, ae))
    }, spmi, attr.entr)
  }
  rspmi
}

avg.res.symb.pair.mutual.information <- function(dataset, data.char) {
  rspmi <- GetMeasure("res.symb.pair.mutual.information", data.char)
  mean(unlist(rspmi))
}

min_res.symb.pair.mutual.information <- function(dataset, data.char) {
  rspmi <- GetMeasure("res.symb.pair.mutual.information", data.char)
  min(unlist(rspmi))
}

max_res.symb.pair.mutual.information <- function(dataset, data.char) {
  rspmi <- GetMeasure("res.symb.pair.mutual.information", data.char)
  max(unlist(rspmi))
}

var_res.symb.pair.mutual.information <- function(dataset, data.char) {
  rspmi <- GetMeasure("res.symb.pair.mutual.information", data.char)
  var(unlist(rspmi))
}

sd_res.symb.pair.mutual.information <- function(dataset, data.char) {
  rspmi <- GetMeasure("res.symb.pair.mutual.information", data.char)
  sd(unlist(rspmi))
}

hist_res.symb.pair.mutual.information <- function(dataset, data.char) {
  x <- unlist(GetMeasure("res.symb.pair.mutual.information", data.char))
  x <- range01(na.omit(as.numeric(x)))

  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

hist_res.symb.pair.mutual.information.bin1 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[1]
}

hist_res.symb.pair.mutual.information.bin2 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[2]
}

hist_res.symb.pair.mutual.information.bin3 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[3]
}

hist_res.symb.pair.mutual.information.bin4 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[4]
}

hist_res.symb.pair.mutual.information.bin5 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[5]
}

hist_res.symb.pair.mutual.information.bin6 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[6]
}

hist_res.symb.pair.mutual.information.bin7 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[7]
}

hist_res.symb.pair.mutual.information.bin8 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[8]
}

hist_res.symb.pair.mutual.information.bin9 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[9]
}

hist_res.symb.pair.mutual.information.bin10 <- function(dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_res.symb.pair.mutual.information", data.char))[10]
}

avg.mutual.information <- function(dataset, data.char) {
  mutual.information <- GetMeasure("mutual.information", data.char)
  mean(as.numeric(mutual.information))
}

max_mutual.information <- function(dataset, data.char) {
  mutual.information <- GetMeasure("mutual.information", data.char)
  max(as.numeric(mutual.information))
}

min_mutual.information <- function(dataset, data.char) {
  mutual.information <- GetMeasure("mutual.information", data.char)
  min(as.numeric(mutual.information))
}

var_mutual.information <- function(dataset, data.char) {
  mutual.information <- GetMeasure("mutual.information", data.char)
  var(as.numeric(mutual.information))
}

sd_mutual.information <- function(dataset, data.char) {
  mutual.information <- GetMeasure("mutual.information", data.char)
  sd(as.numeric(mutual.information))
}


hist_mutual.information <- function (dataset, data.char) {
  x <- na.omit(as.numeric(GetMeasure("mutual.information", data.char)))
  x <- range01(x)

  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x, breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

hist_mutual.information.bin1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[1]
}

hist_mutual.information.bin2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[2]
}

hist_mutual.information.bin3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[3]
}

hist_mutual.information.bin4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[4]
}

hist_mutual.information.bin5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[5]
}

hist_mutual.information.bin6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[6]
}

hist_mutual.information.bin7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[7]
}

hist_mutual.information.bin8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[8]
}

hist_mutual.information.bin9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[9]
}

hist_mutual.information.bin10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("hist_mutual.information", data.char))[10]
}

r_value <- function(dataset, data.char) {
  k_overlap <- 7
  theta_overlap <- 4

  data_noclass <- dataset[[2]]$frame[, -ncol(dataset[[2]]$frame)]
  dist_mat <- as.matrix(dist(data_noclass))

  o.dm <- apply(dist_mat, 1, order)
  kOL.dm <- t(apply(o.dm, 1, function(o) o[2:(k_overlap + 1)]))

  CLASS <- as.character(dataset[[2]]$frame[, ncol(dataset[[2]]$frame)])

  kOL.class <- t(apply(kOL.dm, 1, function(o) CLASS[o]))

  kLOL.class <- vapply(seq_len(nrow(kOL.dm)), function(o) {
    CLASS[kOL.dm[o, ]] != CLASS[o]
  }, logical(k_overlap))

  colSums(kLOL.class) >= theta_overlap
}

r_value <- function (dataset, data.char) {
  k_overlap <- 7
  theta_overlap <- 4

  data_noclass <- dataset[[2]]$frame[,-ncol(dataset[[2]]$frame)]
  dist_mat <- dist(data_noclass)

  overlapping <- apply(
    t(apply(
      t(apply(
        apply(as.matrix(dist_mat), 1, order)
        , 1, function(x){x[2:(k_overlap+1)]}))
      , 1, function(x){dataset[[2]]$frame[x,ncol(dataset[[2]]$frame)]})) != dataset[[2]]$frame[,ncol(dataset[[2]]$frame)]
    , 1, function(x) {sum(x) >= theta_overlap }
  )
  overlapping
}


# r_value <- function(dataset, data.char) {
#   k_overlap <- 7
#   theta_overlap <- 4
#
#   data_noclass <- dataset[[2]]$frame[, -ncol(dataset[[2]]$frame)]
#   dist_mat <- as.matrix(dist(data_noclass))
#
#   o.dm <- apply(dist_mat, 1, order)
#   kOL.dm <- t(apply(o.dm, 1, function(o) o[2:(k_overlap + 1)]))
#
#   CLASS <- as.character(dataset[[2]]$frame[, ncol(dataset[[2]]$frame)])
#
#   kOL.class <- t(apply(kOL.dm, 1, function(o) CLASS[o]))
#
#   kLOL.class <- vapply(seq_len(nrow(kOL.dm)), function(o) {
#     CLASS[kOL.dm[o, ]] != CLASS[o]
#   }, logical(k_overlap))
#
#   colSums(kLOL.class) >= theta_overlap
# }



avg.r_value <- function(dataset, data.char) {
  r.values <- GetMeasure("r_value", data.char)

  mean(r.values)
}

max_r_value <- function(dataset, data.char) {
  r.values <- GetMeasure("r_value", data.char)

  max(r.values)
}

min_r_value <- function(dataset, data.char) {
  r.values <- GetMeasure("r_value", data.char)

  min(r.values)
}

var_r_value <- function(dataset, data.char) {
  r.values <- GetMeasure("r_value", data.char)

  var(r.values)
}

sd_r_value <- function(dataset, data.char) {
  r.values <- GetMeasure("r_value", data.char)

  sd(r.values)
}

r_value.hist <- function (dataset, data.char) {
  r.values <- GetMeasure("r_value", data.char)
  x <- range01(na.omit(as.numeric(r.values)))
  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

r_value.hist1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[1]
}

r_value.hist2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[2]
}

r_value.hist3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[3]
}

r_value.hist4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[4]
}

r_value.hist5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[5]
}

r_value.hist6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[6]
}

r_value.hist7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[7]
}

r_value.hist8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[8]
}

r_value.hist9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[9]
}

r_value.hist10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("r_value.hist", data.char))[10]
}


relieff.weights <- function (dataset, data.char) {
  d <- dataset[[2]]$frame

  weights <- CORElearn::attrEval(class ~., data=d, estimator="ReliefFexpRank", ReliefIterations=30)
  weights <- weights[is.finite( (weights * 10) / 10 )]

  weights
}

relieff.weights.avg <- function (dataset, data.char) {
  js <- GetMeasure("relieff.weights", data.char)

  mean(js)
}

relieff.weights.max <- function (dataset, data.char) {
  js <- GetMeasure("relieff.weights", data.char)
  max(js)
}

relieff.weights.min <- function (dataset, data.char) {
  js <- GetMeasure("relieff.weights", data.char)
  return(  min(js)  )
}

relieff.weights.var <- function (dataset, data.char) {
  js <- GetMeasure("relieff.weights", data.char)
  var(js)
}

relieff.weights.sd <- function (dataset, data.char) {
  js <- GetMeasure("relieff.weights", data.char)
  sd(js)
}

relieff.weights.hist <- function (dataset, data.char) {
  js <- GetMeasure("relieff.weights", data.char)
  x <- range01(na.omit(as.numeric(js)))
  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

relieff.weights.hist1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[1]
}

relieff.weights.hist2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[2]
}

relieff.weights.hist3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[3]
}

relieff.weights.hist4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[4]
}

relieff.weights.hist5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[5]
}

relieff.weights.hist6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[6]
}

relieff.weights.hist7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[7]
}

relieff.weights.hist8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[8]
}

relieff.weights.hist9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[9]
}

relieff.weights.hist10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("relieff.weights.hist", data.char))[10]
}

skewness.values <- function(dataset, data.char) {
  wkContAttrs <- ContAttrs(dataset)
  s <- NULL

  if (length(wkContAttrs) > 0) {
    for (l in 1:length(wkContAttrs)) {
      s[l] <- e1071::skewness(dataset[[2]]$frame[,wkContAttrs[l]])
    }
  } else {
    s <- NA
  }
  s
}

avg.skewness <- function(dataset, data.char) {
  mean(GetMeasure("skewness.values", data.char))
}

avg.abs.skewness <- function(dataset, data.char) {
  mean(abs(GetMeasure("skewness.values", data.char)))
}

max_abs.skewness <- function(dataset, data.char) {
  max(abs(GetMeasure("skewness.values", data.char)))
}

min_abs.skewness <- function(dataset, data.char) {
  min(abs(GetMeasure("skewness.values", data.char)))
}

sd_abs.skewness <- function(dataset, data.char) {
  sd(abs(GetMeasure("skewness.values", data.char)))
}

var_abs.skewness <- function(dataset, data.char) {
  var(abs(GetMeasure("skewness.values", data.char)))
}

skewness.hist <- function (dataset, data.char) {
  js <- GetMeasure("skewness.values", data.char)
  x <- range01(na.omit(as.numeric(js)))
  if (length(na.omit(x)) == 0) {
    return(NA)
  } else {
    return(hist(x,breaks=seq(from=min(x), to=max(x), by=(max(x)-min(x))/10), plot = FALSE)$counts)
  }
}

skewness.hist1 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[1]
}

skewness.hist2 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[2]
}

skewness.hist3 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[3]
}

skewness.hist4 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[4]
}

skewness.hist5 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[5]
}

skewness.hist6 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[6]
}

skewness.hist7 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[7]
}

skewness.hist8 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[8]
}

skewness.hist9 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[9]
}

skewness.hist10 <- function (dataset, data.char) {
  bin <- as.vector(GetMeasure("skewness.hist", data.char))[10]
}
