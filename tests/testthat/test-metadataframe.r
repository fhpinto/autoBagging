context("meta data frame")

data(sysdata, envir=environment())

test_that("length output list", {
  metaf <- meta.dataframe(iris, sysdata$metafeatures_names)

  expect_equal(length(metaf), length(sysdata$metafeatures_names))
})
