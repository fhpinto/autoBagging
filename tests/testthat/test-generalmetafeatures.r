context("general metafeatures")

ext.iris <- ReadDF(iris)

test_that("length n.examples", {
  expect_length(n.examples(ext.iris), 1)
})

test_that("mode in value equal mode x", {
  expect_equal(mode(n.examples(ext.iris)), mode(5))
})
