test_that("lr works", {
  data(Angell)
  expect_equal(lr(moral~hetero, Angell)$sigma, summary(lm(moral~hetero, Angell))$sigma)
  expect_equal(lr(moral~hetero+mobility, Angell, interact = TRUE)$sigma,
               summary(lm(moral~hetero*mobility, Angell))$sigma)
  expect_equal(lr(moral~hetero, Angell, intercept = FALSE)$sigma,
               summary(lm(moral~-1+hetero, Angell))$sigma)
  expect_equal(lr(moral~region, Angell, category = 1)$sigma,
               summary(lm(moral~factor(region), Angell))$sigma)
  expect_equal(lr(moral~region, Angell, category = 2)$sigma,
               summary(lm(moral~-1+factor(region), Angell))$sigma)
})
