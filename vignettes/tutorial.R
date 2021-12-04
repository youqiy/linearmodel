## ----setup, include=FALSE-----------------------------------------------------
library(linearmodel)

## -----------------------------------------------------------------------------
data("NHANE")
data <- NHANE
head(data)
dim(data)

## -----------------------------------------------------------------------------
lm1.2 <- lr(Age~Weight+Height, data)

## -----------------------------------------------------------------------------
lm1.1 <- lm(Age~Weight+Height, data)

names(lm1.2)
# fitted values
all.equal(as.numeric(lm1.1$fitted.values), as.numeric(lm1.2$y.fitted))
# conventional and internally studendized residuals
all.equal(as.numeric(lm1.1$residuals), as.numeric(lm1.2[["residual"]][, 1]))
all.equal(as.numeric(rstandard(lm1.1)), as.numeric(lm1.2[["residual"]][, 2]))
# coefficients
all.equal(as.numeric(summary(lm1.1)$coefficients), as.numeric(lm1.2$cov.matrix))
# degree of freedom
all.equal(as.numeric(summary(lm1.1)$r.squared), as.numeric(lm1.2$r.squared))
# sigma
all.equal(as.numeric(summary(lm1.1)$sigma), as.numeric(lm1.2$sigma))
# F-test
all.equal(as.numeric(as.matrix(anova(lm(Age~1, data), lm1.1))[2, 5]), as.numeric(lm1.2$f.result[1]))

## -----------------------------------------------------------------------------
system.time({
  lm1.1 <- lm(Age~Weight+Height, data)
  anova(lm(Age~1, data), lm1.1)
  rstandard(lm1.1)
})
system.time(lm1.2 <- lr(Age~Weight+Height, data))

## -----------------------------------------------------------------------------
lm2.2 <- lr(Age~Weight+Height, data, intercept = FALSE)

## -----------------------------------------------------------------------------
lm2.1 <- lm(Age~-1+Weight+Height, data)

names(lm2.2)
# fitted values
all.equal(as.numeric(lm2.1$fitted.values), as.numeric(lm2.2$y.fitted))
# conventional and internally studendized residuals
all.equal(as.numeric(lm2.1$residuals), as.numeric(lm2.2[["residual"]][, 1]))
all.equal(as.numeric(rstandard(lm2.1)), as.numeric(lm2.2[["residual"]][, 2]))
# coefficients
all.equal(as.numeric(summary(lm2.1)$coefficients), as.numeric(lm2.2$cov.matrix))
# degree of freedom
all.equal(as.numeric(summary(lm2.1)$r.squared), as.numeric(lm2.2$r.squared))
# sigma
all.equal(as.numeric(summary(lm2.1)$sigma), as.numeric(lm2.2$sigma))

## -----------------------------------------------------------------------------
system.time({
  lm2.1 <- lm(Age~Weight+Height, data)
  anova(lm2.1)
  rstandard(lm2.1)
})
system.time(lm2.2 <- lr(Age~Weight+Height, data, intercept = FALSE))

## -----------------------------------------------------------------------------
lm3.2 <- lr(Age~Weight+Height, data, interact = TRUE)

## -----------------------------------------------------------------------------
lm3.1 <- lm(Age~Weight*Height, data)

names(lm3.2)
# fitted values
all.equal(as.numeric(lm3.1$fitted.values), as.numeric(lm3.2$y.fitted))
# conventional and internally studendized residuals
all.equal(as.numeric(lm3.1$residuals), as.numeric(lm3.2[["residual"]][, 1]))
all.equal(as.numeric(rstandard(lm3.1)), as.numeric(lm3.2[["residual"]][, 2]))
# coefficients
all.equal(as.numeric(summary(lm3.1)$coefficients), as.numeric(lm3.2$cov.matrix))
# degree of freedom
all.equal(as.numeric(summary(lm3.1)$r.squared), as.numeric(lm3.2$r.squared))
# sigma
all.equal(as.numeric(summary(lm3.1)$sigma), as.numeric(lm3.2$sigma))
# F-test
all.equal(as.numeric(as.matrix(anova(lm(Age~1, data), lm3.1))[2, 5]), as.numeric(lm3.2$f.result[1]))

## -----------------------------------------------------------------------------
system.time({
  lm3.1 <- lm(Age~Weight*Height, data)
  anova(lm(Age~1, data), lm3.1)
  rstandard(lm3.1)
})
system.time(lm3.2 <- lr(Age~Weight+Height, data, interact = TRUE))

## -----------------------------------------------------------------------------
lm4.2 <- lr(Age~Race1, data, category = 1)

## -----------------------------------------------------------------------------
lm4.1 <- lm(Age~factor(Race1), data)

names(lm4.2)
# fitted values
all.equal(as.numeric(lm4.1$fitted.values), as.numeric(lm4.2$y.fitted))
# conventional and internally studendized residuals
all.equal(as.numeric(lm4.1$residuals), as.numeric(lm4.2[["residual"]][, 1]))
all.equal(as.numeric(rstandard(lm4.1)), as.numeric(lm4.2[["residual"]][, 2]))
# coefficients (Note that the order of coefficients is different)
all.equal(as.numeric(summary(lm4.1)$coefficients[c(1:3), ]), as.numeric(lm4.2$cov.matrix[c(1:3), ]))
all.equal(as.numeric(summary(lm4.1)$coefficients[4, ]), as.numeric(lm4.2$cov.matrix[5, ]))
all.equal(as.numeric(summary(lm4.1)$coefficients[5, ]), as.numeric(lm4.2$cov.matrix[4, ]))
# degree of freedom
all.equal(as.numeric(summary(lm4.1)$r.squared), as.numeric(lm4.2$r.squared))
# sigma
all.equal(as.numeric(summary(lm4.1)$sigma), as.numeric(lm4.2$sigma))
# F-test
all.equal(as.numeric(as.matrix(anova(lm(Age~1, data), lm4.1))[2, 5]), as.numeric(lm4.2$f.result[1]))

## -----------------------------------------------------------------------------
system.time({
  lm4.1 <- lm(Age~factor(Race1), data)
  anova(lm(Age~1, data), lm4.1)
  rstandard(lm4.1)
})
system.time(lm4.2 <- lr(Age~Race1, data, category = 1))

## -----------------------------------------------------------------------------
lm5.2 <- lr(Age~Race1, data, category = 2)

## -----------------------------------------------------------------------------
lm5.1 <- lm(Age~-1+factor(Race1), data)

names(lm5.2)
# fitted values
all.equal(as.numeric(lm5.1$fitted.values), as.numeric(lm5.2$y.fitted))
# conventional and internally studendized residuals
all.equal(as.numeric(lm5.1$residuals), as.numeric(lm5.2[["residual"]][, 1]))
all.equal(as.numeric(rstandard(lm5.1)), as.numeric(lm5.2[["residual"]][, 2]))
# coefficients (Note that the order of coefficients is different)
all.equal(as.numeric(summary(lm5.1)$coefficients[c(1:3), ]), as.numeric(lm5.2$cov.matrix[c(1:3), ]))
all.equal(as.numeric(summary(lm5.1)$coefficients[4, ]), as.numeric(lm5.2$cov.matrix[5, ]))
all.equal(as.numeric(summary(lm5.1)$coefficients[5, ]), as.numeric(lm5.2$cov.matrix[4, ]))
# degree of freedom
all.equal(as.numeric(summary(lm5.1)$r.squared), as.numeric(lm5.2$r.squared))
# sigma
all.equal(as.numeric(summary(lm5.1)$sigma), as.numeric(lm5.2$sigma))

## -----------------------------------------------------------------------------
system.time({
  lm5.1 <- lm(Age~--1+factor(Race1), data)
  anova(lm5.1)
  rstandard(lm5.1)
})
system.time(lm5.2 <- lr(Age~Race1, data, category = 2))

