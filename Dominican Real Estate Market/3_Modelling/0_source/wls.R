rm(list = ls())
realestate <- read.delim("C:/Users/Augus/Downloads/STAT501_Lesson13 (1)/STAT501_Lesson13/home_price.txt",
                          header = T)
attach(realestate)

plot(Sales.Price ~ home.ft2)

logY <- log(Sales.Price)
logX1 <- log(home.ft2)
logX2 <- log(lot.ft2)

plot(logY ~ logX1)

model.1 <- lm(logY ~ logX1 + logX2)
summary(model.1)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.25485    0.07353  57.864  < 2e-16 ***
# logX1        1.22141    0.03371  36.234  < 2e-16 ***
# logX2        0.10595    0.02394   4.425 1.18e-05 ***

plot(fitted(model.1), residuals(model.1))

wts <- 1/fitted(lm(abs(residuals(model.1)) ~ fitted(model.1)))^2

model.2 <- lm(logY ~ logX1 + logX2, weights=wts)
summary(model.2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.35189    0.06330  68.755  < 2e-16 ***
# logX1        1.20150    0.03332  36.065  < 2e-16 ***
# logX2        0.07924    0.02152   3.682 0.000255 ***

plot(fitted(model.2), rstandard(model.2))

detach(realestate)

