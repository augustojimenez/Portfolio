library(nlme)
library(tidyverse)

set.seed(4)
n <- 1000
x <- seq(1,10,length.out = n)
y <- 1.2 + 2.1*x + rnorm(n, sd = 2*abs(x)^1.5)
d <- data.frame(y, x)
ggplot(d, aes(x, y)) + geom_point()

vm4 <- gls(y ~ x, data = d, weights = varPower(form = ~x))
model <- lm(y ~ x)
summary(vm4)
summary(model)
predict(vm4, newdata = data.frame(x = c(1:10)))
predict(model, newdata = data.frame(x = c(1:10)))
