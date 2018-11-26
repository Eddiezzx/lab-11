library(microbenchmark)
library(ggplot2)

#1
q1 <- function(n, p){
  a <- runif(n, 0, 1)
  b <- sum(a < p)
  return(b)
}

q1(100, 0.6)
#2
microbenchmark(q1(100, 0.6))
microbenchmark(rbinom(100, 1, 0.6))

#3
set.seed(1)
x1 <- runif(50, min = 20, max = 40)
y1 <- (15 + 0.4 * x1 + rnorm(50, 0, 3))

q3 <- data.frame(x1, y1)
linearmodel <- lm(y1 ~ x1, data = q3)
fit <- fitted.values(linearmodel)
res <- residuals(linearmodel)

ggplot(q3, aes(x1,y1)) +
  geom_point(aes(y = fit)) +
  geom_point(aes(color = res)) +
  geom_segment(aes(xend = x1, yend = prd)) +
  geom_smooth(method = "lm") 
# I'm not sure if above is right plot to make for Q3. If not, the one below is a plot of fitted values vs residuals. If the plot from above is correct, please ignore this plot.
ggplot(c)+
  geom_point(aes(x = res, y = fit)) +
  labs(x = "Resiuals", y = "Fitted values")
#4

boxmuller <- function(n){
  set.seed(1)
  u1 <- runif(n, 0, 1)
  u2 <- runif(n, 0, 1)
  r <- (-2 * log(u1))^0.5
  theta <- 2 * pi * u2
  x <- r * cos(theta)
  y <- r * sin(theta)
  box <- matrix(c(x, y), nrow = 2)
  return((box[1, ] + box[2, ]) / 2)
}

ggplot() +
  geom_histogram(aes(boxmuller(1000)),color = 'red',binwidth = 0.1)+
  geom_histogram(aes(rnorm(1000, 0, 1)),color = 'blue', binwidth = 0.1)


