set.seed(5)

library(ggplot2)
library(data.table)

# Draw 100 values from a standarized normally distributed variable with mean = 0, sd = 1
x <- rnorm(100, 0,1)

# Draw a random sample of values between 1:100
y <- sample(1:100, 20, replace = TRUE)

# Construct a quantile-quantile plot for random variable y
qqnorm(y)

# Calculate the z-value for the quantiels of random variable y
quantiles.y <- quantile(df$y, probs = seq(0.05, 1, 0.05))
quantiles.y <- quantile(y, probs = seq(0, 1, 0.2))

# Draw z-values from a standardized normal distribution from 0.2 to 1.0 in 0.2 increments 
x <- qnorm(seq(0.2, 1, 0.2), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

# What I want to do:
# 1. Draw a density plot with shaded quantiles and overlap with the density plot for a normally distributed random variable with the same shaded quantiles
# 2. Move both these density plots to the corresponding axes on the quantile quantile plot and in an ideal world, draw the points as the intersection between

# Everything below was copy&pasted into R from Stackoverflow answers that I have saved.

# Draw a density function for random variable y
# This does not work
dens <- density(y)
df <- data.frame(y, dens)
qplot(x,y,data=dd,geom="line")+
  geom_ribbon(data=subset(dd,x>q75 & x<q95),aes(ymax=y),ymin=0,  fill="red",colour=NA,alpha=0.5)

dd <- with(dens,data.frame(x,y))
ggplot(df, aes(x=x)) + geom_density() + geom_ribbon(aes(min=0, ymax=dens, fill=quant)) + scale_x_continuous(breaks=quantiles.x) + scale_fill_brewer(guide="none")
ggplot(df, aes(x=y)) + geom_density()

# This works
set.seed(1)
draws <- rnorm(100)^2
dens <- density(draws)
plot(dens)

dt <- data.table(x=c(1:200),y=rnorm(200))
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
quantiles <- quantile(dt$y, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
df$quant <- factor(findInterval(df$x,quantiles.x))
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles.x) + scale_fill_brewer(guide="none")


