set.seed(5)
x <- qnorm(seq(0.05, 1, 0.05), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

y <- sample(1:100, 20, replace = TRUE)
qqnorm(y)
quantile(y, probs = seq(0, 1, 0.2))
dens <- density(y)
df <- data.frame(y, dens)
qplot(x,y,data=dd,geom="line")+
  geom_ribbon(data=subset(dd,x>q75 & x<q95),aes(ymax=y),ymin=0,  fill="red",colour=NA,alpha=0.5)

dd <- with(dens,data.frame(x,y))

ggplot(df, aes(x=x)) + geom_density() + geom_ribbon(aes(min=0, ymax=dens, fill=quant)) + scale_x_continuous(breaks=quantiles.x) + scale_fill_brewer(guide="none")
ggplot(df, aes(x=y)) + geom_density()

set.seed(1)
draws <- rnorm(100)^2
dens <- density(draws)
plot(dens)



quantiles.x <- quantile(df$x, prob=seq(0.2,1,0.2))
quantiles.y <- quantile(df$y, probs = seq(0.05, 1, 0.05))

?geom_density
library(data.table)
dt <- data.table(x=c(1:200),y=rnorm(200))
dens <- density(dt$y)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
quantiles <- quantile(dt$y, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
df$quant <- factor(findInterval(df$x,quantiles.x))
ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles.x) + scale_fill_brewer(guide="none")
library(ggplot2)
??qqplot
