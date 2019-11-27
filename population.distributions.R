library(ggplot2)
library(ggthemes)
read.csv('population-by-country-1970-2015-melt.csv') -> d
transform(d, pop.rank=ave(pop, year,
                          FUN = function(x) rank(-x, ties.method="first"))) -> d

# compare two countries
dc = subset(d, country=='Pakistan')
dc2 = subset(d, country=='Nigeria')
plot(dc$year, dc$pop, col='blue', pch=20)
points(dc2$year, dc2$pop, col='red')

# plot population with rank
m=subset(d, year==1980)
plot(m$pop.rank, m$pop, log='y', col='red')

q = data.frame(x=m$pop.rank, p=m$pop)
q[-which(m$pop.rank<3),]->q
x = 1:nrow(q)

nls(data = q, p ~ A*exp(-(x*B)^0.4), start=list(A=1E6,B=0.3)) -> fit3
#nls(data = q, p ~ A*exp(-(x*B)^0.3), start=list(A=5E6,B=0.4)) -> fit3
predict(fit3, list(x=x)) -> y3
q2 = data.frame(x,y=y3, type=rep('stretch',length(x)))
lines(q2$x, q2$y, col='blue', lwd=3)
# fit data

data = subset(d, year==2015)
table(cut(data$pop, breaks=(0:1000)/1000*max(data$pop)))->m
as.data.frame(m)->m2
cumsum(m2$Freq) -> p
x = 1:length(p)
q = data.frame(x,p)

head(q)
head(data$pop)
plot(q, log='xy')



nls(data = q, p ~ A*x^B, start=list(A=150,B=0.06)) -> fit
nls(data = q, p ~  A*(1-exp(-x*B)), start=list(A=222,B=0.06)) -> fit2
nls(data = q, p ~ A*(1-exp(-(x*B)^0.5)), start=list(A=230,B=0.09)) -> fit3

predict(fit, list(x=x)) -> y
predict(fit2, list(x=x)) -> y2
predict(fit3, list(x=x)) -> y3

q2 = data.frame(x,y, type=rep('power',length(x)))
q2 = rbind(q2, cbind(x,y=y2, type=rep('exp',length(x))))
q2 = rbind(q2, cbind(x,y=y3, type=rep('stretch',length(x))))
q2$x = as.numeric(q2$x)
q2$y = as.numeric(q2$y)

ggplot(q,aes(x,p)) + geom_point( size=4, col='blue') + scale_x_log10() + scale_y_log10() +
  geom_line(data=q2, aes(x, y, color=type), size=5, alpha=0.5) + #theme_tg() + 
  xlab('log (population)') + ylab('log (CDF)') +
  ggtitle('Country Population') + theme_economist()
