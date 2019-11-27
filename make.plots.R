library(ggplot2)
library(ggthemes)
source('../World-Data-Population/theme_tg.R')

read.csv('population-by-country-1970-2015-melt.csv') -> d
transform(d, pop.rank=ave(pop, year,
                          FUN = function(x) rank(-x, ties.method="first"))) -> d
d$pop=d$pop*1E3
d$pop = log(d$pop, base=10)

ggplot(d, aes(pop.rank, pop, color=as.factor(year))) + geom_point() + 
  theme_tg(base_size=18) + xlab('Rank') + ylab('Population in Country (log)') + ggtitle('World Population 1950 - 2015')
