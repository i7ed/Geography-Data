# make database for world population by country
# for different years
library(XML)
library(RCurl)
library(ggplot2)
library(reshape2)

SAVE.DATA = FALSE

agent="Mozilla/5.0 (Windows NT 6.3; WOW64; rv:32.0) Gecko/20100101 Firefox/32.0"

#Set RCurl pars
curl = getCurlHandle()
curlSetOpt(cookiejar="cookies.txt",  useragent = agent, followlocation = TRUE, curl=curl)
PopPage <- getURL("http://www.nationmaster.com/country-info/stats/People/Population-in-2015", curl=curl)

tabs = readHTMLTable(PopPage, stringsAsFactors = FALSE)

d = tabs[[1]]

# list of countries
countries = d[,2]
population_string = d[,3]
gsub('million','0000',population_string)->p
gsub('thousand','000',p)->p
gsub(',','',p)->p
gsub('\\.','',p)->p
gsub(' ','',p)->p
population= as.numeric(p)

head(p)
df.2015 = data.frame(country=countries,x=1:length(p),p=as.numeric(p))
if (SAVE.DATA) { write.csv(file='population-by-country-2015.csv', df.2015) }
ggplot(df.2015, aes(p)) + geom_density(fill='red') + scale_x_log10() + xlab('population (2015)')

######## get previous years
histPopPage = getURL("https://en.wikipedia.org/wiki/List_of_countries_by_past,_current_and_future_population", curl=curl)
tabs = readHTMLTable(histPopPage, stringsAsFactors = FALSE)
m = na.omit(tabs[[1]])
names(m) = m[1,]
m <- m[-1,]
m[,-which(names(m)=='%')]->m
names(m)[1]='Country'

n = na.omit(tabs[[7]])
head(n)
names(n) = n[1,]
n <- n[-1,]
n[,-which(names(n)=='%')]->n
names(n)[1]='Country'

# merge two tables
merge(n,m, by='Country') -> d
if (SAVE.DATA) { write.csv(file='population-by-country-1970-2015.csv', d) }

# convert matrix to data frame
lapply(d, function(x) gsub(',','',x))->d
as.data.frame(d)->d2
# head(d2[,2:15])
# lapply(d2[,2:15],  as.numeric)->d3
# as.data.frame(d3)->d4

melt(d2, id.vars='Country', value.name='population')->dm
names(dm) = c('country','year','pop')
dm$pop = as.numeric(dm$pop)
as.numeric(substr(as.character(dm$year),2,5)) -> dm$year
as.factor(dm$year) -> dm$year
if (SAVE.DATA) { write.csv(file='population-by-country-1970-2015-melt.csv', dm) }

ggplot(dm, aes(pop, fill=year)) + geom_density(alpha=0.3) + scale_x_log10()
if (SAVE.DATA) {ggsave('population-density-1950-2015.png') }

dm2 = subset(dm, year==1950 | year==1990 | year == 2015)
ggplot(dm2, aes(pop, fill=year)) + geom_density(alpha=0.3) + scale_x_log10()
if (SAVE.DATA) {ggsave('population-density-1950-2015-less.png') }
