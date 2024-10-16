library(rethinking)
d <- read.csv("https://www.torkar.se/data-slc.csv")
d <- d[d$AGE <= 30,]
plot(d$AGE)