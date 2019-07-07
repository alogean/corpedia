library(corpedia)
# install.packages(c('RCurl','XML','igraph','bitops'),dependencies=TRUE)
# https://semanticweb.cs.vu.nl/R/wikipedia_graph/wikipedia_graph.html

g <- crawl("https://en.wikipedia.org/wiki/Infrastructure", 3)
save.graph(g, 'Infrastructures')

f_2000_2013 <- read.csv("Forbes_2000_2013.csv", header = TRUE, sep = ",")
f_2000_2015 <- read.csv("Forbes_2000_2015.csv", header = TRUE, sep = ",")
f_2000_2017 <- read.csv("Forbes_2000_2017.csv", header = TRUE, sep = ",")
f_2000_2018 <- read.csv("Forbes_2000_2018.csv", header = TRUE, sep = ",")
