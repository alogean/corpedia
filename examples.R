#install.packages(c('RCurl','XML','igraph','bitops'),dependencies=TRUE)
g <- crawl("https://en.wikipedia.org/wiki/Legal_liability", 2)
save.graph(g, 'legal_liability_graph')
