dat_birds <- read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")
dat_habitat <- read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")

#pairplot for habitat data, pick any three columns to graph
pairs(dat_habitat[,c("elev", "p.edge.1", "ba.tot")])

#pairplot for bird data, pick any three columns to graph
pairs(dat_birds[,c("sta", "b.rich", "b.total")])

#note about subsetting
#subsetting has two main categories
#by position (brackets, single or double for lists) 
#or by name (with a dollar sign)

#make histogram of a bird species
hist(dat_birds$WIWA, 
     xlab = "Number of Birds Counted",
     main = "Histogram of Wilson's Warbler" )

