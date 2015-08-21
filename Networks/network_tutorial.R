###########################################################
### Class to process edge and link data and create network 
### analyses and graphs.
###########################################################

# Some needed libs for networks
library(igraph)
library(network)
library(sna)
library(ndtv)

# Load the first data set
nodes <- read.csv("data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
