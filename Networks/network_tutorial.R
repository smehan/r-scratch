###########################################################
### Class to process edge and link data and create network 
### analyses and graphs. 
### Builds on https://rpubs.com/kateto/netviz
###########################################################

# Some needed libs for networks
library(igraph)
library(network)
library(sna)
library(ndtv)

# Load the first data set
nodes <- read.csv("Networks/data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Networks/data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# Examine the data
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# Notice that there are more links than unique from-to combinations. That means 
# we have cases in the data where there are multiple links between the same two nodes. 
# We will collapse all links of the same type between the same two nodes by summing 
# their weights, using aggregate() by “from”, “to”, & “type”:

links <- aggregate(links$weight, list(from=links$from, to=links$to, type=links$type), sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

# dataset 2
nodes2 <- read.csv("Networks/data/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Networks/data/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

# Examine the data
head(nodes2)
head(links2)

# This shows that links2 is an adjacency matrix for a two mode network
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

# start by converting the raw data to an igraph network object. Use igraph’s graph.data.frame function,
# which takes two data frames: d and vertices.
# d describes the edges of the network. Its first two columns are the IDs 
# of the source and the target node for each edge. 
# The following columns are edge attributes (weight, type, label, or anything else).
# vertices starts with a column of node IDs. Any following columns are interpreted as node attributes.

net <- graph.data.frame(links, nodes, directed=T)

# several helper functions to access edges and vertices, e.g.
E(net)
V(net)
E(net)$type
V(net)$audience.size

# You can also manipulate the network matrix directly:
net[1,]
net[5,7]

plot(net) # not a pretty picture!

# fixing things by removing the loops in the graph.

net <- simplify(net, remove.multiple = F, remove.loops = T) 

# You might notice that could have used simplify to 
# combine multiple edges by summing their weights with a 
# command like simplify(net, edge.attr.comb=list(Weight="sum","ignore")). 
# The problem is that this would also combine multiple edge types
# (in our data: “hyperlinks” and “mentions”).
# Let’s also reduce the arrow size and remove the labels (we do that by setting them to NA):
    
plot(net, edge.arrow.size=.4,vertex.label=NA)

# We need some palettes for maps, so 
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(8, "Blues")
pal3 <- brewer.pal(10, "Set3") 
plot(x=10:1, y=10:1, pch=19, cex=4, col=pal3)

# Two ways to set attributes.
# First is to set in base plot()
# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to light gray, the node & border color to orange 
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black") 

# Second is to set attrs in igraph object
# Generate colors base on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
# TODO have a failuer at this step
# http://stackoverflow.com/questions/9641650/problem-with-igraph-degree-function
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net) 

# We can also override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50") 

# It helps to add a legend explaining the meaning of the colors we used:
plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Sometimes, especially with semantic networks, we may be 
# interested in plotting only the labels of the nodes:
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
         vertex.label.font=2, vertex.label.color="gray40",
         vertex.label.cex=.7, edge.color="gray85")

# Let’s color the edges of the graph based on their source node color. 
# We can get the starting node for each edge with get.edges igraph function.

edge.start <- get.edges(net, 1:ecount(net))[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)  

###########################################################
# Network Layouts
###########################################################

