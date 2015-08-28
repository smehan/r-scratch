###########################################################
### Class working with diagrammeR package to produce programmatic
### diagrams including sequence diagrams
###########################################################

library(DiagrammeR)

### Creates a directed graph Left to Right  with labelled nodes

DiagrammeR("
    graph LR;
           A-->B;
           A-->C;
           C-->E;
           B-->D;
           C-->D;
           D-->F;
           E-->F;
           ")

### Same graph Top to Bottom
DiagrammeR("
    graph TB;
    A-->B;
    A-->C;
    C-->E;
    B-->D;
    C-->D;
    D-->F;
    E-->F;
")

### Now with some CSS applied
DiagrammeR("
  graph LR;
           A(Rounded)-->B[Squared];
           B-->C{A Decision};
           C-->D[Square One];
           C-->E[Square Two];
           
           style A fill:#E5E25F;
           style B fill:#87AB51;
           style C fill:#3C8937;
           style D fill:#23772C;
           style E fill:#B6E6E6;
           ")

### With link text
DiagrammeR("
  graph LR;
           A(Start)-->|Line Text|B(Keep Going)
           B-->|More Line Text|C(Stop);
           
           style A fill:#A2EB86, stroke:#04C4AB, stroke-width:2px;
           style B fill:#FFF289, stroke:#FCFCFF, stroke-width:2px, stroke-dasharray: 4, 4;
           style C fill:#FFA070, stroke:#FF5E5E, stroke-width:2px;
           ")
### Display summary information on mtcars data set
# Load in the 'mtcars' dataset
data(mtcars)

# Obtain column names
column_names <- colnames(mtcars)

# Use for in loop to generate summary strings for each mtcars column
for (i in 1:length(column_names)){
    if (i == 1) connections <- vector(mode = "character", length = 0L)
    
    connections <-
        c(connections,
          paste0(i, "(", column_names[i], ")---", i, "-stats(",
                 "min: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 1]))), "<br/>",
                 "1Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 2]))), "<br/>",
                 "med: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 3]))), "<br/>",
                 "mean: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 4]))), "<br/>",
                 "3Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 5]))), "<br/>",
                 "max: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 6]))),
                 ")"))
}

DiagrammeR(
    paste0(
        "graph TD;", "\n",
        paste(connections, collapse = "\n"),"\n",
        "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
        "class ", paste0(1:length(column_names), collapse = ","), " column;"
    )
)

# Using this "How to Draw a Sequence Diagram" 
#  http://www.cs.uku.fi/research/publications/reports/A-2003-1/page91.pdf
# draw some sequence diagrams with DiagrammeR

DiagrammeR("
sequenceDiagram;
  customer->>ticket seller: ask ticket;
  ticket seller->>database: seats;
  alt tickets available
    database->>ticket seller: ok;
    ticket seller->>customer: confirm;
    customer->>ticket seller: ok;
    ticket seller->>database: book a seat;
    ticket seller->>printer: print ticket;
  else sold out
    database->>ticket seller: none left;
    ticket seller->>customer:  sorry;
  end
")

# library(pipeR)
# graph_1 <- create_graph() %>>%
#     add_node("a") %>>% add_node("b") %>>% add_node("c") %>>%
#     add_edges(from = c("a", "a", "b"),
#               to =   c("c", "b", "c"))
# graph_2 <- graph_1 %>>%
#     add_node("d") %>>% add_edges(from = "d", to = "c")
# graph_3 <- graph_2 %>>%
#     add_node("e") %>>% add_edges(from = "e", to = "b")
# # Create an empty graph series
# series <- create_series(series_type = "sequential")
# # Add graphs to the graph series
# series <- graph_1 %>>% add_to_series(series)
# series <- graph_2 %>>% add_to_series(series)
# series <- graph_3 %>>% add_to_series(series)
### display_graph(graph_#) is needed
