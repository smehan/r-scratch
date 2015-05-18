###########################################################
### CH-Heatmap-plot Class using heatmap2
###########################################################

# HC-heatmap with dendogram showing groups by states vs vars including
# average scores, spending, population
library(gplots)   # contains the heatmap.2 package
library(car)    
States[1:3,] # look at the data

scaled <- scale(States[,-1]) # scale all but the first column to make information comparable
heatmap.2(scaled, # specify the (scaled) data to be used in the heatmap
          cexRow=0.5, cexCol=0.95, # decrease font size of row/column labels
          scale="none", # we have already scaled the data
          trace="none") # cleaner heatmap

# Take out the dendograms and density plot information as well as add cell information manually

# Use color brewer
library(RColorBrewer)
my_palette <- colorRampPalette(c('red','yellow','green'))(256)

scaled <- scale(States[,-1])    # scale all but the first column to make information comparable
heatmap.2(scaled,               # specify the (scaled) data to be used in the heatmap
          cexRow=0.5, 
          cexCol=0.95,          # decrease font size of row/column labels
          col = my_palette,     # arguments to read in custom colors
          colsep=c(2,4,5),      # Adding on the separators that will clarify plot even more
          rowsep = c(6,14,18,25,30,36,42,47), 
          sepcolor="black", 
          sepwidth=c(0.01,0.01),  
          scale="none",         # we have already scaled the data 
          dendrogram="none",    # no need to see dendrograms in this one 
          trace="none",         # cleaner heatmap
          par(cex.main=1.0),    # change title font size
          density.info="none",  # turn off denisty plot inside color legend
          main="<State Exam Scores>\n vs.\nState \nEducation Spending") # title

