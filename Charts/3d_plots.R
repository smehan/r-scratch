### create a 3d scatterplot with a color map from an additional variable
library(scatterplot3d)
myData <- read.csv2("~/Downloads/PlotData.csv", header=TRUE, sep = ",")
# Chante the path and filename to match your path and filename

myData$pcolor[]
myData$pcolor[myData$Category=='A'] <- "red"
myData$pcolor[myData$Category=='B'] <- "blue"
myData$pcolor[myData$Category=='C'] <- "darkgreen"
# if your categories are different, change them above

# here's the plotting function and it's all base R
# Here we are using myData which has x,y,z values in three columns
# labeled Alpha, Beta, Gama [sic]. If your headers were different, these
# will need to reflect that
with(myData, {
    s3d <- scatterplot3d(Alpha, Beta, Gama,        # x y and z axis from myData
                         color=pcolor, pch=19,     # circle color indicates category type
                         # type="h", lty.hplot=2,       # lines to the horizontal plane
                         scale.y=.75,              # scale y axis (reduce by 25%)
                         main="Your 3-D Scatterplot",
                         xlab="Alpha",
                         ylab="Beta",
                         zlab="Gama")
    s3d.coords <- s3d$xyz.convert(Alpha, Beta, Gama)
})


### create a 2d surface for z(x,y)
newData <- matrix(data = runif(90), nrow = 30, ncol = 30)
persp(x = seq(1,30,1), # This is the x-coords, so if percentages, 1,100,1
      y = seq(1,30,1), # this could be x if square,
      newData, # This is your z values, as a matrix against the x,y
      phi = 30, # standard polar coordinates
      theta = 45, 
      expand = .15, # expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction.
      col = "lightblue", # color of the surface
      border = "yellow", # This is the color of the wireframe, try "NA" for no grid 
      box = TRUE, # Change me to FALSE to get rid of bounding box
      ltheta = 120, shade = 0.75, ticktype = "detailed", # I'd leave these
      xlab = "Alpha", ylab = "Beta", zlab = "Gamma",
      main = "Your Plot")