#Unsure if the necessary packages are installed in R for this script to run?
#Or are you getting an error because they aren't?
#Uncomment the below lines, it will install all the packages I've used in this script.

#doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
#toInstall <- c("RColorBrewer", "colorspace")
#if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
#lapply(toInstall, library, character.only = TRUE)
require(RColorBrewer)
require(grDevices)
require(gplots)
source('gerrymandering_in.r')

#Now, we'll run the simulation. And, we'll put the results into some graphs.

not.gerrymandered <- MultipleDistribution(30, 20)
gerrymandered <- MultipleDistribution(30, 20, gerrymandering = T)
less.gerrymandered <- MultipleDistribution(30, 20, gerrymandering = T, howunfair = c(.45,.55))

GifBarPlot(not.gerrymandered, 'barplot.not.gerrymandered.gif', runs = 30)
GifHeatMap(not.gerrymandered, 'heatmap.not.gerrymandered.gif', runs = 30, rows = 4)

GifBarPlot(gerrymandered, 'barplot.gerrymandered.gif', runs = 30)
GifHeatMap(gerrymandered, 'heatmap.gerrymandered.gif', runs = 30, rows = 4)

GifBarPlot(less.gerrymandered, 'batplot.less.gerrymandered.gif', runs = 30)
GifHeatMap(less.gerrymandered, 'heatmap.less.gerrymandered.gif', runs = 30, rows = 4)
