#doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
#toInstall <- c("maps", "ggplot2", "RColorBrewer", "MASS", "grDevices")
#if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
#lapply(toInstall, library, character.only = TRUE)

require(grDevices)
require(MASS)
require(ggplot2)
require(RColorBrewer)

#I'm going to separate the work into various functions.
#I am going to be writing these functions in the most legible way,
#since this is going to be linked to by the article, not necessarily in the most
#computationally efficient way.

#Deforming a map is quite hard to do (and far harder to understand to the
#sight reader. Therefore, I am going to move around people rather than moving
#around the map.

RegularDistribution = function() {

}

GerrymanderedDistribution = function() {

}
