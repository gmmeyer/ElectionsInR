#Unsure if the necessary packages are installed in R for this script to run?
#Or are you getting an error because they aren't?
#Uncomment the below lines, it will install all the packages I've used in this script.

#doInstall <- TRUE  # Change to FALSE if you don't want packages installed, or just comment it out.
#toInstall <- c("RColorBrewer", "colorspace")
#if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
#lapply(toInstall, library, character.only = TRUE)
#dir.create('graphs/', showWarnings = FALSE)
require(RColorBrewer)
require(grDevices)

color.vector.1  <- colorRampPalette(brewer.pal(n = 11, name = 'RdBu'))(101)

set.seed(1)

#Here's the general idea. I wanted to make a bit of a toy simulation of gerrymandering in states.
#So, basically, I just made random number generators to do the work, so as to be relatively fair.
#It just flips a coin to determine who is what party in a district.
#When I gerrymander, I distribute more heavily to some districts than to others and say, then we'll
#just distribute the rest of the people randomly accross the other districts.

#Mostly what I wanted was just a graphical display of an obvious statistical fact: when you gerrymander
#all you need is to gerrymander a few districts. Because, if you distribute two districts with 60% of one party,
#then the other districts will have that much less of that party. So, if the other districts aren't gerrymandered
#and are just random, then they'll likely be noncompetitive. And that's what the graphs show.

#Since the argument is purely statistical, this "simulation" doesn't really show anything. But I figured it's a cool
#little thing to use for this article I'm writing about this, which makes that essential argument:
#gerrymandering doesn't have to be very obvious or even all that insidious in order to have a big effect on a state.



#First we'll start by defining a few functions.
#Argument notes: numberofdistricts is the number of districts in the fictional state you're creating
#gerrymandering can be turned on or off with the true or false, default is off
#how unfair is only relevant if you're using gerrymandering, you can change it if you're not
#it just won't do anything
#demdistricts and rep districts merely defines how many districts are of each. make sure the variables
#line up to numberofdistricts or you'll get some districts with 0's in them
#originally I had made peopleperdistrict to be less than 700000, but the simulation runs fast enough
#with 700000 that I figured I'd just bump it up there. if it doesn't run fast enough for you, bump it down

#includeindependents and partyalignment are not yet implemented, I might not implement them.
#they would make the simulation more complex, but I'm not sure I need it/am willing to do the work.

ElectoralDistribution = function(numberofdistricts, 
                                 gerrymandering = FALSE,
                                 howunfair = c(.4, .6),
                                 demdistricts = c(1:2),
                                 repdistricts = c(3:numberofdistricts),
                                 peopleperdistrict = 700000,
                                 includeindependents = FALSE, #addthislater (I probably won't)
                                 partyalignment = c(.5,.5)) {

	#just two variables we'll need, the empty state variable saves some memory
    state <- rep(0, numberofdistricts)
    population = numberofdistricts * peopleperdistrict
    
    #results that aren't gerrymandered
    
    if(gerrymandering == FALSE){
   		for(district in 1:numberofdistricts){
   			state[district] <- sum(sample(c(-1,1), size = peopleperdistrict, 
   										  replace = T))
        }
    }
    
    #gerrymandered results
    
    if(gerrymandering == TRUE){
    	#democratic districts
    	for(district in demdistricts){              
    		state[district] <- sum(sample(c(-1,1), size = peopleperdistrict, 
    							      replace = T, prob = howunfair))
    	}
    	
		offsetrestofstate = (.1 * length(demdistricts))/length(repdistricts)
		demrestofstate = .5 - offsetrestofstate
		represtofstate = .5 + offsetrestofstate
    	
    	#republican districts
    	for(district in repdistricts){
    		state[district] <- sum(sample(c(-1,1), size = peopleperdistrict,
    								  replace = T, prob = c(represtofstate, 
    								  demrestofstate)))
    	}
    }

    return(state)
}

#This will run the simulation multiple times. I passed all the arguments except for the one
#that was needed for both functions to run onto the above function.

MultipleDistribution <- function(numberoftimes, numberofdistricts, ... ) {
	
	#I created the matrix beforehand, saves on memory space.
	
	sim.results <- matrix(data = NA, ncol = numberoftimes, 
						  nrow = numberofdistricts)

	for(i in 1:numberoftimes){
		sim.results[,i] <- ElectoralDistribution(numberofdistricts, ... )
	}
	
	return(sim.results)

}

#These functions will make the graphs of the simulation. 
#remember, the layout is this: simulation[district,run]
#These are just so messy, graphs in R are unfortunately, annoyingly messy.

GifBarPlot <- function(distribution, filename, runs, delay = 30){
	png(file='%02dtempelections.png', width=800, height=400)
		for(run in 1:runs){
			district.names = rep(0, length(distribution[,run]))
			for(i in 1:length(distribution[,run])){ district.names[i] = paste0('District ',i) }
			
			color.vector <- rep(NA, length(distribution[,run]))			
			for(district in 1:length(distribution[,run])){
				if(distribution[district,run] < 0){ 
					color.vector[district] <- 'red'
				} else if(distribution[district,run] > 0){
					color.vector[district] <- 'blue'
				} else{ 
					color.vector[district] <- 'grey'
				}
			}
			barplot(distribution[,run], col = color.vector, names.arg = district.names, border = 'black', ylim = c(min(distribution),max(distribution)))
		}
	dev.off()

	system(paste0("convert -delay ",delay," *tempelections.png ",filename))
	file.remove(list.files(pattern="tempelections.png"))
	
}

GifHeatMap <- function(distribution, filename, rows, runs, delay = 30){
	heat.breaks = seq(min(distribution),max(distribution), length.out = 102)
	if(0 %in% heat.breaks != T){
		heat.breaks = sort(c( seq(min(distribution), max(distribution), length.out = 101), 0))
	}
	
	png(file='%02dtempelections.png', width=800, height=400)
		for(run in 1:runs){

		heatmap.matrix <- matrix(distribution[,run], nrow = rows)
		heatmap.2(heatmap.matrix, col = color.vector.1, Rowv = NA, Colv = NA, labRow = NA, labCol = NA, 
				  key=T, dendrogram="none", keysize=1.5, density.info="none", 
				  trace="none", sepcolor = "black", breaks = heat.breaks)
		}
	dev.off()

	system(paste0("convert -delay ",delay," *tempelections.png ",filename))
	file.remove(list.files(pattern="tempelections.png"))
}


BarPlot <- function(distribution, filename){
	png(file=filename, width=800, height=400)
		district.names = rep(0, length(distribution))
		for(i in 1:length(distribution)){ district.names[i] = paste0('District ',i) }
	
		color.vector <- rep(NA, length(distribution))			
		for(district in 1:length(distribution)){
			if(distribution[district] < 0){ 
				color.vector[district] <- 'red'
			} else if(distribution[district] > 0){
				color.vector[district] <- 'blue'
			} else{ 
				color.vector[district] <- 'grey'
			}
		}
	
		barplot(distribution, col = color.vector, names.arg = district.names, border = 'black', ylim = c(min(distribution),max(distribution)))
	dev.off()
}

HeatMap <- function(distribution, filename, rows){
	png(file=filename, width=800, height=400)
		heatmap.matrix <- matrix(distribution, nrow = rows)
		heatmap.2(heatmap.matrix, col = color.vector.1, Rowv = NA, Colv = NA, labRow = NA, labCol = NA, 
				  key=T, dendrogram="none", keysize=1.5, density.info="none", 
				  trace="none", sepcolor = "black", breaks = heat.breaks)
	dev.off()
}



