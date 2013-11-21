#Unsure if the necessary packages are installed in R for this script to run?
#Or are you getting an error because they aren't?
#Uncomment the below lines, it will install all the packages I've used in this script.

#doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
#toInstall <- c("ggplot2", "RColorBrewer", "colorspace")
#if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
#lapply(toInstall, library, character.only = TRUE)

require(ggplot2)
require(RColorBrewer)

set.seed(1)

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
