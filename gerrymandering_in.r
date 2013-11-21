#doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
#toInstall <- c("maps", "ggplot2", "RColorBrewer", "MASS", 'colorspace')
#if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
#lapply(toInstall, library, character.only = TRUE)

require(MASS)
require(ggplot2)
require(RColorBrewer)

set.seed(1)

ElectoralDistribution = function(numberofdistricts, 
                                 gerrymandering = FALSE,
                                 howunfair = c(.4, .6), #add this later
                                 demdistricts = c(1:2),
                                 repdistricts = c(3:numberofdistricts),
                                 peopleperdistrict = 70000,
                                 includeindependents = FALSE, #addthislater
                                 partyalignment = c(.5,.5)) {

    state <- rep(0, numberofdistricts)
    population = numberofdistricts * peopleperdistrict
    
    if(gerrymandering == FALSE){
   		for(district in 1:numberofdistricts){
   			state[district] <- sum(sample(c(-1,1), size = peopleperdistrict, 
   										  replace = T))
        }
    }
    
    if(gerrymandering == TRUE){
    	for(district in demdistricts){               #democratic districts
    		state[district] <- sum(sample(c(-1,1), size = peopleperdistrict, 
    							      replace = T, prob = howunfair))
    	}
    	
    	offsetrestofstate = (.1 * length(demdistricts))/length(repdistricts)
    	demrestofstate = .5 - offsetrestofstate
    	represtofstate = .5 + offsetrestofstate
    	
    	
    	for(district in repdistricts){		         #republican districts
    		state[district] <- sum(sample(c(-1,1), size = peopleperdistrict,
    								  replace = T, prob = c(represtofstate, 
    								  demrestofstate)))
    	}
    }

    return(state)
}

MultipleDistributions <- function(numberoftimes, numberofdistricts, ... ) {

	sim.results <- matrix(data = NA, nrow = numberoftimes, 
						  ncol = numberofdistricts)

	for(i in 1:numberoftimes){
	
		sim.results[i,] <- ElectoralDistribution(numberofdistricts, ... )
		
	}
	
	return(sim.results)

}


