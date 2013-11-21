#doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
#toInstall <- c("maps", "ggplot2", "RColorBrewer", "MASS", "grDevices")
#if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
#lapply(toInstall, library, character.only = TRUE)

require(grDevices)
require(MASS)
require(ggplot2)
require(RColorBrewer)

set.seed(1)

#I'm going to separate the work into various functions.
#I am going to be writing these functions in the most legible way,
#since this is going to be linked to by the article, not necessarily in the most
#computationally efficient way.

#Deforming a map is quite hard to do (and far harder to understand to the
#sight reader. Therefore, I am going to move around people rather than moving
#around the map.

#Because of the one man, one vote rule in the US, each district has to have
#a roughly equal population. The average population per district is ~710,767
#as per the last census. I am going to round this down to 700,000 for our
#purposes here. 

ElectoralDistribution = function(numberofdistricts, 
                                 gerrymandering = FALSE,
                                 howunfair = 'null', #add this later
                                 peopleperdistrict = 70000,
                                 includeindependents = FALSE, #addthislater
                                 partyalignment = c(.5,.5)) {
    #I am initializing these two vectors. One of them shows the electoral
    #distribution over the state. The other one shows the population 
    #of a given district.
    state <- rep(0, numberofdistricts)
    districtpopulation <- rep(0, numberofdistricts)
    population = numberofdistricts * peopleperdistrict
    person = NA
    district = NA
    districtsdown = 0
    
    if(gerrymandering == FALSE) {
        districtprob = rep(1/numberofdistricts, numberofdistricts)
    }
    
    #The way this works is pretty simple. Every passby of the loop will
    #create a random person then assign him to a district. Through  the use
    #of the prob function, we will ensure that the distribution of parties
    #statewide is proper.
    #Each Republican added to the district will be represented by a negative number.
    #Each Democrat, a positive number.
    
    #I will not assure in the ungerrymandered case that the distribution of parties
    #is equal by district.
    #That state would be very easy to create:  state <- rep(0, numberofdistricts).
    #Instead, this is an experiment to see how easy it would be to randomly
    #generate districts, and how easily the districts could be shifted towards
    #one side or another.
    
    while(sum(districtpopulation) <= population & gerrymandering == FALSE) {
        person <- sample(c(-1,1), 1)
        district <- sample(c(1:numberofdistricts),1, prob = districtprob, 
                           Replace = TRUE)
        
        state[district] = state[district] + person
        districtpopulation[district] = districtpopulation[district] + 1
        
        if(districtpopulation[district] == peopleperdistrict) {
            districtprob[district] = 0
            districtsdown = districtsdown + 1
        }
    }
    
    while(sum(districtpopulation) <= population & gerrymandering == TRUE) {
        person <- sample(c(-1,1), 1)
    }

    return(state)
}
