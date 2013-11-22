#Now, we'll run the simulation. And, we'll put the results into some graphs.
run.names = rep(0, 30)
district.names = rep(0, 20)



for(i in 1:30){ run.names[i] = paste0('Run ',i) }
for(i in 1:20){ district.names[i] = paste0('District ',i) }

run.names = c('X','Y',run.names)

not.gerrymandered <- MultipleDistribution(30, 20)
gerrymandered <- MultipleDistribution(30, 20, gerrymandering = T)
less.gerrymandered <- MultipleDistribution(30, 20, gerrymandering = T, howunfair = c(.45,.55))

# with this code, I can now finally implement ggplot2
X = rep(c(1,2,3,4), times = 1, each = 5)
Y = rep(c(1,2,3,4,5), times = 4)

not.gerrymandered <- cbind(X, Y, gerrymandered)
gerrymandered <- cbind(X, Y, gerrymandered)
less.gerrymandered <- cbind(X, Y, less.gerrymandered)

not.gerrymandered.df <- data.frame(not.gerrymandered, row.names = district.names)
names(not.gerrymandered.df) <- run.names
gerrymandered.df <- data.frame(gerrymandered, row.names = district.names)
names(gerrymandered.df) <- run.names
less.gerrymandered.df <- data.frame(less.gerrymandered, row.names = district.names)
names(less.gerrymandered.df) <- run.names


#make this into a function later and put it on the functions file.
#making images in R is so messy. I'd make this prettier if I could, but it's very hard to do.
#I should output a lot of this into the outpur vector and just strip it in a future function.
png(file='/graphs/%02delections.png', width=800, height=800)
	for(run in 1:30){
		color.vector <- rep(NA, length(not.gerrymandered[,run])
		for(district in 1:length(not.gerrymandered[,run])){
			if(not.gerrymandered[district,run] < 0){ 
				color.vector[district] <- 'red'
			} else if(not.gerrymandered[district,run] > 0){
				color.vector[district] <- 'blue'
			} else{ 
				color.vector[district] <- 'grey'
			}
			
		}
		par(mfrow=c(2,1))
		a <- matrix(not.gerrymandered[,run], nrow = 4, ncol = 5)
		plot() #line plot
		barplot(not.gerrymandered[,run], col = color.vector, names.arg = district.names, border = 'black'
				ylim = c(700000,-700000) )
		
		
		howmanyr = sum( not.gerrymandered[,run] < 0 )
		howmanyd = sum( not.gerrymandered[,run] > 0 )
		mtext(paste0("Republicans have ",howmanyr," districts"), side = 3, outer = TRUE, col = 'red')
		mtext(paste0("Democrats have ",howmanyd," districts"), side = 1, outer = TRUE, col = 'red')
	
	}
dev.off()

system("convert -delay 30 *elections.png not.gerrymandered.gif")

file.remove(list.files(pattern="elections.png"))


