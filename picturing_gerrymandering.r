#Now, we'll run the simulation. And, we'll put the results into some graphs.
run.names = rep(0, 30)
district.names = rep(0, 20)

for(i in 1:30){ run.names[i] = paste0('Run ',i) }
for(i in 1:20){ district.names[i] = paste0('District ',i) }

not.gerrymandered <- MultipleDistribution(30, 20)
gerrymandered <- MultipleDistribution(30, 20, gerrymandering = T)
less.gerrymandered <- MultipleDistribution(30, 20, gerrymandering = T, howunfair = c(.45,.55))






