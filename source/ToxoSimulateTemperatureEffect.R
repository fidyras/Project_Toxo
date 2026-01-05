
### Explore fits assuming that there are strong biological limits, but there is some variation in the detail of the relationship between these extremes dependent on local context (e.g., there might be more cats, or more rapid cat consumption of mice, etc). 


# temperature pattern
temperatures <- seq(0,45, length=100)


## assume a hard limit at t=40, and lower bound at t=20 - i.e., no oocyst survival. Then make triangles that peak at different point in this margin

index <- c(which(temperatures==20), which(temperatures==40))
rel1 <- rel2 <- rel3 <- rep(0,length(temperatures))

# the upslope
a1 <- 1/(25-20); b1 <- (1-a1*25)
a2 <- 1/(30-20); b2 <- (1-a2*30)
a3 <- 1/(35-20); b3 <- (1-a3*35)

rel1[index[1]:which(temperatures==25)] <- a1*temperatures[index[1]:which(temperatures==25)]+b1
rel2[index[1]:which(temperatures==30)] <- a2*temperatures[index[1]:which(temperatures==30)]+b2
rel3[index[1]:which(temperatures==35)] <- a3*temperatures[index[1]:which(temperatures==35)]+b3

# the downslope
# the upslope
a11 <- 1/(25-40); b11 <- 1-a11*25
a21 <- 1/(30-40); b21 <- 1-a21*30
a31 <- 1/(35-40); b31 <- 1-a31*35


rel1[which(temperatures==25):index[2]] <- a11*temperatures[which(temperatures==25):index[2]]+b11
rel2[which(temperatures==30):index[2]] <- a21*temperatures[which(temperatures==30):index[2]]+b21
rel3[which(temperatures==35):index[2]] <- a31*temperatures[which(temperatures==35):index[2]]+b31

## Plot them out
plot(temperatures,rel1,type="l",lty=1)
points(temperatures,rel2,type="l",lty=3)
points(temperatures,rel3,type="l",lty=2)



## Translate into force of infection across age by sampling at random 

nsim <- 1000
prob.seropos <- age <- temp.index <- seropos <- rep(NA,nsim)

for (j in 1:nsim) { 

	#select which curve
	nval <- seq(1:3,size=1) 
	if (nval==1) rel0 <- rel1	
	if (nval==2) rel0 <- rel2	
	if (nval==3) rel0 <- rel3	
	
	#pick out an age 
	age[j] <- sample(1:50,size=1)

	#choose a temperature index
	temp.index[j] <- sample(1:length(temperatures),size=1)
	
	#simulate the seroprevalence - get prob, and then get a random variable from binomial
	prob.seropos[j] <- exp(-rel0[temp.index[j]]*age[j])
	seropos[j] <- rbinom(1, prob.seropos[j],1)


}








