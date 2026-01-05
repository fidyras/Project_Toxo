## analysis of temperature effects on Oocysts - Dubey at al. 1998, Dubey et al. 1970 ##############

##Values extracted from Meerburg & Kijlstra 2009 Parasitology Research, p19, citing Dubey et al. work 
#temp <- c(-10,4,35,40)
#days.survived <- c(106,54*4*7,32,9) #this is 'days survived 'at least'' 
#par(mfrow=c(1,1),bty="l")
#plot(temp,1/(days.survived/7), xlab="Temperature",ylab="Hazard rate per week", pch=19)
#fit <- smooth.spline(I(1/(days.survived/7))~temp,df=3)
#points(predict(fit,-10:40), type="l",col=2,lwd=2) ## notice goes below 0, will need to constrain
### These numbers are just made up based on the lower bounds, so update, and get detailed info as below. 


## Full data from Dubey et al. 1998; different temperatures and experimented with actually infecting mice; 
## Data is in Table 1; assuming that 35 is the first two rows, etc
temperature <- c(rep(35,8), 
		rep(40,5*4),rep(45,9*2),
		rep(50,4*2),  			## weirdness in table, assumes numbers sequential
		rep(55,4*2))
duration <- c(32,32,32,32,62,62,62,62,		#duration in days
		rep(c(2,9,16,21,28),each=4),
		rep(c(1,2,3,6)/24,each=2),rep(c(1,3,4,7,11),each=2),
		rep(c(0.5,1,2,3)/24,each=2),
		rep(c(10,20,30,60)/24,each=2))

#here outcome framed in terms of ooccysts dying (so zero means 'censored', and 1 means 'dead', or failed infection)	
outcome <- 1-c(1,1,1,1,0,0,0,0,
			rep(1,4),rep(1,4),rep(0,4),c(1,0,0,0),rep(0,4),
			rep(1,8),1,1,rep(0,8),
			1,1,1,1,0,0,0,0,
			rep(0,8))



#  Convert duration into weeks, for congruence with the rest of the model (note that this doesn't actually change anything since PH)
duration <- duration/7

# Fit a proportional hazards model
require(survival)
fit1 <- coxph(Surv(duration,outcome)~temperature)

#require(survminer)
#test.ph <- cox.zph(fit1)
#ggcoxzph(test.ph)
#ggcoxfunctional(Surv(duration,outcome) ~ temperature + log(temperature) + sqrt(temperature), data=data.frame(duration=duration,outcome=outcome,temperature=temperature))
### Looks like the lowest temperature sharp decline

# Plot with the confidence intervals. 

par(mfrow=c(1,3))
plot(Surv(duration[temperature<45],outcome[temperature<45]), xlab="Time (days)", ylab="Survival", cex.lab=1.5)
plot(Surv(duration[temperature>40],outcome[temperature>40]), xlab="Time (days)", ylab="Survival", cex.lab=1.5)

plot(0:15,exp(fit1$coeff*(0:15)), type="l", xlab="Increase in temperature", ylab="Multiplier of the mortality hazard",log="y", cex.lab=1.5)
points(0:15,exp((fit1$coeff+1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3)
points(0:15,exp((fit1$coeff-1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3)



