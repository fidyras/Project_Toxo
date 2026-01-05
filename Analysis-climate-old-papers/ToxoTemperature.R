###############     Mathematical model of Toxoplasma based on Rasambainarivo et al. 2024; 
###############     Exploring putative temperature depenence in d 


options(scipen = 999)

library(deSolve)
library(reshape2)
library(tidyverse)
library(patchwork)

# SIR equations
sir_equations <- function(time, variables, parameters) {
  
  with(as.list(c(variables, parameters)), {
   
    dNc <- (bc-mc) * Nc * (1 - Nc / Kc) #population size of cats
    
    dNr <- (br-mr) * Nr * (1 - Nr / Kr) #population size of rodents
  
    dNf <- (bf-mf) * Nf * (1 - Nf / Kf) #population size of euplerids
    
    dSc <-  bc*(1-vc)*Nc - # birth of cats v  is vaccination
      betac*E*Sc  - # infection through contact with the environment E at a rate betac
      mc * Sc -     #mortality of susceptible cats at rate mc
      g*ac*Sc*Ir/Nr    #predation on infected rodents at rate a and given probability of infection when cat consumes infected prey g
    
    dIc <-  - mc*Ic + # mortality of infected cats
      betac*E*Sc - 
      gamma * Ic + # recovery of infected cats at a gamma rate
      g*ac*Sc*Ir/Nr        # infected cats
    
    dRc <-   gamma * Ic +
      bc*vc*Nc-
      mc*Rc #recovered cats
    
    
    
    dE <- lambda*Ic*(1-M*E) - d*E #environment where M indicates is clustering of the infection
    
    
    
    dSr <- br*Nr - 
      (mr + (br - mr)* Nr/Kr)*Sr -
      betar*E*Sr  - #infection through contact with environment at a rate betar
       mr * Sr - 
       g*ac*Nc * Sr/Nr - # predation by cats
       g*af*Nf * Sr/Nr # predation by carnivores 
    
    dIr <-  - mr*Ir + 
      betar*E*Sr - 
      g*ac*Nc*Ir/Nr -  # predation by cats
      g*af*Nf*Ir/Nr       # predation by carnivores
    
    
    
    dSf <- bf*Nf - 
      betaf*E*Sf  - 
      mf * Sf - 
      g*af*Sf*Ir/Nr          # susceptible Eupleridae
    
    dIf <-  - mf*If +
      betaf*E*Sf +
      g*af*Sf*Ir/Nr                       # infected Eupleridae
    
    return(list(c(dSc,
                  dIc,
                  dRc,
                  dE,
                  dSr,
                  dIr,
                  dSf,
                  dIf,
                  dNc,
                  dNr,
                  dNf)))
  })
}



## Set up the range of parameters ################################################

parameters_values <- c(
  bc = 2.4/52, #birth rates of cats 
  mc = 2.4/52, # mortality rate of cats
  betac  = 0.54/52, # Transmission rate from contaminated environments to cats (/cats/week)
  Kc = 100, #carrying capacity of cats
  vc = 0,# proportion of vaccinated cats
  g = 1, # probability of infection when a cat consumes an infected rodent
  
  M = 1, # indicates clustering of cat defecation 1 is clustered
  ac = 1/52, # predation rate (rodent per week per cat)
  af = 4/52, # predation rate (rodent per week per carnivore)
  
  br = 8/52, # birth rate of rodents
  mr = 2/52, # mortality rate of rodents
  Kr = 1500,
  betar = 0.4/52, # transmission rate from contaminated environment to rodents
  
  bf = 2/52, #birth rates of wild carnivores 
  mf = 2/52, # mortality rate of wild carnivores
  betaf  = 2.16/52, # Transmission rate from contaminated environments to wild carnivores (/cats/week)
  Kf = 100,
  
  gamma = 0.5, # recovery rate
  lambda = 1/16, #contamination rate of environment by one infected cat
  d = 1/100#decontamination rate

  )


## Set up the starting population values  ################################################

initial_values <- c(
  Sc = 99,  # number of susceptible cats at time = 0
  Ic =   1,  # number of infectious cats at time = 0
  Rc =   0,   # number of recovered (and immune) cats at time = 0
  
  E = 0, # proportion of contaminated patches of environment
  
  Sr = 1500,  # number of susceptible rodents at time =0
  Ir = 0,   # number of infected rodents at time = 0
  
  Sf = 100, # number of susceptible carnivores at time = 0
  If = 0, # number of infected carnivores at time = 0 
  
  Nc = 100, 
  Nr = 1500,
  Nf = 100)


time_values <- seq(0, 520) # weeks for three years




##### Illustrate one simulation ################################################

output<-as.data.frame( ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)
)



##### Make up a rule set based on temperature based on Dubey at al. 1998, Dubey et al. 1970 ##############

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
plot(0:15,exp(fit1$coeff*(0:15)), type="l", xlab="Increase in temperature relative to 30 degrees", ylab="Multiplier of the mortality hazard",log="y")
points(0:15,exp((fit1$coeff+1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3)
points(0:15,exp((fit1$coeff-1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3)





##### Loop over a range of temperatures, extracting d from the above, and setting minimum to be 0.0001 (i.e., very long duration)

#temp.test <- seq(-10,40,length=60)
temp.test <- seq(15,40,length=60)
dtest <- exp(fit1$coeff*(temp.test-30))*0.03 ## assume that at 30degrees persistence is 0.03 based on Fidy's Dumetre etc
store.results <- matrix(NA,length(dtest),12)

for (j in 1:length(dtest)) { 

	## replace parameter values with the original, and then over-write the d with desired value
	parameters_values_here <- parameters_values
	parameters_values_here["d"] <- dtest[j]

	## run the model with these parameter values 
	output<-as.data.frame(ode(
  		y = initial_values,
  		times = time_values,
  		func = sir_equations,
  		parms = parameters_values_here))

	## store the last row, 
	store.results[j,] <- as.numeric(output[nrow(output),])

	
}

colnames(store.results) <- c("time","susceptible cats","infected cats","recovered cats","env","susceptible rats","recovered rats",
				"susc fossa","infected fossa","ncats","nrats","nfossa")

##### Plot out the proportion of infected cats 
prop.infected.cats <- store.results[,3]/rowSums(store.results[,2:4])
infected.env <- store.results[,5]

#par(mfrow=c(1,2),bty="l")
#plot(temp.test,dtest,ylab="Rate of oocyst 'decontamination'", xlab="Temperature in celsius", type="l")
#plot(temp.test,prop.infected.cats, xlab="Temperature in celsius", ylab="Proportion of infected cats", type="l")



##### Estimate FOI in people based on this paper  https://pathexo.societe-mtsi.fr/documents/articles-bull/BullSocPatholExot-1995-88-1-046-049.pdf
### 31st Jan 1995

## this is the data
lower.ages <- c(15,20,25,30,35,40)
upper.ages <- c(19,24,29,34,39,50)
mid.ages <- c(lower.ages+upper.ages)/2
iggpos <- c(75,81,100,84,80,81)/100


## here is a simple sum of squares minimizer to get the right coefficiet
sum.squares <- function(par,xvals,yvals){
	pred <- 1-exp(-par*xvals)
	u <- sum((yvals-pred)^2)
	return(u)
}

## run this
tmp <-optimize(f=sum.squares,lower=0.01,upper=3,xvals=c(1,mid.ages),yvals=c(0,iggpos))

## plot the data and the fitted model (!note making up the first point - use age=1 for 0 to ensure no maternal immunity) 
#plot(c(1,mid.ages),c(0,iggpos), type="b",xlab="age", ylab="Igg seropos", pch=19)
#points(1:100,1-exp(-tmp$minimum*(1:100)),type="l", col="red")

## plot the average age of infection based on this 
density.avg.age <- tmp$minimum*(exp(-tmp$minimum*(1:100)))
#plot(1:100,density.avg.age,type="l",xlab="Age", ylab="pdf of cases", xlim=range(c(0,upper.ages)))


## Assume that the rate of infection of humans should be x the approx environmental reservoir
## and assume temperature is around 25 degrees on average - somewhere around upper end of Tana)
chs.tmp <- which(abs(temp.test-25)==min(abs(temp.test-25)), arr.ind=TRUE)


temp.test[chs.tmp] 
dtest[chs.tmp]
focal.e <- store.results[chs.tmp,5]

rate.human.infection.from.e <- tmp$minimum/focal.e
rate.human.infection.from.e


### get the average age of infection, defined as 1/rate
average.age.across.temp <- 1/(rate.human.infection.from.e*infected.env)
#plot(temp.test,average.age.across.temp, xlab="temperature", ylab="Average age 1st infection with T. gondii", ylim=c(0,50), type="l")




### Put this in the context of the age profile of fertility from https://population.un.org/wpp/Download/Standard/Fertility/

ages.of.fertility <- c(15:49)
fertility.per.1000.1995 <- c(63.23,	105.17,	148.60,	189.94,	223.54,	248.42,	264.49,	272.71,	276.03,	275.60,	273.29,	269.81,	265.08,	259.23,	252.90,	246.48,	239.08,	230.69,	220.16,	207.73,	194.84,	183.35,	171.08,	157.90,	144.58,	132.24,	115.02,	94.78,	75.00,	56.82,	39.81,	28.58,	20.09,	13.90,	8.49)
fertility.per.1000.2023 <- c(60.17,	101.81,	139.95,	169.83,	185.08,	186.50,	184.54,	181.80,	179.84,	178.79,	177.79,	176.13,	172.99,	168.23,	162.18,	154.99,	147.72,	140.37,	132.71,	124.73,	116.69,	108.38,	99.58,	90.21,	80.26,	69.66,	58.48,	46.95,	36.52,	27.59,	20.37,	15.42,	11.37,	7.93,	4.74)


number.women.1000s.1995 <-c(163,158,153,148,143,	138,	134	,129	,124	,119	,113	,108	,103	,99	,95	,91	,88	,86	,83	,81	,79	,76	,73,	71,	68,	66,	63,	61,	58,	56,	53,	47,	40	,38	,36)
number.women.1000s.2023<-c(347,	338,	329,	322,	315,	308,	303	,298	,293	,288	,280	,270	,259	,248	,236	,226	,217	,208	,199	,190	,182	,175	,170,	166,	161,	156,	152,	148,	144,	139,	135,	130	,125	,120	,115)


## plot, making assumption fertility is 0 at age 14
#par(mfrow=c(1,1), bty="l")
#plot(c(14,ages.of.fertility),c(0,fertility.per.1000.1995/1000),xlab="Ages", ylab="Fertility", type="l", xlim=c(0,50))
#points(c(14,ages.of.fertility),c(0,fertility.per.1000.2023/1000),type="l",lty=3)
#legend("topright",legend=c(1995,2023), lty=c(1,3),bty="n")



## inverse the plot, making assumption fertility is 0 at age 14
#par(mfrow=c(1,2), bty="l", mar=c(5,4,2,0))
#plot(c(14,ages.of.fertility)~c(0,-fertility.per.1000.1995/1000),ylab="", xlab="Fertility", type="l", ylim=c(0,50),axes=FALSE)
#points(c(14,ages.of.fertility)~c(0,-fertility.per.1000.2023/1000),type="l",lty=3)
#abline(v=0); axis(1, at=c(0,-0.05,-0.15,-0.25), lab=c(0,0.05,0.15,0.25)); axis(2)
#legend("topleft",legend=c(1995,2023), lty=c(1,3),bty="n")
#par(mar=c(5,0,2,4))
#plot(temp.test[temp.test>0],average.age.across.temp[temp.test>0], type="l", col=2,ylab="", ylim=c(0,50), xlab="Temperature",axes=FALSE)
#abline(v=0); axis(1); axis(4)


## get the risk of first infection in WCB for the two fertility profiles
ages.test <- 1:50; first.infection <- matrix(NA,length(temp.test),length(ages.test))
for (j in 1:length(temp.test)) first.infection[j,] <- (rate.human.infection.from.e*infected.env[j])*exp(-(rate.human.infection.from.e*infected.env[j])*ages.test)

#plot(temp.test,colSums(t(first.infection[,15:49])*(fertility.per.1000.1995)*number.women.1000s.1995),type="l", xlab="Temperature", ylab="# children at risk of Toxoplasmosis", ylim=c(0,15000))
#points(temp.test,colSums(t(first.infection[,15:49])*(fertility.per.1000.2023)*number.women.1000s.2023),type="l",lty=3)



#########################################
##### Figure for the proposal ###########

layout(matrix(c(1,2,3,3,4), 1, 5, byrow = FALSE))


### Plot of age against Igg, in Tana in 1995 from this: https://pathexo.societe-mtsi.fr/documents/articles-bull/BullSocPatholExot-1995-88-1-046-049.pdf
par(bty="l", mar=c(5,4,2,3))
plot(c(1,mid.ages),c(0,iggpos), type="p",xlab="Age (years)", ylab="Pregnant women previously infected", pch=c(1,rep(19,6)))
points(1:100,1-exp(-tmp$minimum*(1:100)),type="l", col="blue",lwd=2)
legend("topleft", legend="A)", bty="n")

### Plot of temperature against how much more likely oocysts are likely to die
plot(30+0:15,exp(fit1$coeff*(0:15)), type="l", xlab="Temperature", ylab="Prop. mortality hazard",log="y",col="blue",lwd=2)
points(30+0:15,exp((fit1$coeff+1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3,col="blue")
points(30+0:15,exp((fit1$coeff-1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3,col="blue")
legend("topleft", legend="B)", bty="n")

#### Plot fertility from https://population.un.org/wpp/Download/Standard/Fertility/
par(mar=c(5,4,2,3))
plot(c(14,ages.of.fertility),c(0,fertility.per.1000.1995/1000),ylab="Fertility", xlab="Age", type="l", xlim=c(0,50),lwd=2)
points(c(14,ages.of.fertility),c(0,fertility.per.1000.2023/1000),type="l",lty=3,lwd=2)
legend("topright",legend=c(1995,2023), lty=c(1,3),bty="n",lwd=2)
legend("topleft", legend="C)", bty="n")

#### Plot average age of infection obtained by linking the temperature in Tana to age curve and the equilibrium oocysts in Rasambainarivo 
## on top of this one
points(average.age.across.temp[temp.test>0],temp.test[temp.test>0]*0.27/50, type="l", col="red",ylab="", xlim=c(0,50), xlab="Temperature")
axis(2,at=c(0,10,20,30)*0.27/50,lab=c(0,10,20,30),col="red",line=-3,col.ticks="red", col.axis="red")

#### Plot out the CRS burden 

mother.child.transmission.ratio <- 0.44 #based on Milne et al. Trends in Parasitology 
first.trimester <- 1/3

par(mar=c(5,4,2,3))
plot(temp.test,colSums(first.trimester*mother.child.transmission.ratio *t(first.infection[,15:49])*(fertility.per.1000.1995)*number.women.1000s.1995),type="l",lwd=2, xlab="Temperature", ylab="# of cases of Congenital Toxoplasmosis", ylim=c(0,2100), xlim=c(15,40))
points(temp.test,colSums(first.trimester*mother.child.transmission.ratio *t(first.infection[,15:49])*(fertility.per.1000.2023)*number.women.1000s.2023),type="l",lwd=2,lty=3)
legend("topleft", legend="D)", bty="n")
#legend("bottomleft",legend=c(1995,2023), lty=c(1,3),bty="n",lwd=2)





### Figure caption: A) Serological testing of pregnant women in Antananarivo in 1995 (full points, [ref]) allows estimation of the force of infection  ($\lambda_h=0.07177068$), with the assumption that all 1 year olds are susceptible (empty circle); B) Success or failure of experimental infection of mice following exposure of occysts to a range of temperatures for variable durations provided in [ref] (n= 62, number of events=35, temperatures of 35, 40, 45, 50 and 55 degrees celsuis tested) can be fitted using a cox proporortional hazards model to estimate oocyst proportional mortality hazard across temperature (par=0.5145, se=0.1039, p<0.001), showing the mean (dark blue line) and confidence intervals (dashed lines). Setting the oocyst weekly mortality rate at 30 degree to 0.03, we obtain equilirbium densities for cat, rodent, and environment compartments within a dynamic model [ref: Fidy paper]. We use this to obtain an estimate of the transmission rate $\beta_h = \lambda_h/E_{30}$ making the simplifying assumption that dietary aquisition of infection will largely scale with this. C) This allows us to estimate the average age of human infection (x axis) across a specturm of temperatures (red y axis), and compare this to the age profile of fertility per 1000 for 1995 and 2023 (black lines). Once the temperature exceeds 12 degrees, the average age of infection shoots up, reflecting longer wait times to infection as a result of oocyst mortality; and the age of first infection shifts from being before the start of fertility (age 15) to during it (red line). D) The probability of first infection over age can be combined with the age profile of the population and the age profile of fertility to identify how many children are at risk of being born with congenital toxoplasmosis across a spectrum of temperatures; showing a sharp increase corresponding to higher temperatures (and thus lower oocyst survival and thus risk of infection, also termed 'the paradoxical increase for rubella).  


### Santiy check
# number of children born in Madagascar in a year in 2023: pop size x birth rate per 1000 #
30e6*31/1000

## what fraction of this are we saying have toxo? 
1500/(30e6*31/1000)  #this suggests that at the steady state of teh final graph, 1 child in 1000 is being born with CT? 


### Try reversal.  ##################################################

layout(matrix(c(1,2,3,3,4), 1, 5, byrow = FALSE))


### Plot of age against Igg, in Tana in 1995 from this: https://pathexo.societe-mtsi.fr/documents/articles-bull/BullSocPatholExot-1995-88-1-046-049.pdf
par(bty="l", mar=c(5,4,2,3))
plot(c(1,mid.ages),c(0,iggpos), type="p",xlab="Age (years)", ylab="Pregnant women previously infected", pch=c(1,rep(19,6)))
points(1:100,1-exp(-tmp$minimum*(1:100)),type="l", col="blue",lwd=2)
legend("topleft", legend="A)", bty="n")

### Plot of temperature against how much more likely oocysts are likely to die
plot(30+0:15,exp(fit1$coeff*(0:15)), type="l", xlab="Temperature", ylab="Prop. mortality hazard",log="y",col="blue",lwd=2)
points(30+0:15,exp((fit1$coeff+1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3,col="blue")
points(30+0:15,exp((fit1$coeff-1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3,col="blue")
legend("topleft", legend="B)", bty="n")

#### Plot fertility from https://population.un.org/wpp/Download/Standard/Fertility/
par(mar=c(5,4,2,3))
plot(c(14,ages.of.fertility)~c(0,fertility.per.1000.1995/1000),xlab="Temperature", ylab="Age", type="l", ylim=c(0,60),lwd=2, axes=FALSE, xlim=c(0,0.537))
points(c(14,ages.of.fertility)~c(0,fertility.per.1000.2023/1000),type="l",lty=3,lwd=2)
legend("topright",legend=c(1995,2023), lty=c(1,3),bty="n",lwd=2)
legend("topleft", legend="C)", bty="n")
axis(2)
axis(3,line=-6, at=c(0,0.05,0.1,0.15,0.20,0.25),lab=c(0,0.05,0.1,0.15,0.20,0.25))

#### Plot average age of infection obtained by linking the temperature in Tana to age curve and the equilibrium oocysts in Rasambainarivo 
## on top of this one
points(temp.test[temp.test>0]*0.67/50,average.age.across.temp[temp.test>0], type="l", col="red",ylab="", ylim=c(0,50))
axis(1,at=c(0,10,20,30,40)*0.67/50,lab=c(0,10,20,30,40))#,col="red",col.ticks="red", col.axis="red")

#### Plot out the CRS burden 

mother.child.transmission.ratio <- 0.44 #based on Milne et al. Trends in Parasitology 
first.trimester <- 1/3

par(mar=c(5,4,2,3))
plot(temp.test,colSums(first.trimester*mother.child.transmission.ratio *t(first.infection[,15:49])*(fertility.per.1000.1995)*number.women.1000s.1995),type="l",lwd=2, xlab="Temperature", ylab="# of cases of Congenital Toxoplasmosis", ylim=c(0,2100), xlim=c(15,40))
points(temp.test,colSums(first.trimester*mother.child.transmission.ratio *t(first.infection[,15:49])*(fertility.per.1000.2023)*number.women.1000s.2023),type="l",lwd=2,lty=3)
legend("topleft", legend="D)", bty="n")
legend("bottomleft",legend=c(1995,2023), lty=c(1,3),bty="n",lwd=2)





###########################################################
### Only show 2023 and start the temperatures at 15
#### CURRENT VERSION FOR THE GRANT ########################

par(mfrow=c(1,4), bty="l")

### Plot of age against Igg, in Tana in 1995 from this: https://pathexo.societe-mtsi.fr/documents/articles-bull/BullSocPatholExot-1995-88-1-046-049.pdf
par(bty="l", mar=c(5,4,2,3))
plot(c(1,mid.ages),c(0,iggpos), type="p",xlab="Age (years)", ylab="Pregnant women previously infected", pch=c(15,rep(19,6)))
points(1:100,1-exp(-tmp$minimum*(1:100)),type="l", col="black",lwd=2)
legend("topleft", legend="A)", bty="n", cex=1.5)

### Plot of temperature against how much more likely oocysts are likely to die
plot(30+0:15,exp(fit1$coeff*(0:15)), type="l", xlab="Temperature", ylab="Prop. mortality hazard",log="y",col="black",lwd=2)
points(30+0:15,exp((fit1$coeff+1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3,col="black")
points(30+0:15,exp((fit1$coeff-1.96*summary(fit1)$coeff[3])*(0:15)), type="l",lty=3,col="black")
legend("topleft", legend="B)", bty="n", cex=1.5)

#### Plot fertility from https://population.un.org/wpp/Download/Standard/Fertility/
par(mar=c(5,4,2,3))
plot(temp.test,average.age.across.temp, type="l", col="black",ylim=c(10,60), xlab="Temperature", ylab="Age", xlim=c(15,35),lwd=2)
points(c(14,ages.of.fertility)~c(15+0,15+40*fertility.per.1000.2023/1000), type="l", lty=3)
legend("topleft", legend="C)", bty="n", cex=1.5)
axis(2)
axis(3,line=-7, at=15+40*c(0,0.1,0.2),lab=c(0,0.1,0.2),lty=3)
text(15,56,"Fertility", pos=4)

#### Plot out the CRS burden 

mother.child.transmission.ratio <- 0.44 #based on Milne et al. Trends in Parasitology 
first.trimester <- 1/3

par(mar=c(5,4,2,3))
plot(temp.test,colSums(first.trimester*mother.child.transmission.ratio *t(first.infection[,15:49])*(fertility.per.1000.2023)*number.women.1000s.2023),type="l",lwd=2, xlab="Temperature", ylab="# of cases of Congenital Toxoplasmosis", ylim=c(0,2100), xlim=c(15,40))
legend("topleft", legend="D)", bty="n", cex=1.5)







