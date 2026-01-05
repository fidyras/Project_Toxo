
## Location of all of the data (change to your path)
fname <- "data/"


################ PART 1 - ESTIMATION OF THE FOI IN PEOPLE ACROSS COUNTIRES / YEARS #########################################################

## Bring in the data on seroprevalence in women
df <- read.csv(paste(fname,"Updated-data-added-Ingrid-Jun28-2025.csv", sep=""))


## Sum of squares minimizer to get the right coefficiet for the Force of Infection (FOI)
## Note that using 'optimize' to minimise, so provide it with minus the log likleihood
logLike <- function(par,xvals,yvals,tot){
	pred <- 1-exp(-par*xvals)
	u <- dbinom(yvals,tot,pred, log=TRUE)
	u[u==-Inf] <- -exp(200)
	return(-sum(u)) 
}


## Extract the publication names 
u.pub <- unique(df$Reference..authors..date..journal.)

## Generate files for storage
year <- country <- foi <- city <- rep(NA,length(u.pub))

## Plot out

par(mfrow=c(3,3))

## Loop over all of the publications and estimate the FOI if possible
for (j in 1:length(u.pub)) { 

	# pick out one of the studies (in order of u.pub)
	chs <- which(df$Reference..authors..date..journal.==u.pub[j],arr.ind=TRUE)
	lower.age <- as.numeric(df$Lower.bound.of.age[chs])
	upper.age <- as.numeric(df$Upper.bound.of.age[chs]) 

	# if the authors don't provide a lower age, assume 15 - they probably didn't actually measure pregnancy in women younger than that
	lower.age[is.na(lower.age) & df$Pregant.population[chs][1]==1] <- 15
	lower.age[is.na(lower.age) & df$Pregant.population[chs][1]==0] <- 0
	# if the authors don't provide an upper age, assume 50, since presumably very little pregnancy after that
	upper.age[is.na(upper.age) & df$Pregant.population[chs][1]==1] <- 50
	upper.age[is.na(upper.age) & df$Pregant.population[chs][1]==0] <- 100


	#Get variables required for fitting
	iggpos <- as.numeric(df$Number.of.people.seropositive[chs])
	tot <- as.numeric(df$Number.of.people.tested[chs])

	#Round for the one digitized from Cameroon
	iggpos <- round(iggpos)
	

	#allow oneself to add 0 at age 0 - just a single sample?  
	if (!is.na(lower.age[1])) {
	if (lower.age[1]!=0) { 
	  lower.age <- c(0,lower.age)
	  upper.age <- c(lower.age[1],upper.age)
	  iggpos <- c(0,iggpos)
	  tot <- c(1,tot) 
	}}	

	mid.ages <- 0.5*(lower.age+upper.age)

	#If this publication provides less than three points to fit the data to, skip to the next one
	if (sum(!is.na(iggpos))<3) next()
	if (sum(!is.na(tot))<3) next()


	print(cbind(mid.ages,iggpos,tot))
			
	# Otherwise run optim on the data to estimate the FOI
	tmp <-optimize(f=logLike,lower=1e-21,upper=1,xvals=mid.ages,yvals=iggpos,tot=tot)
	#print(tmp)

	# Store the estimated parameter, alongside the country/year  
	foi[j] <- tmp$minimum
	country[j] <- df$Location..country.[chs][1]
	year[j] <- as.numeric(substring(df$Year.of.data.collection..Final.Year.of.Data.Collection.[chs][1],1,4))
	city[j] <- df$Location..town.[chs][1]

	# Plot  
	plot(mid.ages,iggpos/tot,pch=19,xlab="age", ylab="seropos", ylim=c(0,1))
	points(1:100,1-exp(-foi[j]*c(1:100)), type="l")
	title(df$Location..country.[chs][1])
	legend("topleft",legend=j,bty="n") # The indicator j in the top left of plot is to help pinpoint any that look very strange. 

}
	


foi[foi>0.99] <- NA



################ PART 2 - COMPARE FOI WITH HDI AND TEMPERATURE #########################################################

## Bring in the HDI data by year
HDI2 <- read.csv(paste(fname, "human-development-index/human-development-index.csv",sep=""))
HDI2$Entity[HDI2$Entity =="Cote d'Ivoire"] <- "Ivory Coast"
HDI2$Entity[HDI2$Entity =="Sao Tome and Principe"] <- "Democratic Republic of São Tomé and Príncipe"
HDI2$Year <- as.numeric(HDI2$Year)

## Align this with the data organization from above 
hdi.match <- rep(NA,length(year))
for (j in 1:nrow(df)) {
   if (is.na(country[j])) next()
  chs <-  HDI2$Year==year[j] & HDI2$Entity==country[j]
  if (sum(chs)>0) hdi.match[j] <- HDI2$Human.Development.Index[chs]
  if (sum(chs)==0) hdi.match[j] <- HDI2$Human.Development.Index[HDI2$Entity==country[j] & HDI2$Year==2022]

}


## Bring in temperature data from Wenchang ######################
temps <- read.csv(paste(fname, "/climate-data-from-Wenchang/era5.skt.daily.Ingrid.1979-2024.degC.csv", sep=""))
country.temps <- city.temps <- rep(NA,ncol(temps)-1)
for (j in 2:ncol(temps)) { country.temps[j-1] <- substring(colnames(temps)[j],1,regexpr("_",colnames(temps)[j])[[1]]-1); 
			    city.temps[j-1] <-  substring(colnames(temps)[j],regexpr("_",colnames(temps)[j])[[1]]+1,nchar(colnames(temps)[j]))}
year.temperature <- as.numeric(substring(temps$time,1,4))
city.temps[city.temps=="SãoTomé"] <- "São Tomé"
city.temps[city.temps=="PenkaMichel"] <- "Penka-Michel"
city.temps[city.temps=="Burch.i"] <- "Burch'i"
city.temps[city.temps=="Jos.North"] <- "Jos–North"



#city.temps[city.temps=="DaresSalaam"] <- "Dar es Salaam"
#city.temps[city.temps=="BeninCity"] <- "Benin City"

## Find temperature for each year/country/city combo
min.temperatature.match <- mean.temperatature.match <- median.temperatature.match <- max.temperatature.match <- rep(NA,length(year)) 
for (j in 1:length(year)) {
	  if (is.na(country[j])) next()
	  row.chs <- which(year.temperature==year[j])
 	  col.chs <- which(city.temps== gsub(" ","",city[j])) 
	  if (length(col.chs)==0) col.chs <-  which(city.temps== gsub("-","",city[j])) 
	  if (length(col.chs)==0) col.chs <- which(country.temps ==country[j]) 

	  tmp.here <- c(as.matrix(temps[row.chs,col.chs]))	
	
	  mean.temperatature.match[j] <- mean(tmp.here)	
	  median.temperatature.match[j] <- median(tmp.here)	
	  max.temperatature.match[j] <- max(tmp.here)	
	  min.temperatature.match[j] <- min(tmp.here)	

}

mean.temperatature.match[!is.finite(mean.temperatature.match)] <- NA
max.temperatature.match[!is.finite(max.temperatature.match)] <- NA
median.temperatature.match[!is.finite(median.temperatature.match)] <- NA


# Transform both into direct variables easily entered into the statistical model 
hdi <- hdi.match
#tmpt <-  max.temperatature.match
#tmpt <-  mean.temperatature.match
#tmpt <-  median.temperatature.match
#tmpt <-  min.temperatature.match

# fit the model 
require(mgcv)
fit <- gam(foi~ hdi +s(tmpt))


## Plot the results
par(mfrow=c(1,3),bty="l")
hist(foi, xlab="Force of infection", col="grey" ,main="", cex.lab=1.5)
plot(hdi,foi, xlab="Human Development Index", ylab="FOI", pch=19, cex.lab=1.5)
test.hdi <- seq(0.3,1,length=100)
rc <- predict(fit,newdata=data.frame(hdi=test.hdi,tmpt=mean(tmpt,na.rm=TRUE)),se.fit = TRUE)
points(test.hdi,rc$fit, type="l")
points(test.hdi,rc$fit-1.96*rc$se.fit, type="l",lty=2)
points(test.hdi,rc$fit+1.96*rc$se.fit, type="l",lty=2)
plot(fit, xlab="Average temperature (C)", ylab="Smooth effect on FOI", cex.lab=1.5)


## Use 'predict' to figure out what the climate would be in future climates; and get the standard errors too

make.data.to.predict <- data.frame(hdi=0.5,tmpt=22:30)
pred <- predict(fit, newdata= make.data.to.predict,se.fit=TRUE)
pred 





################ PART 3 - PUT THIS IN THE CONTEXT OF A CHANGING AGE PROFILE OF FERTILITY as well as changing climate #########################################################
### USING OurWorldInData - here is the age specific fertility rate: https://ourworldindata.org/grapher/age-of-mothers-at-childbirth-by-year?country=~AGO 
### and here is the number of individuals per age group: https://ourworldindata.org/grapher/female-population-by-age-group    
## pick out the country and years you want (year options currently should be constrained to: 1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2023
chs.country <- "Madagascar"; yr.a <- 2000; yr.b <- 2023 

## Age specific fertility rate ########################################################
asfr <- read.csv(paste(fname,"age-of-mothers-at-childbirth-by-year.csv",sep=""))

## organize this data
ages.of.fertility <- asfr$Year
year.fertility <- c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2023)
index.column.year.fertility <- c(4:17)

## age specific fertility for the chosen country / year 
fertility.per.1000a <- asfr[asfr$Entity==chs.country & asfr$Year>14 & asfr$Year<50, index.column.year.fertility[year.fertility==yr.a]]
fertility.per.1000b <- asfr[asfr$Entity==chs.country & asfr$Year>14 & asfr$Year<50, index.column.year.fertility[year.fertility==yr.b]]



### Number of women in different age groups ########################################################
nwomen.per.age <- read.csv(paste(fname,"female-population-by-age-group.csv",sep=""))

l## organize this data (note that this has wider age classes)
ower.age <-  seq(0,99,by=5)
upper.age <-  c(lower.age[-1]-1,99)
mid.age <- c(0.5*(lower.age+upper.age),100)
number.women.a <- as.numeric(nwomen.per.age[nwomen.per.age$Entity==chs.country & nwomen.per.age$Year==yr.a,-c(1:3)])
number.women.b <- as.numeric(nwomen.per.age[nwomen.per.age$Entity==chs.country & nwomen.per.age$Year==yr.b,-c(1:3)])

## translate into finer age classes
pedictedNa <- predict(smooth.spline(number.women.a~ mid.age),15:49)$y
pedictedNb <- predict(smooth.spline(number.women.b~ mid.age),15:49)$y




## Pick out a range of temperatures to test across ########################################################
temp.test <- seq(15,40,length=60)
ages.test <- 15:50

## Get the risk of first infection in WCB for different FOIs
ages.test <- 1:50; first.infection <- matrix(NA,length(temp.test),length(ages.test))
for (j in 1:length(temp.test)) {
	make.data.to.predict <- data.frame(hdi=0.5,tmpt= temp.test[j]) ### TODO the HDI should match the coutnry / year
	pred <- max(as.numeric(predict(fit, newdata= make.data.to.predict,se.fit=FALSE)),0)
	first.infection[j,] <- pred*exp(-(pred)*ages.test)
}

## Translate this into the two years you are comparing 
resA <- colSums(t(first.infection[,15:49])*(fertility.per.1000a)*pedictedNa) ## TODO maybe double check all the units (is asfr per 1000? is pop size in n or in 1000s?)
resB <-colSums(t(first.infection[,15:49])*(fertility.per.1000b)*pedictedNb)



##### PLOT THIS OUT. #######################################

par(mfrow=c(1,3), bty="l")
plot(15:49,fertility.per.1000a, type="l", xlab="Age", ylab="Fertility", ylim=range(c(fertility.per.1000a, fertility.per.1000b)))
points(15:49,fertility.per.1000b, type="l",lty=3)
### Having babies later

plot(15:49, pedictedNa, type="l", xlab="Age", ylab="# individuals", ylim=range(c(pedictedNa,pedictedNb)))
points(15:49, pedictedNb, type="l",lty=3)
### But many more individuals

plot(temp.test,resA,type="l", xlab="Temperature", ylab="# children at risk of Toxoplasmosis", ylim=range(c(resA,resB)))
points(temp.test,resB,type="l",lty=3)
legend("topright",legend=c(yr.a,yr.b), bty="n",lty=c(1,3))
### See magnitude of these effects relative to effect of temperature


