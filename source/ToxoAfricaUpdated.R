
## Location of all of the data (change to your path)
fname <- "data/"



#### 1. BRING IN THE DATA ##################################################################################################################################################

## A. Data on seroprevalence  ###############
df <- read.csv(paste(fname,"Updated-data-added-Ingrid-Jun28-2025.csv", sep=""))

## Organize names - take the first year of the sero-surveys as our reference point
year <- as.numeric(substring(df$Year.of.data.collection..Final.Year.of.Data.Collection.,1,4))
country <- df$Location..country.
city <- df$Location..town
u.ctry <- unique(country)

## Extract the core variables for fitting
lower.age <- as.numeric(df$Lower.bound.of.age)
upper.age <- as.numeric(df$Upper.bound.of.age) 
iggpos <- as.numeric(df$Number.of.people.seropositive)
tot <- as.numeric(df$Number.of.people.tested)

#Assume that upper age for missing and population reported as reproductive women is 50 (~menopause)
upper.age[is.na(upper.age) & df$Women.of.Reproductive.age==1] <- 50

#Round for the one digitized from Cameroon
iggpos <- round(iggpos)

#Get midpoint of the age groups
mid.ages <- 0.5*(lower.age+upper.age)






## B. HDI data by year / country ################################################################################################################################################
HDI2 <- read.csv(paste(fname, "human-development-index/human-development-index.csv",sep=""))
HDI2$Entity[HDI2$Entity =="Cote d'Ivoire"] <- "Ivory Coast"		#change names to match data file
HDI2$Entity[HDI2$Entity =="Sao Tome and Principe"] <- "Democratic Republic of São Tomé and Príncipe"
HDI2$Year <- as.numeric(HDI2$Year)

## Align the full data-frame with the country/year combinations from the serology dataset
hdi.match <- rep(NA,length(year))
for (j in 1:length(year)) {
   if (is.na(country[j])) next()
   if (is.na(year[j])) next()
  chs <-  HDI2$Year==year[j] & HDI2$Entity==country[j]
  if (sum(chs)>0) hdi.match[j] <- HDI2$Human.Development.Index[chs]
  if (sum(chs)==0) hdi.match[j] <- HDI2$Human.Development.Index[HDI2$Entity==country[j] & HDI2$Year==2022] #if no data available, take the most recent

}

#store the most recent values for each country for future projection once the statistical model is fitted
recent.hdi <- rep(NA,length(u.ctry)) 
for (j in 1:length(u.ctry)) recent.hdi[j] <-HDI2$Human.Development.Index[HDI2$Year==2022 & HDI2$Entity==country[j]]








## C. Temperature data  #######################################################################################################################################################
temps <- read.csv(paste(fname, "/climate-data-from-Wenchang/era5.skt.daily.Ingrid.1979-2024.degC.csv", sep=""))

# extract names of countries and cities
country.temps <- city.temps <- rep(NA,ncol(temps)-1)
for (j in 2:ncol(temps)) { country.temps[j-1] <- substring(colnames(temps)[j],1,regexpr("_",colnames(temps)[j])[[1]]-1); 
			    city.temps[j-1] <-  substring(colnames(temps)[j],regexpr("_",colnames(temps)[j])[[1]]+1,nchar(colnames(temps)[j]))}

# extract years
year.temperature <- as.numeric(substring(temps$time,1,4))

# match the names to the names in the serology data-set
city.temps[city.temps=="SãoTomé"] <- "São Tomé"
city.temps[city.temps=="PenkaMichel"] <- "Penka-Michel"
city.temps[city.temps=="Burch.i"] <- "Burch'i"
city.temps[city.temps=="Jos.North"] <- "Jos–North"

country.temps[country.temps=="BurkinaFaso"] <- "Burkina Faso"
country.temps[country.temps=="DemocraticRepublicofSãoToméandPríncipe"] <- "Democratic Republic of São Tomé and Príncipe"
country.temps[country.temps=="IvoryCoast"] <- "Ivory Coast"


## Find temperature for each year/country/city combo
min.temperature.match <- mean.temperature.match <- median.temperature.match <- max.temperature.match <- rep(NA,length(year)) 
for (j in 1:length(year)) {
	  if (is.na(country[j])) next()
	  
	   ## find the right row
	   row.chs <- which(year.temperature==year[j])

	   ## if the data year is lower than the minimum year, set to 1979 (for Mauritania, not too big a different)
	   if (length(row.chs)==0) row.chs <- which(year.temperature==max(year[j],1979))
 		
	   ## find the right column
 	  col.chs <- which(city.temps== gsub(" ","",city[j])) 
	  if (length(col.chs)==0) col.chs <-  which(city.temps== gsub("-","",city[j])) 
	  if (length(col.chs)==0) col.chs <- which(country.temps ==country[j]) 

	  #indexing - note, need to add one to the column index to account for the first colum of time. 
	  tmp.here <- as.numeric(c(as.matrix(temps[row.chs,col.chs+1])))	
	
	  mean.temperature.match[j] <- mean(tmp.here)	
	  median.temperature.match[j] <- median(tmp.here)	
	  max.temperature.match[j] <- max(tmp.here)	
	  min.temperature.match[j] <- min(tmp.here)	

}

## Remove NAs / infinities
mean.temperature.match[!is.finite(mean.temperature.match)] <- NA
max.temperature.match[!is.finite(max.temperature.match)] <- NA
median.temperature.match[!is.finite(median.temperature.match)] <- NA
min.temperature.match[!is.finite(min.temperature.match)] <- NA



## Get one value per country for most recent measurements (2024) and earliest (1979) for future projection
early.mean.temp <- early.max.temp <- early.min.temp <- rep(NA,length(u.ctry))
recent.mean.temp <- recent.max.temp <- recent.min.temp <- rep(NA,length(u.ctry))
mean.temp.u.ctry <- max.temp.u.ctry <- min.temp.u.ctry <- rep(NA,length(u.ctry))
for (j in 1:length(u.ctry)) { 
	col.chs <- which(country.temps ==u.ctry[j],arr.ind=TRUE)
        row.chs <- which(year.temperature==2024,arr.ind=TRUE)
        row.chs2 <- which(year.temperature==1979,arr.ind=TRUE)

	mean.temp.u.ctry[j] <- mean(as.numeric(c(as.matrix(temps[,col.chs+1]))))	
	max.temp.u.ctry[j] <- max(as.numeric(c(as.matrix(temps[,col.chs+1]))))	
	min.temp.u.ctry[j] <- min(as.numeric(c(as.matrix(temps[,col.chs+1]))))	


	recent.mean.temp [j] <- mean(as.numeric(c(as.matrix(temps[row.chs,col.chs+1]))))	
	recent.max.temp[j] <- max(as.numeric(c(as.matrix(temps[row.chs,col.chs+1]))))	
	recent.min.temp[j] <- min(as.numeric(c(as.matrix(temps[row.chs,col.chs+1]))))	

	early.mean.temp [j] <- mean(as.numeric(c(as.matrix(temps[row.chs2,col.chs+1]))))	
	early.max.temp[j] <- max(as.numeric(c(as.matrix(temps[row.chs2,col.chs+1]))))	
	early.min.temp[j] <- min(as.numeric(c(as.matrix(temps[row.chs2,col.chs+1]))))	

}




## Get most recent scores (for current burden) and order by unique country again for future projection
recent.data <- data.frame(country=u.ctry, recent.hdi= recent.hdi,
	recent.mean.temp=recent.mean.temp,recent.max.temp=recent.max.temp,recent.min.temp=recent.min.temp,
	early.mean.temp=early.mean.temp,early.max.temp=early.max.temp,early.min.temp=early.min.temp,  
		mean.temp= mean.temp.u.ctry, max.temp= max.temp.u.ctry, min.temp= min.temp.u.ctry)

recent.data <- recent.data[order(recent.data[,1]),]






## D. Fertility over age ######################
### Data is from UN population projections: https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Fertility
### Births by single age of mother annually in 1000s - now and in future (so presumably correcting for changing populations)


## past and current estimates
estimate.births <- read.csv(paste(fname,"/UNdataFertPop/Estimates.births.by.age.mother.csv",sep=""))
year.estimate <- estimate.births$Year
births.per.age <- estimate.births[,12:ncol(estimate.births)]

## Adjust names to have a match between the different data sets
estimate.births$Region..subregion..country.or.area..[estimate.births $Region..subregion..country.or.area..=="United Republic of Tanzania"] <- "Tanzania"
estimate.births$Region..subregion..country.or.area..[estimate.births $Region..subregion..country.or.area..=="Côte d'Ivoire"] <- "Ivory Coast"
estimate.births$Region..subregion..country.or.area..[estimate.births $Region..subregion..country.or.area..=="Sao Tome and Principe"] <- "Democratic Republic of São Tomé and Príncipe"


## Future medium variant estimates
median.future.births <- read.csv(paste(fname,"/UNdataFertPop/MediumVariantPorjection.births.by.age.mother.csv",sep=""))
year.median <- median.future.births$Year
births.per.age.future <- median.future.births[,12:ncol(median.future.births)]


## Adjust names to have a match between the different data sets
median.future.births$Region..subregion..country.or.area..[median.future.births $Region..subregion..country.or.area..=="United Republic of Tanzania"] <- "Tanzania"
median.future.births$Region..subregion..country.or.area..[median.future.births $Region..subregion..country.or.area..=="Côte d'Ivoire"] <- "Ivory Coast"
median.future.births$Region..subregion..country.or.area..[median.future.births $Region..subregion..country.or.area..=="Sao Tome and Principe"] <- "Democratic Republic of São Tomé and Príncipe"










#### 2. CREATE A DATA FRAME WWITH PUBLICATION, POS, TOT, AGE, TEMPERATURE, HDI ###############################################################################################################

## Here framed using the midage as the offset [ test sensitivity to using the upper age, substitute in log(upper.age) here ]
new.data <- data.frame(lower.age=lower.age,upper.age=upper.age,Pos=iggpos,Neg=tot-iggpos,tot=tot,lAge=log(mid.ages),#lAge=log(upper.age),
			year=year, country= country,city=city,hdi= hdi.match,
			mean.temperature=mean.temperature.match, max.temperature= max.temperature.match,
			median.temperature= median.temperature.match, min.temperature= min.temperature.match)


## convenience vector indicating which rows are 'bad', i.e., missing values
bad <- is.na(new.data$Pos) | is.na(new.data$Neg) | is.na(new.data$lAge) | !is.finite(new.data$lAge) | is.na(new.data$median.temperature)

## convenience vectors for projection, and unique country index - this will be used in future ordering of data
test.hdi <- seq(0.3,1,length=100)
test.temp <- seq(14,35,length=100)
test.min.temp <- seq(1,26,length=100)
test.max.temp <- seq(20,42,length=100)
u.ctry <- unique(new.data$country[!bad])[order(unique(new.data$country[!bad]))]

## eliminate the countries that don't make it from the 'recent.data' that will be used for projection
recent.data <- recent.data[is.element(recent.data[,1],u.ctry),]

## Index the countries that we are focussed on in the fertility data (with the updated u.ctry)
## Important: the ordering of u.ctry must not change for this to work
index.u.ctry.in.estimate <- index.u.ctry.in.future <- rep(NA,nrow(estimate.births))
for (j in 1:length(u.ctry)) { 
	index.u.ctry.in.estimate[u.ctry[j]==estimate.births$Region..subregion..country.or.area..] <- j 
	index.u.ctry.in.future[u.ctry[j]==median.future.births$Region..subregion..country.or.area..] <- j 
}
## sanity check
#estimate.births[index.u.ctry.in.estimate==22 & !is.na(index.u.ctry.in.estimate),]








## FiGURES OF THE DATA  #################################

## A. SEROPREV - stand alone Figure 3
par(mfrow=c(1,1),bty="n", mar=c(5,5,4,1))
cols <- colorRampPalette(c("blue", "red"))(24)
plot(exp(new.data$lAge[!bad]),new.data$Pos[!bad]/(new.data$tot[!bad]),  pch=19, xlab="Age class midpoint", 
	ylab="Propoertion IgG Positive", col=cols[round(new.data$max.temperature[!bad])-18], cex=new.data$hdi[!bad]*2, cex.lab=1.5)
legend("topright",legend=c(19,30,42), col=cols[c(1,12,24)],pch=15, title="Max T (C)", bty="n", cex=1.5)
#title("A) Seroprevalence data")





## B. TEMPERATURE - not presented in manuscript
par(mfrow=c(1,1),bty="n", mar=c(10,5,4,1))
o.temp <- order(recent.data$mean.temp)
plot(c(1:length(u.ctry)), recent.data$mean.temp[o.temp], ylim=range(c(recent.data$mean.temp, recent.data$min.temp, recent.data$max.temp),na.rm=T), xlab="", ylab="Temperature (C)", pch=1, axes=F, type="n", cex.lab=1.5)
points(c(1:length(u.ctry))-0.25, recent.data$early.mean.temp[o.temp], pch=19,col="grey")
points(c(1:length(u.ctry))+0.25, recent.data$recent.mean.temp[o.temp], pch=19,col="black")

for (j in  c(1:length(u.ctry))) { 
	#points(c(j,j), c(recent.data$min.temp[o.temp][j], recent.data$max.temp[o.temp][j]),lty=1, type="l", col="black",lwd=0.5)
	points(c(j,j)-0.25, c(recent.data$early.min.temp[o.temp][j], recent.data$early.max.temp[o.temp][j]),lty=1, type="l", col="grey",lwd=0.5)
	points(c(j,j)+0.25, c(recent.data$recent.min.temp[o.temp][j], recent.data$recent.max.temp[o.temp][j]),lty=1, type="l", col="black",lwd=0.5)
	}


axis(1,at=c(1:length(u.ctry)),lab=u.ctry[o.temp],las=2); axis(2)
title("B) Temperature ranges")
#legend("bottomright",legend=c("Average","1979","2023"), pch=c(1,19,19),col=c(1,"grey",1),bty="n")
legend("bottomright",legend=c("1979","2023"), pch=c(19,19),col=c("grey",1),bty="n")


## Presented in the supplementary data (Figure S3) 
par(mfrow=c(1,1),bty="l")
plot(new.data$min.temperature[!bad], new.data$max.temperature[!bad], pch=19, xlab="Minimum temperature", ylab="Maximum temperature", cex.lab=1.5)#, type="n")
#text(jitter(new.data$min.temperature[!bad]), new.data$max.temperature[!bad], new.data$country, cex=0.3)




## C. Fertility - not presented in manuscript 
par(mfrow=c(1,1),bty="n", mar=c(10,5,4,1))
j <- 23#12
births.2023 <- as.numeric(births.per.age[index.u.ctry.in.estimate==j & !is.na(index.u.ctry.in.estimate) & year.estimate==2023,])
births.2050 <- as.numeric(births.per.age.future[index.u.ctry.in.future ==j & !is.na(index.u.ctry.in.future) & year.median ==2050,])
births.2100 <- as.numeric(births.per.age.future[index.u.ctry.in.future ==j & !is.na(index.u.ctry.in.future) & year.median ==2100,])

yupper <- max(c(births.2023, births.2050, births.2100))
plot(15:49, births.2023,xlab="Age", ylab="Births per year age (1000s)", type="l",ylim=c(0,yupper), cex.lab=1.5)
points(15:49, births.2050,type="l",lty=2)
points(15:49, births.2100,type="l",lty=3)
title("C) Fertility trajectory example")
#title(u.ctry[j])
legend("topright",legend=c(2023,2050,2100), title=u.ctry[j],lty=c(1,2,3),bty="n")







#### 3. FIT THE MODEL ###########################################################################################################################################################################


require(mgcv)

## make country a factor in order to add random effect
new.data$country <- as.factor(new.data$country)

## Assume: linear on min and max temperature and HDI; random effect of country 
fit <- gam(cbind(Pos,Neg)~hdi+max.temperature+min.temperature+s(country,bs="re"),offset=lAge,family=binomial(link=cloglog), data=new.data[!bad,])

## Comparison with simpler models to make sure that we need - i) any temperature; ii) max as well as min  
fit.notemp <- gam(cbind(Pos,Neg)~hdi+s(country,bs="re"),offset=lAge,family=binomial(link=cloglog), data=new.data[!bad,])
fit.maxtemp <- gam(cbind(Pos,Neg)~hdi+max.temperature+s(country,bs="re"),offset=lAge,family=binomial(link=cloglog), data=new.data[!bad,])
anova(fit.notemp,fit.maxtemp,fit,test="Chisq") ## does seem to improve


## Check error distribution 
plot(fit) 


## Plot across the range of variables, others set to mean; and country is Somalia (closest to average random effect); note that lAge is set to 18, but predict does not include the offset
## Included as Figure 4 

rc.hdi <- predict(fit,newdata=data.frame(hdi=test.hdi, max.temperature =mean(new.data$max.temperature[!bad],na.rm=TRUE), 
		min.temperature =mean(new.data$min.temperature[!bad],na.rm=TRUE),country=as.factor("Somalia"),lAge=log(18)),se.fit = TRUE)
rc.min.temp <- predict(fit,newdata=data.frame(hdi=0.5, min.temperature =test.min.temp,max.temperature=mean(new.data$max.temperature[!bad],na.rm=TRUE),country=as.factor("Somalia"),lAge=log(18)),se.fit = TRUE)
rc.max.temp <- predict(fit,newdata=data.frame(hdi=0.5, max.temperature =test.max.temp, min.temperature =mean(new.data$min.temperature[!bad],na.rm=TRUE),country=as.factor("Somalia"),lAge=log(18)),se.fit = TRUE)
rc.ctry <- predict(fit,newdata=data.frame(hdi=0.5, max.temperature =mean(new.data$max.temperature[!bad],na.rm=TRUE), min.temperature =mean(new.data$min.temperature[!bad],na.rm=TRUE),country=as.factor(u.ctry),lAge=log(18)),se.fit = TRUE)

## convenience to allow toggle between different outputs - need exp since getting FOI on the log scale 
conv <- function(x) return(exp(x)) 
#conv <- function(x) return(x)

ylims <- c(0.007,0.028)

## Show results
par(mfrow=c(1,3), mar=c(5,5,4,1), bty="l")
plot(test.hdi,conv(rc.hdi$fit), type="l", xlab="Human Development Index", ylab="Force of infection", pch=19, cex.lab=1.5,lwd=2,cex.axis=1.25, ylim=ylims)
polygon(c(test.hdi,test.hdi[length(test.hdi)],test.hdi[length(test.hdi):1]),
	c(conv(rc.hdi$fit-1.96*rc.hdi$se.fit)[c(1:length(test.hdi),length(test.hdi))],conv(rc.hdi$fit+1.96*rc.hdi$se.fit)[length(test.hdi):1]), col="lightgrey", border=NA)
points(test.hdi,conv(rc.hdi$fit), type="l")
points(new.data$hdi[!bad],rep(ylims[1],nrow(new.data[!bad,])),pch=3, cex=0.5) #tickmarks showing where data is

par(mar=c(5,3,4,1), bty="l")

plot(test.max.temp,conv(rc.max.temp$fit), type="l", xlab="Max temperature (C)", ylab="", pch=19, cex.lab=1.5,lwd=2, cex.axis=1.25,ylim=ylims)
polygon(c(test.max.temp,test.max.temp[length(test.max.temp)],test.max.temp[length(test.max.temp):1]),
	c(conv(rc.max.temp$fit-1.96*rc.max.temp$se.fit)[c(1:length(test.max.temp),length(test.max.temp))],conv(rc.max.temp$fit+1.96*rc.max.temp$se.fit)[length(test.max.temp):1]), col="lightgrey", border=NA)
points(test.max.temp,conv(rc.max.temp$fit), type="l")
points(new.data$max.temperature[!bad],rep(ylims[1],nrow(new.data[!bad,])),pch=3, cex=0.5) #tickmarks showing where data is


plot(test.min.temp,conv(rc.min.temp$fit), type="l", xlab="Min temperature (C)", ylab="", pch=19, cex.lab=1.5,lwd=2,cex.axis=1.25, ylim=ylims)
polygon(c(test.min.temp,test.min.temp[length(test.min.temp)],test.min.temp[length(test.min.temp):1]),
	c(conv(rc.min.temp$fit-1.96*rc.min.temp$se.fit)[c(1:length(test.min.temp),length(test.min.temp))],conv(rc.min.temp$fit+1.96*rc.min.temp$se.fit)[length(test.min.temp):1]), col="lightgrey", border=NA)
points(test.min.temp,conv(rc.min.temp$fit), type="l")
points(new.data$min.temperature[!bad],rep(ylims[1],nrow(new.data[!bad,])),pch=3, cex=0.5) #tickmarks showing where data is





### New Figure S4 repreesnting the random effects associated with the model fit 

par(mfrow=c(1,1), mar=c(10,5,4,1), bty="l")
order.c <- order(rc.ctry$fit)
plot(1:length(u.ctry),conv(rc.ctry$fit[order.c]), type="p", xlab="", ylab="Force of infection", pch=19, cex.lab=1.5,lwd=2, axes=FALSE)#, ylim=ylims)
for (j in 1:length(u.ctry)) points(c(j,j),conv(rc.ctry$fit[order.c][j]+c(1.96,-1.96)*rc.ctry$se.fit[order.c][j]), type="l",lty=2)
axis(1,at=1:length(u.ctry),lab=u.ctry[order.c],las=2, cex.lab=0.5); axis(2)
## cbind(u.ctry,coef(fit)[5:length(coef(fit))])[order(coef(fit)[5:length(coef(fit))]),] #sanity check this matches what you get with predict for country level



## Figure not provided in manuscript - check effect of median temperatures - this is rather unstable, which is why went with Max and Min  
test.median.temp <- seq(15,37,length=100)
fit.test <- gam(cbind(Pos,Neg)~hdi+s(median.temperature)+s(country,bs="re"),offset=lAge,family=binomial(link=cloglog), data=new.data[!bad,])
rc.median.temp <- predict(fit.test,newdata=data.frame(hdi=0.5, median.temperature = test.median.temp,country=as.factor("Somalia"),lAge=log(18)),se.fit = TRUE)
par(mfrow=c(1,1))
plot(test.median.temp,conv(rc.median.temp $fit), type="l", xlab="Median temperature (C)", ylab="Force of infection", pch=19, cex.lab=1.5,lwd=2)
points(test.median.temp,conv(rc.median.temp$fit-1.96*rc.median.temp$se.fit), type="l",lty=2)
points(test.median.temp,conv(rc.median.temp$fit+1.96*rc.median.temp$se.fit), type="l",lty=2)





#### 4. ESTIMATE THE BURDEN ###########################################################################################################################################################################
## Note that Sao Tome and Principe contains no data on Babies (country too small) so does not appear in subsequent models 


model.now <-  gam(cbind(Pos,Neg)~hdi+max.temperature+min.temperature+s(country,bs="re"),offset=lAge,family=binomial(link=cloglog), data=new.data[!bad,])


##orders and checks: 
recent.data <- recent.data[order(recent.data[,1]),]
#cbind(recent.data[,1],u.ctry,rownames(ranef(model.now)$country))

## storage
burdens.adj <- burdens <- matrix(NA,length(u.ctry),12)
calc.change.mean.age.birth <- rep(NA,length(u.ctry))

par(mfrow=c(1,4), bty="l")
yupper <- 60 ##upper bound for plots of countries births per age class


## Loop over 
for (j in 1:length(u.ctry)) { 

	## relies on order u.ctry being same as in model.now and in recent.data; and there obly being 4 fixed effects (intercept, slope1,2,3); 
	## can check order of coefficients in model.now by doing levels(new.data$country)	
	country.effect <- coef(model.now)[5:length(coef(model.now))][j]

	## get current age profile risk
	risk.now <- exp(coef(model.now)[1]+coef(model.now)[2]*recent.data$recent.hdi[j]+
					coef(model.now)[3]*recent.data$recent.max.temp[j]+coef(model.now)[4]*recent.data$recent.min.temp[j]+ country.effect)
	staying.susceptible <- exp(-risk.now*(15:49))
	first.infection.at.age <- risk.now* staying.susceptible

	## get same with 3 degree increase in maximum
	risk.now.hot <- exp(coef(model.now)[1]+coef(model.now)[2]*recent.data$recent.hdi[j]+
					coef(model.now)[3]*(recent.data$recent.max.temp[j]+3)+coef(model.now)[4]*recent.data$recent.min.temp[j]+ country.effect)
	staying.susceptible.hot <- exp(-risk.now.hot*(15:49))
	first.infection.at.age.hot <- risk.now.hot* staying.susceptible.hot

	## get same with 3 degree increase in minimum
	risk.now.minimum.hot <- exp(coef(model.now)[1]+coef(model.now)[2]*recent.data$recent.hdi[j]+
					coef(model.now)[3]*(recent.data$recent.max.temp[j])+coef(model.now)[4]*(recent.data$recent.min.temp[j]+3)+ country.effect)
	staying.susceptible.minimum.hot <- exp(-risk.now.minimum.hot*(15:49))
	first.infection.at.age.minimum.hot <- risk.now.minimum.hot*staying.susceptible.minimum.hot


	## get same with 3 degree increase in both
	risk.now.both.hot <- exp(coef(model.now)[1]+coef(model.now)[2]*recent.data$recent.hdi[j]+
					coef(model.now)[3]*(recent.data$recent.max.temp[j]+3)+coef(model.now)[4]*(recent.data$recent.min.temp[j]+3)+ country.effect)
	staying.susceptible.both.hot <- exp(-risk.now.both.hot*(15:49))
	first.infection.at.age.both.hot <- risk.now.both.hot*staying.susceptible.both.hot

		
	## get births in the future
	births.2023 <- as.numeric(births.per.age[index.u.ctry.in.estimate==j & !is.na(index.u.ctry.in.estimate) & year.estimate==2023,])
	births.2050 <- as.numeric(births.per.age.future[index.u.ctry.in.future ==j & !is.na(index.u.ctry.in.future) & year.median ==2050,])
	births.2100 <- as.numeric(births.per.age.future[index.u.ctry.in.future ==j & !is.na(index.u.ctry.in.future) & year.median ==2100,])


	## Store
	burdens[j,] <- c(sum(first.infection.at.age*births.2023), 	#burden now
			sum(first.infection.at.age*births.2050), 	#burden 2050
			sum(first.infection.at.age*births.2100), 	#burden 2100

			sum(first.infection.at.age.hot*births.2023), 	#burden now max hotter
			sum(first.infection.at.age.hot*births.2050), 	#burden 2050
			sum(first.infection.at.age.hot*births.2100), 	#burden 2100

			sum(first.infection.at.age.minimum.hot*births.2023), 	#burden now minimum hotter
			sum(first.infection.at.age.minimum.hot*births.2050), 	#burden 2050
			sum(first.infection.at.age.minimum.hot*births.2100), 	#burden 2100

			sum(first.infection.at.age.both.hot*births.2023), 	#burden now both hotter
			sum(first.infection.at.age.both.hot*births.2050), 	#burden 2050
			sum(first.infection.at.age.both.hot*births.2100) 	#burden 2100
			)


	## Store burdens adjust by totals 
	burdens.adj[j,] <- c(sum(first.infection.at.age*births.2023)/sum(births.2023), 	#burden now
			sum(first.infection.at.age*births.2050)/sum(births.2050), 	#burden 2050
			sum(first.infection.at.age*births.2100)/sum(births.2100), 	#burden 2100

			sum(first.infection.at.age.hot*births.2023)/sum(births.2023), 	#burden now max hotter
			sum(first.infection.at.age.hot*births.2050)/sum(births.2050), 	#burden 2050
			sum(first.infection.at.age.hot*births.2100)/sum(births.2100), 	#burden 2100

			sum(first.infection.at.age.minimum.hot*births.2023)/sum(births.2023), 	#burden now minimum hotter
			sum(first.infection.at.age.minimum.hot*births.2050)/sum(births.2050), 	#burden 2050
			sum(first.infection.at.age.minimum.hot*births.2100)/sum(births.2100), 	#burden 2100

			sum(first.infection.at.age.both.hot*births.2023)/sum(births.2023), 	#burden now both hotter
			sum(first.infection.at.age.both.hot*births.2050)/sum(births.2050), 	#burden 2050
			sum(first.infection.at.age.both.hot*births.2100)/sum(births.2100)	#burden 2100
			)

	calc.change.mean.age.birth[j] <- ((sum(births.2100*(15:49))/sum(births.2100))-(sum(births.2023*(15:49))/sum(births.2023)))#/sum(births.2100*(15:49))/sum(births.2100)
	

	## Contrast fertility patterns of the extremes (demography and one temperature; not Eswatini, just too too small to be interesting looking)
	if (j==23 | j==13 | j==24 | j==17 ) {

		yupper <- max(c(births.2023, births.2050, births.2100))

 		plot(15:49, births.2023,xlab="Age", ylab="Births per year age (1000s)", type="l",ylim=c(0,yupper))
		points(15:49, births.2050,type="l",lty=2)
		points(15:49, births.2100,type="l",lty=3)
		title(u.ctry[j])

		#points(15:49, yupper*first.infection.at.age/max(first.infection.at.age), type="l", col="grey",lwd=2)
		#points(15:49, yupper*first.infection.at.age.hot/max(first.infection.at.age.hot), type="l", col="coral",lwd=2)

		#susceptible is more interpretable?
		#points(15:49, yupper*(1-staying.susceptible), type="l", col="grey",lwd=2)
		#points(15:49, yupper*(1-staying.susceptible.both), type="l", col="red",lwd=2)
		
		#axis(4, at=c(0,0.25,0.5,0.75,1)*yupper, lab=c(0,0.25,0.5,0.75,1),lwd=2)
	}


}
#legend("topright",legend=c(2023,2050,2100), lty=c(1,2,3),bty="n")



### Plot out the results ##
colnames(burdens) <- c("2023","2050","2100","2023maxhotter","2050maxhotter","2100maxhotter","2023minhotter","2050minhotter","2100minhotter","2023bothhotter","2050bothhotter","2100bothhotter")
cols <- c("black","coral","orange","red")
chk <- c(-0.2,0,0.2,0.4)

o1<- order(burdens[,3]/burdens[,1])

par(mfrow=c(1,1), mar=c(5,10,2,2))
plot(c(burdens[o1,2]/burdens[o1,1]), 1:length(o1),ylab="", xlab="Relative future burden",pch=19,type="n",xlim=c(0.5,2), ylim=c(0.5,length(o1)-1+0.1),axes=FALSE)
axis(2, at=1:length(o1),lab=u.ctry[o1],las=2);axis(1)
abline(v=1)
#legend("bottomright",legend=c("None", "Max only", "Min only", "Both"), title="Temp. increase", col=cols,pch=19, bty="n")
legend("bottomright",legend=c("None", "Max only", "Min only"), title="Temp. increase", col=cols[-4],pch=19, bty="n")
legend("right",legend=c("2050", "2100"), title="Forecast rel. burden", col=1,pch=c(19,1), bty="n")
abline(h=c(1:length(o1)), col="grey",lwd=0.5)

## demography
points(c(burdens[o1,2]/burdens[o1,1]),1:length(o1)+chk[1],pch=19) 
points(c(burdens[o1,3]/burdens[o1,1]),1:length(o1)+chk[1],pch=1) 
for (j in 1:length(o1)) points(c(burdens[o1[j],2]/burdens[o1[j],1],burdens[o1[j],3]/burdens[o1[j],1]),c(j,j)+chk[1], type="l",lty=1)

## heat max
points(c(burdens[o1,5]/burdens[o1,1]),1:length(o1)+chk[2],pch=19, col=cols[2]) 
points(c(burdens[o1,6]/burdens[o1,1]),1:length(o1)+chk[2],pch=1, col=cols[2]) 
for (j in 1:length(o1)) points(c(burdens[o1[j],5]/burdens[o1[j],1],burdens[o1[j],6]/burdens[o1[j],1]),c(j,j)+chk[2], type="l",lty=1, col=cols[2])

## heat min
points(c(burdens[o1,8]/burdens[o1,1]),1:length(o1)+chk[3],pch=19, col=cols[3]) 
points(c(burdens[o1,9]/burdens[o1,1]),1:length(o1)+chk[3],pch=1, col=cols[3]) 
for (j in 1:length(o1)) points(c(burdens[o1[j],8]/burdens[o1[j],1],burdens[o1[j],9]/burdens[o1[j],1]),c(j,j)+chk[3], type="l",lty=1, col=cols[3])

## heat both
#points(c(burdens[o1,11]/burdens[o1,1]),1:length(o1)+chk[4],pch=19, col=cols[4]) 
#points(c(burdens[o1,12]/burdens[o1,1]),1:length(o1)+chk[4],pch=1, col=cols[4]) 
#for (j in 1:length(o1)) points(c(burdens[o1[j],11]/burdens[o1[j],1],burdens[o1[j],12]/burdens[o1[j],1]),c(j,j)+chk[4], type="l",lty=1, col=cols[4])






## Look at proportions and see if the same

o1<- order(burdens.adj[,3]/burdens.adj[,1])


par(mfrow=c(1,1), mar=c(5,10,2,2))
plot(c(burdens.adj[o1,2]/burdens.adj[o1,1]), 1:length(o1),ylab="", xlab="Relative future burden in proportion CT births",pch=19,type="n",xlim=c(0.81,1.19), ylim=c(0.5,length(o1)-1+0.1),axes=FALSE)
axis(2, at=1:length(o1),lab=u.ctry[o1],las=2);axis(1)
abline(v=1)
#legend("bottomright",legend=c("None", "Max only", "Min only", "Both"), title="Temp. increase", col=cols,pch=19, bty="n")
legend("bottomright",legend=c("None", "Max only", "Min only"), title="Temp. increase", col=cols[-4],pch=19, bty="n")
legend("right",legend=c("2050", "2100"), title="Year", col=1,pch=c(19,1), bty="n")
abline(h=c(1:length(o1)), col="grey",lwd=0.5)

## demography
points(c(burdens.adj[o1,2]/burdens.adj[o1,1]),1:length(o1)+chk[1],pch=19) 
points(c(burdens.adj[o1,3]/burdens.adj[o1,1]),1:length(o1)+chk[1],pch=1) 
for (j in 1:length(o1)) points(c(burdens.adj[o1[j],2]/burdens.adj[o1[j],1],burdens.adj[o1[j],3]/burdens.adj[o1[j],1]),c(j,j)+chk[1], type="l",lty=1)

## heat max
points(c(burdens.adj[o1,5]/burdens.adj[o1,1]),1:length(o1)+chk[2],pch=19, col=cols[2]) 
points(c(burdens.adj[o1,6]/burdens.adj[o1,1]),1:length(o1)+chk[2],pch=1, col=cols[2]) 
for (j in 1:length(o1)) points(c(burdens.adj[o1[j],5]/burdens.adj[o1[j],1],burdens.adj[o1[j],6]/burdens.adj[o1[j],1]),c(j,j)+chk[2], type="l",lty=1, col=cols[2])

## heat min
points(c(burdens.adj[o1,8]/burdens.adj[o1,1]),1:length(o1)+chk[3],pch=19, col=cols[3]) 
points(c(burdens.adj[o1,9]/burdens.adj[o1,1]),1:length(o1)+chk[3],pch=1, col=cols[3]) 
for (j in 1:length(o1)) points(c(burdens.adj[o1[j],8]/burdens.adj[o1[j],1],burdens.adj[o1[j],9]/burdens.adj[o1[j],1]),c(j,j)+chk[3], type="l",lty=1, col=cols[3])


## Depict the effect of changinge age of fertility on burden in 2100
par(mfrow=c(1,1), mar=c(6,6,1,1))
plot(calc.change.mean.age.birth,burdens.adj[,3]/burdens.adj[,1], 
	xlab="2023-2100 change mean mother's age for children born", ylab="Rel. future burden in prop. CT births", pch=15, cex=1.5, cex.lab=1.5)
abline(h=1,col="grey")
text(calc.change.mean.age.birth,burdens.adj[,3]/burdens.adj[,1],u.ctry)




#### 5. GIGANTIC SENSITIVITY ANALYSIS ###########################################################################################################################################################################
## Note that Sao Tome and Principe contains no data on Babies

## the first value is for max temp, and the second for min 
testmatrixtemps <- cbind(rep(0:3,each=4),rep(0:3,4))

burdens2023 <- burdens2050 <- burdens2100 <- rep(NA, length(u.ctry)*nrow(testmatrixtemps))

count <- 0

for (j in 1:length(u.ctry)) { 


	## relies on order u.ctry being same as in model.now and in recent.data; and there obly being 4 fixed effects (intercept, slope1,2,3); 
	## can check order of coefficients in model.now by doing levels(new.data$country)	
	country.effect <- coef(model.now)[5:length(coef(model.now))][j]

	## get births in the future
	births.2023 <- as.numeric(births.per.age[index.u.ctry.in.estimate==j & !is.na(index.u.ctry.in.estimate) & year.estimate==2023,])
	births.2050 <- as.numeric(births.per.age.future[index.u.ctry.in.future ==j & !is.na(index.u.ctry.in.future) & year.median ==2050,])
	births.2100 <- as.numeric(births.per.age.future[index.u.ctry.in.future ==j & !is.na(index.u.ctry.in.future) & year.median ==2100,])
	
	for (k in 1:nrow(testmatrixtemps)) { 

		## get same with selected degree increase in maximum and minimum
		risk.now.hot <- exp(coef(model.now)[1]+coef(model.now)[2]*recent.data$recent.hdi[j]+
					coef(model.now)[3]*(recent.data$recent.max.temp[j]+testmatrixtemps[k,1])+
					coef(model.now)[4]*(recent.data$recent.min.temp[j]+testmatrixtemps[k,2])+ country.effect)
		staying.susceptible.hot <- exp(-risk.now.hot*(15:49))
		first.infection.at.age.hot <- risk.now.hot* staying.susceptible.hot
		
		#index and add
		count <- count+1
		burdens2023[count] <- sum(first.infection.at.age.hot*births.2023)
		burdens2050[count] <- sum(first.infection.at.age.hot*births.2050)
		burdens2100[count] <- sum(first.infection.at.age.hot*births.2100)

	}}


#rescale values for each country to be relative to the first value of burdens2023 which corresponds to no change in temperature (do 2023 last, otherwise no effect)
country.index <- rep(u.ctry,each=nrow(testmatrixtemps))

for (j in 1:length(u.ctry)) { 

	burdens2100[country.index==u.ctry[j]] <- burdens2100[country.index==u.ctry[j]]/burdens2023[country.index==u.ctry[j]][1]
	burdens2050[country.index==u.ctry[j]] <- burdens2050[country.index==u.ctry[j]]/burdens2023[country.index==u.ctry[j]][1]
	burdens2023[country.index==u.ctry[j]] <- burdens2023[country.index==u.ctry[j]]/burdens2023[country.index==u.ctry[j]][1]
	
}



## Set up for the regression; results are presented in Table S3
yvalue <- c(burdens2023, burdens2050, burdens2100)
yvalue[!is.finite(log(yvalue))] <- NA
country.index <- as.factor(rep(rep(u.ctry,each=nrow(testmatrixtemps)),3))
country.index[is.na(yvalue)] <- NA
maxtemp.index <- rep(rep(testmatrixtemps[,1],length(u.ctry)),3)
mintemp.index <- rep(rep(testmatrixtemps[,2],length(u.ctry)),3)
yr.index <- c(rep(2023,length(burdens2023)),rep(2050,length(burdens2050)),rep(2100,length(burdens2100)))
hdi.index <- rep(rep(recent.data$recent.hdi,each=nrow(testmatrixtemps)),3)

fitxx <- gam(c(log(yvalue))~ maxtemp.index+ mintemp.index+yr.index+hdi.index+hdi.index:yr.index)
summary(fitxx)

#+s(as.factor(country.index),bs="re"), subset=!is.na(c(log(yvalue)))) ## this is confounded with HDI....

summary(fitxx)


## Currently presented as Figure S5
pdf("figures/Fig_S5.pdf")

## Reveal all countries
zlims <- range(c(burdens2023,burdens2050,burdens2100),na.rm=T)
par(mfrow=c(4,4), mar=c(4,4,4,4),pty="s")
for (j in 1:length(u.ctry)) { 
	if (j==6) next()
	chs.ctry <- rep(u.ctry,each=nrow(testmatrixtemps))==u.ctry[j]

	par(mar=c(0,0,0,0))
	plot(1:10,1:10,type="n",xlab="", ylab="", axes=FALSE)
	text(10,5,u.ctry[j], pos=2, cex=2)

	par(mar=c(4,4,4,4),pty="s")
	image(0:3,0:3, matrix(burdens2023[chs.ctry],4,4,byrow=TRUE), xlab="Max T increase", ylab="Min T increase", zlim=zlims, cex.lab=1.2)
	contour(0:3,0:3, matrix(burdens2023[chs.ctry],4,4,byrow=TRUE),add=TRUE)
	title(2023)

	image(0:3,0:3, matrix(burdens2050[chs.ctry],4,4,byrow=TRUE), xlab="Max T increase", ylab="Min T increase", zlim=zlims, cex.lab=1.2)
	contour(0:3,0:3, matrix(burdens2050[chs.ctry],4,4,byrow=TRUE),add=TRUE)
	title(2050)

	image(0:3,0:3, matrix(burdens2100[chs.ctry],4,4,byrow=TRUE),xlab="Max T increase", ylab="Min T increase", zlim=zlims, cex.lab=1.2)
	contour(0:3,0:3, matrix(burdens2100[chs.ctry],4,4,byrow=TRUE),add=TRUE)
	title(2100)

}

dev.off()




