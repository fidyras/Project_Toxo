

## Bring in the data
df <- read.csv("~/Downloads/toxo reading/toxo-data-analysis-climate/SeroprevToxo.csv")


## sum of squares minimizer to get the right coefficiet for the FOI
logLike <- function(par,xvals,yvals,tot){
	pred <- 1-exp(-par*xvals)
	u <- dbinom(yvals,tot,pmin(pmax(pred,0),0.99), log=TRUE)

	return(-sum(u))
}



## get the pubs for the analysis 
u.pub <- unique(df$Reference..authors..date..journal.)

## storage
foi <- rep(NA,length(u.pub))


pdf("/Users/cmetcalf/Documents/temp/FOI.pdf")
par(mfrow=c(3,3))

## loop over them and get the FOI
for (j in 1:length(u.pub)) { 

	# pick out one of the studies (in order of u.pub)
	chs <- which(df$Reference..authors..date..journal.==u.pub[j],arr.ind=TRUE)
	lower.age <- as.numeric(df$Lower.bound.of.age[chs])
	upper.age <- as.numeric(df$Upper.bound.of.age[chs]) 

	# if they don't provide a lower age, assume 15, since presumably very little pregnancy below that
	lower.age[is.na(lower.age)] <- 15
	# if they don't provide an upper age, assume 50, since presumably very little pregnancy after that
	upper.age[is.na(upper.age)] <- 15


	#get variables for fitting
	mid.ages <- 0.5*(lower.age+upper.age)
	iggpos <- as.numeric(df$Number.of.people.seropositive[chs])
	tot <- as.numeric(df$Number.of.people.tested[chs])

	#if basically no measurements, skip to the next one
	if (sum(!is.na(iggpos))<3) next()
	if (sum(!is.na(tot))<3) next()
			
	# run optim on this 
	tmp <-optimize(f=logLike,lower=1e-9,upper=1,xvals=c(1,mid.ages),yvals=c(0,iggpos),tot=c(100,tot))
	#print(tmp)

	#store the parameter 
	foi[j] <- tmp$minimum

	plot(c(1,mid.ages),c(0,iggpos)/c(100,tot),pch=19,xlab="age", ylab="seropos", ylim=c(0,1))
	points(1:100,1-exp(-foi[j]*c(1:100)), type="l")
	title(df$Location..country.[chs][1])
	legend("topleft",legend=j,bty="n") #just pop the indicator j in top left of plot to track down weird ones

}
	

hist(foi, xlab="Force of infection")
dev.off()

