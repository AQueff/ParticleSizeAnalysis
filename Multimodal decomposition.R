setwd("C:/Users/a.queffelec/Documents/GitHub/ParticleSizeAnalysis") #place where the Distribution_for_decomposition_script.csv is located, which is a transposed
library(mixdist)

# With gaussian distributions for cave deposits or fluviatil sediment ----------------------

# Data preparation

## creation of an empty dataframe 
parameters <- data.frame(Sample = character(),Goodness_of_fit = double(),Proportion = double(),Mode_microns = double(),Mode_phi = double(),Sigma = double())

## Preparation of the list of samples from the Distribution_for_decomposition_script.csv file
## which is copy/pasted from the granulometric_results created by the other script
dat <- read.csv("Distribution_for_decomposition_script.csv", sep=",", dec=".", header=TRUE)
dat <- dat[order(dat[,1]), ]
dat[93,1] <- Inf
x.column.list = colnames(dat)[2:ncol(dat)]
data=dat[,1]
list_samples <- list()
for(x in x.column.list)
{data = dat[,x]
data = cbind(dat[,1],data)
colnames(data)=c("Grain diameter (phi)",x)
data=as.data.frame(data)
list_samples[[x]]<-data
}
str(list_samples)
attach(list_samples)

# Decomposition by a mix of gaussian distributions 
# (to use other distribution change the dist="norm" in line 38, for example put dist="lnorm" for lognormal distributions for loess)

## Create the function that will use the following parameters to fit the distribution

Fit <- function(sample,iterations,centers,shapes){
  params <- mixparam(mu=centers, sigma=shapes);
  estim <<- mix(sample, params, dist="norm", emsteps=iterations);
  plot(estim,clwd=2)}

# number of iteration of the fitting algorithm. 20 seems to be enough, I let 50 but I don't know how to set this, so I put more rather than less...
iterations=50 

# Values to be modified for each sample
sample=PYS.2017.200127 # Sample name
plot(sample, type="l") # line plot of the distrbution to estimate by eye the number of grain populations and the center of each gaussian curve.

# Estimating the initial value for the fitting algorithm to begin with
centers=c(2.3,3,8.5,11.5) # Estimated centers for the populations, to be put in increasing order of phi values
shapes=c(1,1.3,1,0.6) # Estimated values for the width of the distribution, vary between 0.5 and 2, most of the time I begin with 1 for each population and adjust if it doesn't work.

# Launch the fitting by the mmixdist package
Fit(sample,iterations,centers,shapes)

############ Drawing and saving as pdf a nice plot when the fit is OK ###########

namepdf=paste0(colnames(sample)[2],".pdf")
pdf(namepdf, height=10,width=15)

# 1. Measured distribution :
mixdat <- estim$mixdata
mixpar <- estim$parameters

ntot <- sum(mixdat[, 2]) # effectif total
m <- nrow(mixdat) # nb de lignes
iwid <- mixdat[2:(m - 1), 1] - mixdat[1:(m - 2), 1] # les "plateaux" de la densit\'e empirique
iwid <- c(2 * iwid[1], iwid, 2 * iwid[m - 2])
if (mixdat[1, 1] > 0) {
  iwid[1] <- min(iwid[1], mixdat[1, 1])}
idens <- (mixdat[, 2]/iwid)/ntot
ifreq <- (mixdat[, 2])
hx <- rep(c(mixdat[1, 1] - iwid[1], mixdat[-m, 1], mixdat[m - 1, 1] + iwid[m]), rep(2, m + 1))
hy <- c(0, rep(ifreq, rep(2, m)), 0)

par(mar=c(7,4,0,1))
plot(x=-mixdat[,1], y=ifreq, ylim=c(0,max(dat[,-1])),pch="", cex=0.5, xlab="", ylab="Frequency (%)", axes=FALSE)

title(main=paste0("Sample ",colnames(sample)[2]),line = -3)
lines(-hx,hy,col="blue",lty = 2)

# 2. Each grain population 
ytot <- rep(0,length(ifreq))
listemode <- list()
for (k in 1:nrow(mixpar)) {
  x <- seq(from=-2, to=17, length=length(ifreq))
  par2 <- mixpar[k, 3]
  par1 <- mixpar[k, 2]
  y <- dnorm(x, par1, par2) * mixpar[k,"pi"] * iwid * 100
  ytot <- ytot + y
  smooth_y=smooth.spline(-x,y)
  mode=exp(par1-par2^2)
  listemode[[k]]=mode
  lines(smooth_y, col="red")
}

# 3. Sum of estimated populations
smooth_ytot=smooth.spline(-x,ytot)
lines(smooth_ytot,col="darkgreen",lwd="3")

# 4. Axes :
mtext("Particle size",1,line = -0.5)
axis(side=1, line=1, labels=c("","",""," "," ",""," "," ","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""), at=-c(16.6096404744368,	15.6096404744368,	15.0246779737157,	14.6096404744368,	14.2877123795495,	14.0246779737157,	13.8022855523792,	13.6096404744368,	13.4397154729945,	13.28,	12.2877123795495,	11.7027498788283,	11.2877123795494,	10.9657842846621,	10.7027498788283,	10.4803574574918,	10.2877123795495,	10.1177873781071,	9.96578428466209,	8.96578428466209,	8.38082178394093,	7.96578428466209,	7.64385618977473,	7.38082178394093,	7.15842936260448,	6.96578428466209,	6.79585928321978,	6.64385618977472,	5.64385618977472,	5.05889368905357,	4.64385618977472,	4.32192809488736,	4.05889368905357,	3.83650126771712,	3.64385618977472,	3.47393118833241,	3.32192809488736,	2.32192809488736,	1.73696559416621,	1.32192809488736,	1,	0.736965594166206,	0.514573172829759,	0.321928094887362,	0.15200309344505,	0.000000001), padj=0.5)
axis(side=1, line=1, labels=c(0.01,0.1,1,10,100,1000,3000), at=-c(16.6096404744368,	13.28,	9.9657,	6.64385,	3.3219280,0.000001,		-2), padj=0.5)
mtext("Microns", 1, line=1, at=-16.1,padj=-2)
axis(side=2) # axe vertical
axis(side=1, line=4, labels=c(-2,"",0,"","","","",5,"","","","",10,"","","","",15), at=-seq(-2,15,by=1), padj=0.5)
mtext("Phi", 1, line=4, at=-14.5, padj=-2)

dev.off()


###### Lines to run when the fit is good to add the parameters of this sample to the other samples' parameters #######
sample_name=as.data.frame(matrix(data=NA,nrow = length(centers),ncol =1,dimnames = list(seq(1,length(centers),by=1),"Sample"))); sample_name[1,1]=colnames(sample)[2];
GoF = as.data.frame(matrix(data=NA,nrow = length(centers),1)); colnames(GoF)="Goodness of fit";GoF[1,1]=estim$chisq ;
matrixmode=t(as.matrix(as.data.frame(listemode)));
new_parameters = as.data.frame(stringsAsFactors=F,cbind(sample_name,GoF=round(GoF,2),Proportion = round((estim$parameters$pi)*100,1), Mode_microns=round(1000*2^-(matrixmode),2), Mode_phi=round(matrixmode,2), Sigma = round(estim$parameters$sigma,2)))
parameters=rbind(parameters,new_parameters)



##########  Line to run after all samples are done, to write the csv with parameters for all samples ########
write.csv(parameters, "All decomposition parameters.csv")