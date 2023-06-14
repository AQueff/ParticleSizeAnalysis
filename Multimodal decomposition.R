setwd("C:/Users/XXX/XXX/XXX") #place where the Distribution_for_decomposition_script.csv is located, which is a transposed
library(mixdist)

# With gaussian distributions for cave deposits or fluviatil sediment ----------------------

# Data preparation
parametres=cbind("Echantillon","Goodness of fit","Proportion","Mode_microns", "Mode_phi", "Sigma");colnames(parametres)=parametres;parametres=as.data.frame(parametres,stringsAsFactors=F);parametres<-parametres[-1,]


dat <- read.csv("Distribution_for_decomposition_script.csv", sep=",", dec=".", header=TRUE)
dat <- dat[order(dat[,1]), ]
dat[93,1] <- Inf
x.column.list = colnames(dat)[2:ncol(dat)]
data=dat[,1]
liste <- list()
for(x in x.column.list)
{data = dat[,x]
data = cbind(dat[,1],data)
colnames(data)=c("Grain diameter (phi)",x)
data=as.data.frame(data)
liste[[x]]<-data
}
str(liste)
attach(liste)

# Estimation par un m?lange de lois gaussiennes

#Variables ? modifier :
ech=PYS.2017.200127 ; plot(ech, type="l") #?chantillon ? traiter et trace la distribution pour trouver les valeurs de d?part des centres et le nb de populations
iterations=50 #nombre d'it?rations, 50, voire m?me 20, semble bien suffisant
centres=c(2.3,3,8.5,11.5) #valeur des centres des populations, en phi, dans l'ordre croissant
formes=c(1,1.3,1,0.6) #valeur des largeurs des populations, varie habituellement entre 0.5 et 2, le plus souvent 1 fonctionne

#Ligne ? lancer pour calculer le fit selon les param?tres pr?c?demment s?lectionn?s
params <- mixparam(mu=centres, sigma=formes) ; estim <- mix(ech, params, dist="norm", emsteps=iterations) ; plot(estim,clwd=2)


############ grahique personnalis? ? lancer quand le fit est bon ###########

namepdf=paste0(colnames(ech)[2],".pdf")
pdf(namepdf, height=10,width=15)

# 1. Distrib empirique :
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

title(main=paste0("Sample ",colnames(ech)[2]),line = -3)
lines(-hx,hy,col="blue")


# 2. Composantes du m?lange : 
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

# 3. Loi m?lange :
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


######Lignes ? lancer quand le fit de l'?chantillon est bon pour faire le pdf et ajouter les param?tres au fichier #######
nom_ech=matrix(data=NA,nrow = length(centres),1); colnames(nom_ech)="Echantillon";nom_ech[1,1]=colnames(ech)[2];nom_ech=as.data.frame(nom_ech) ; GoF = matrix(data=NA,nrow = length(centres),1); colnames(GoF)="Goodness of fit";GoF[1,1]=estim$chisq; GoF=as.data.frame(GoF);matrixmode=as.data.frame(listemode);matrixmode=as.matrix(matrixmode);matrixmode=t(matrixmode);new_parameters = as.data.frame(stringsAsFactors=F,cbind(nom_ech,GoF=round(GoF,2),Proportion = round((estim$parameters$pi)*100,1), Mode_microns=round(1000*2^-(matrixmode),2), Mode_phi=round(matrixmode,2), Sigma = round(estim$parameters$sigma,2)))
parametres=rbind(parametres,new_parameters)


##########  Ligne ? lancer quand la s?rie est termin?e pour cr?er le csv avec les param?tres ########
write.csv(parametres, "All decomposition parameters.csv")




############################################
## FOR LOESS WITH LOGNORMAL DISTRIBUTION ###
############################################

# Data preparation ####
parametres=cbind("Echantillon","Goodness of fit","Proportion","Mode_microns", "Mode_altitude", "Sigma");colnames(parametres)=parametres;parametres=as.data.frame(parametres,stringsAsFactors=F);parametres<-parametres[-1,]

dat1 <- read.csv("Distribution_for_decomposition_script.csv", sep=",", dec=".", header=TRUE)
dat1=dat1[1:93,]
dat1[93,1] <- Inf
x.column.list1 = colnames(dat1)[2:ncol(dat1)]
data1=dat1[,1]
liste1 <- list()
for(x in x.column.list1)
{data1 = dat1[,x]
data1 = cbind(dat1[,1],data1)
colnames(data1)=c("Grain size (phi)",x)
data1=as.data.frame(data1)
liste1[[x]]<-data1
}
str(liste1)
attach(liste1)

#### Variables to be modified for each sample : ####
ech=PYS.2017.200127 #samples in liste1
plot(ech, type="l",lab = c(20, 5, 0)) #tracer la distribution pour voir les modes

iterations=50 #nombre d'itérations, 50, voire même 20, semble bien suffisant
centres=c(2.3,3,7,8.5,11.5) #valeur des centres des populations, en phi, dans l'ordre croissant
formes=c(1,1,1.3,1,0.6) #valeur des largeurs des populations, varie habituellement entre 0.5 et 2, le plus souvent 1 fonctionne

# Lignes ? lancer pour calculer et afficher le fit selon les param?tres pr?c?demment s?lectionn?s
params <- mixparam(mu=centres, sigma=formes) ; 
estim <- mix(ech, params, dist="lnorm", emsteps=iterations) ; 
plot(estim,clwd=2)

# grahique personnalis? pour loess, ? lancer quand le fit est bon---------------
namepdf=paste0(colnames(ech)[2],".pdf")
pdf(namepdf, height=10,width=15)

# 1. Distrib empirique :
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
plot(x=-mixdat[,1], y=ifreq, ylim=c(0,max(dat1[,-1])),pch="", cex=0.5, xlab="", ylab="Frequency (%)", axes=FALSE)

title(main=paste0("Sample ",colnames(ech)[2]),line = -3)
lines(-hx,hy,col="blue")


# 2. Composantes du m?lange :
ytot <- rep(0,length(ifreq))
listemode <- list()
for (k in 1:nrow(mixpar)) {
  x <- seq(from=-40, to=70, length=length(ifreq))
  par2 <- sqrt(log((mixpar[k, 3]/(mixpar[k, 2]))^2 + 1))
  par1 <- log(mixpar[k, 2]) -(par2^2)/2
  y <- dlnorm(x, par1, par2) * mixpar[k,"pi"] * iwid * 100
  ytot <- ytot + y
  smooth_y=smooth.spline(x,y)
  mode=exp(par1-par2^2)
  listemode[[k]]=mode
  lines(smooth_y, col="red")
}

# 3. Loi m?lange :
smooth_ytot=smooth.spline(-x,ytot)
lines(smooth_ytot,col="darkgreen",lwd="3")

# 4. Axes :
mtext("Particle size",1,line = -0.5)
axis(side=1, line=1, labels=c("","",""," "," ",""," "," ","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""), at=-c(16.6096404744368,	15.6096404744368,	15.0246779737157,	14.6096404744368,	14.2877123795495,	14.0246779737157,	13.8022855523792,	13.6096404744368,	13.4397154729945,	13.28,	12.2877123795495,	11.7027498788283,	11.2877123795494,	10.9657842846621,	10.7027498788283,	10.4803574574918,	10.2877123795495,	10.1177873781071,	9.96578428466209,	8.96578428466209,	8.38082178394093,	7.96578428466209,	7.64385618977473,	7.38082178394093,	7.15842936260448,	6.96578428466209,	6.79585928321978,	6.64385618977472,	5.64385618977472,	5.05889368905357,	4.64385618977472,	4.32192809488736,	4.05889368905357,	3.83650126771712,	3.64385618977472,	3.47393118833241,	3.32192809488736,	2.32192809488736,	1.73696559416621,	1.32192809488736,	1,	0.736965594166206,	0.514573172829759,	0.321928094887362,	0.15200309344505,	0.000000001), padj=0.5)
axis(side=1, line=1, labels=c(0.01,0.1,1,10,100,1000), at=-c(16.6096404744368,	13.28,	9.9657,	6.64385,	3.3219280,		0.000000001), padj=0.5)
mtext("Microns", 1, line=1, at=-16.1,padj=-2)
axis(side=2) # axe vertical
axis(side=1, line=4, labels=c(0,"","","","",5,"","","","",10,"","","","",15), at=-seq(0,15,by=1), padj=0.5)
mtext("Phi", 1, line=4, at=-14.5, padj=-2)

dev.off()

#axis(side=1, line=1, labels=c(0.01,"","","","","","","","",0.1,"","","","","","","","",1,"","","","","","","","",10,"","","","","","","","",100,"","","","","","","","",1000), at=-c(16.6096404744368,	15.6096404744368,	15.0246779737157,	14.6096404744368,	14.2877123795495,	14.0246779737157,	13.8022855523792,	13.6096404744368,	13.4397154729945,	13.28,	12.2877123795495,	11.7027498788283,	11.2877123795494,	10.9657842846621,	10.7027498788283,	10.4803574574918,	10.2877123795495,	10.1177873781071,	9.96578428466209,	8.96578428466209,	8.38082178394093,	7.96578428466209,	7.64385618977473,	7.38082178394093,	7.15842936260448,	6.96578428466209,	6.79585928321978,	6.64385618977472,	5.64385618977472,	5.05889368905357,	4.64385618977472,	4.32192809488736,	4.05889368905357,	3.83650126771712,	3.64385618977472,	3.47393118833241,	3.32192809488736,	2.32192809488736,	1.73696559416621,	1.32192809488736,	1,	0.736965594166206,	0.514573172829759,	0.321928094887362,	0.15200309344505,	0.000000001), padj=0.5)

#### ? faire quand estim OK pour ajouter les param?tres au fichier et faire le graphique en pdf #######
nom_ech=matrix(data=NA,nrow = length(centres),1); colnames(nom_ech)="Echantillon";nom_ech[1,1]=colnames(ech)[2];nom_ech=as.data.frame(nom_ech) ; GoF = matrix(data=NA,nrow = length(centres),1); colnames(GoF)="Goodness of fit";GoF[1,1]=estim$chisq; GoF=as.data.frame(GoF);matrixmode=as.data.frame(listemode);matrixmode=as.matrix(matrixmode);matrixmode=t(matrixmode);new_parameters = as.data.frame(stringsAsFactors=F,cbind(nom_ech,GoF=round(GoF,2),Proportion = round((estim$parameters$pi)*100,1), Mode_microns=round(1000*2^-(matrixmode),2), Mode_phi=round(matrixmode,2), Sigma = round(estim$parameters$sigma,2)))
parametres=rbind(parametres,new_parameters)

# cr?er le csv contenant les param?tres########
write.csv(parametres, "parametres.csv", na="")

