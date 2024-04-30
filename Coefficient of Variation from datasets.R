CV<-function(x) {return (sd(na.omit(x))/mean(na.omit(x)))} #coefficient of variation

###################
###### DATASET 1 ##
###################
#Coefficient of variation for all species, all sources of variation included, uncertainty regarding the precision of the estimates; comes from the database FoRAGE by Uiterwaal et al. 2022 ;
#From Raw Data
d<-read.csv("Dataset 1/FoRAGE-SB-curated-raw-data.csv", header=T) 
str(d)
df<-subset(d,d$SampleSize !="" & d$SampleSize !="random" & d$Ratio != "")
df$SampleSize<-as.numeric(df$SampleSize)
df$Ratio<-as.numeric(df$Ratio)
d2D<-subset(df, df$SampleSize>=8 & df$Ratio>=0.8 & df$Ratio<=1.0 & df$relativeDensity2D != "NA" & is.na(df$relativeDensity3D))
d3D<-subset(df, df$SampleSize>=8 & df$Ratio>=0.8 & df$Ratio<=1.0 & df$relativeDensity3D != "NA" & is.na(df$relativeDensity2D))
dbothD<-subset(df, df$SampleSize>=8 & df$Ratio>=0.8 & df$Ratio<=1.0 & df$relativeDensity2D != "NA" & df$relativeDensity3D != "NA")
dall3D<-subset(df, df$SampleSize>=8 & df$Ratio>=0.8 & df$Ratio<=1.0 & df$relativeDensity3D != "NA")
dim(d2D)[1]+dim(d3D)[1]+dim(dbothD)[1]
res2D<-tapply(d2D$ForagingRate,list(d2D$Dataset, d2D$density2D.m2.), CV)
res3D<-tapply(d3D$ForagingRate,list(d3D$Dataset, d3D$density3D.m3.), CV)
resbothD<-tapply(dbothD$ForagingRate,list(dbothD$Dataset, dbothD$density2D.m2.), CV)
length(res2D[!is.na(res2D[,])])+length(res3D[!is.na(res3D[,])])+length(resbothD[!is.na(resbothD[,])])#count the number of data
length(unique(d2D$Predator))+length(unique(d3D$Predator))+length(unique(dbothD$Predator))
length(unique(d2D$Prey))+length(unique(d3D$Prey))+length(unique(dbothD$Prey))
#From Summary Statistics
d<-read.csv("Dataset 1/FoRAGE-SB-curated-summary-statistics.csv", header=T) 
str(d)
df<-subset(d,!is.na(d$CV)&d$StandardError!=0)
d2D<-subset(df, df$SampleSize>=8  & df$density2D.m2. != "NA" & is.na(df$density3D.m3.))
d3D<-subset(df, df$SampleSize>=8 & df$density3D.m3. != "NA" & is.na(df$density2D.m2.))
dbothD<-subset(df, df$SampleSize>=8 & df$density2D.m2. != "NA" & df$density3D.m3. != "NA")
dim(d2D)[1]+dim(d3D)[1]+dim(dbothD)[1]
length(unique(d2D$Predator))+length(unique(d3D$Predator))+length(unique(dbothD$Predator))#count the number of data
length(unique(d2D$Prey))+length(unique(d3D$Prey))+length(unique(dbothD$Prey))
#######
pdf("CV1.pdf")
op<-par(mfrow=c(1,1),mgp=c(1.5,0.5,0),mar=c(4,3,2,2)+0.1)
#nf <- layout( matrix(c(1,2), ncol=2) )
#only2D
plot((as.numeric(colnames(res2D))),res2D[1,],xlim=c(0.0001,1e12),ylim=c(0.000001,100.1),pch = 19,col=0, xlab="Initial prey density (log scale)", ylab="Coefficient of variation of the consumption rate",log='xy',yaxt = "n",xaxt = "n")
axis(2,at = c(0.000001, 0.0001,0.01, 0.1, 1,10,100),labels=c(expression("10"^-6),expression("10"^-4),expression("10"^-2),expression("10"^-1),expression("10"^0),expression("10"^1),expression("10"^2)))
axis(1,at = c(1e-3,1,1e3,1e6,1e9),labels=c(expression("10"^-3),expression("10"^0),expression("10"^3),expression("10"^6),expression("10"^9)))
legend("bottomleft", legend = c("2D", "3D", "Mixed"), col = c('red','forestgreen',4), pch = 19, bty = "n")
for(i in c(2:nrow(res2D))) points((as.numeric(colnames(res2D))),res2D[i,],pch = 20,col='red')
#only3D
for(i in c(2:nrow(res3D))) points((as.numeric(colnames(res3D))),res3D[i,],pch = 20,col='forestgreen')
#mixed2D-3D
for(i in c(2:nrow(resbothD))) points((as.numeric(colnames(resbothD))),resbothD[i,],pch = 20,col=4)
points((d2D$density2D.m2.),d2D$CV,pch = 20,col='red')
points((d3D$density3D.m3.),d3D$CV,pch = 20,col='forestgreen')
points((dbothD$density2D.m2.),dbothD$CV,pch = 20,col=4)
abline(h=c(0.298,1.47014,0.0984321,0.467472,0.00222414), lwd=3,lty=c(1,0,1,1,0),col=c('gray50','red','red','forestgreen','forestgreen'))
arrows(2e12,0.0984321, 2e12, 100.1, col = 'red',lwd=3,code=2)
text(x=1e12,y=4.0,substitute(paste(bold('2D (predicted)'))),srt=90, col='red')
arrows(1e12,0.467472, 1e12, 0.00001, col = 'forestgreen',lwd=3,code=2)
text(x=0.6e12,y=0.001,substitute(paste(bold('3D (predicted)'))),srt=90, col='forestgreen')
text(x=0.2e11,y=0.19,substitute(paste(bold('1D (predicted)'))), col='gray50')
text(x=1e-4,y=100,substitute(paste(bold('(a)'))), col='black', cex=1.5)
#boxplot(log10(allCV),axes=F)
dev.off()

#other calculations
allCV1<-  c(res2D[!is.na(res2D[,])],res3D[!is.na(res3D[,])],resbothD[!is.na(resbothD[,])],d2D$CV,d3D$CV,dbothD$CV)
length(allCV1)
summary(allCV1)
plot(density(log10(allCV1)))
median(allCV1)
length(allCV1[allCV1<=1&allCV1>=0.1])/length(allCV1)
length(allCV1[allCV1<=10&allCV1>=0.01])/length(allCV1)
length(allCV1[allCV1>=0.0984321])/length(allCV1)# percent of data that are in the predicted range for Dim 2
length(allCV1[allCV1<=0.467472])/length(allCV1)# percent of data that are in the predicted range for Dim 3

#######################
########## DATASET 2 ##
#######################
 #keywords used in Dryad for datasets collection: "foraging", "intake", "functional response", "visitation rate" + Data from https://github.com/stoufferlab/general-functional-responses (Novak & Stouffer (2021) Systematic bias in studies of consumer functional responses) ; In the latter sources of data, were included only data where the number of predator was 1, where there was several measurements at a given density, a single type of prey ; we exluded any dataset where there were detected inconsistencies or imprecisions ; we included datasets where only the mean, the sampe size and the standard errors were available and we calculated the coefficient of variation from this. In the figure, circles represent CV calculated from raw data, diamonds represent CV calculated from the mean, sample size and standard error. 
###########
pdf("CV2.pdf")
op<-par(mfrow=c(1,1),mgp=c(1.5,0.5,0),mar=c(4,3,2,2)+0.1)
###################1. 2D. Source : from the authors
d<-read.table("Dataset 2/Baker2009.txt", header=T)
na.omit(d[d$SeedDen==5,]$FeedRate)
str(d)
res<-tapply(d$FeedRate,list(d$SeedDen,d$Pen), CV)
#dens<-log(c(5,10,15,25,50,100,200,400))
dens<-c(5,10,15,25,50,100,200,400)
plot(dens,t(res)[1,],xlim=c(0.05,3e3),ylim=c(0.01,2.2),pch=20,col='red', xlab="Initial prey density or prey number (log scale)", ylab="Coefficient of variation of the consumption rate", log='xy', cex=1,xaxt='n',yaxt = "n")
#axis(2,at = c(0.01, 0.1, 1))
axis(2,at = c(0.01, 0.1, 1),labels=c(expression("10"^-2),expression("10"^-1),expression("10"^0)))
axis(1,at = c(1e-1,1e0,1e1,1e2,1e3),labels=c(expression("10"^-1),expression("10"^0),expression("10"^1),expression("10"^2),expression("10"^3)))
legend("bottomleft", legend = c("2D", "3D", "Mixed", "Unknown", "From raw data", "From summary statistics"), col = c('red','forestgreen',4,'black','black','black'), pch = c(15,15,15,15,1,5), bty = "n")
for (i in c(2:4)) points(dens,t(res)[i,],pch=20,col='red')
abline(h=c(0.298,1.47014,0.0984321,0.467472,0.00222414), lwd=3,lty=c(1,0,1,1,0),col=c('gray50','red','red','forestgreen','forestgreen'))
allCV2<-  c(res[!is.na(res[,])])
#########2. 3D Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Thorp2018.csv", header=T)
res<-tapply(d$R,list(d$density,d$Predator.size),CV)
dens<-(c(20,50,100,200,500))
for (i in c(1:3)) points(dens,t(res)[i,],pch=20,col='forestgreen')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########3. 2D  Source : from https://datadryad.org/
d<-read.table("Dataset 2/Duijns2015.txt", header=T)
res<-tapply(d$R,d$Prey_m2, CV)
dens<-(unique(d$Prey_m2))
points(dens,t(res)[1,],ylim=c(-0.01,1),pch=20,col='red')
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########4. 3D   Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Gallagher2016.csv", header=T)
res<-tapply(d$R,list(d$Prey.Density.Start,d$Hawkfish.Size),CV)
dens<-(unique(d$Prey.Density.Start))
points(dens,t(res)[2,],pch=20,col='forestgreen')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########5. 2D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Bressendorff2011.csv", header=T)
res<-tapply(d$R,list(d$Density,d$Test_flies),CV)
dens<-(unique(d$Density))
for (i in c(1:2)) points(dens,t(res)[i,],pch=20,col='red')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########6. 3D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Barrios-Oneill2014.csv", header=T)
#d<-na.omit(d)
#str(d)
d1<-subset(d,predator=="Gdc")
d2<-subset(d,predator=="Gp")
res1<-tapply(d1$R,list(d1$prey.density, d1$refuge),CV)
res2<-tapply(d2$R,list(d2$prey.density, d2$refuge),CV)
dens<-(unique(d$prey.density))
for (i in c(1:4)) points(dens,t(res1)[i,],pch=20,col='forestgreen')
for (i in c(1:4)) points(dens,t(res2)[i,],pch=20,col='forestgreen')
allCV2<-c(allCV2,res1[!is.na(res1[,])],res2[!is.na(res2[,])])
########7. 2D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Uiterwaal2018a.csv", header=T)
d1<-subset(d,Arena_shape=="Circular")
d2<-subset(d,Arena_shape=="Annular")
res1<-tapply(d1$R,list(d1$Density, d1$Arena_size),CV)
res2<-tapply(d2$R,list(d2$Density, d2$Arena_size),CV)
dens1<-(unique(d1$Density))
dens2<-(unique(d2$Density))
for (i in c(1:3)) points(dens1,t(res1)[i,],pch=20,col='red')
for (i in c(1:3)) points(dens2,t(res2)[i,],pch=20,col='red')
allCV2<-c(allCV2,res1[!is.na(res1[,])],res2[!is.na(res2[,])])
#########8. 3D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Wasserman2016.csv", header=T)
res<-tapply(d$R,list(d$Density,d$Predator),CV)
dens<-(unique(d$Density))
for (i in c(1:9)) points(dens,t(res)[i,],pch=20,col='forestgreen')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########9. 3D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Sentis2015.csv", header=T)
res<-tapply(d$R,d$Density,CV)
dens<-(unique(d$Density))
points(dens,t(res)[1,],ylim=c(-0.01,1),pch=20,col='forestgreen')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########10. 2D Source : from https://datadryad.org/
d<-read.csv("Dataset 2/vucic2010.csv", header=T)
res<-tapply(d$R,d$N0,CV)
dens<-(unique(d$N0))
points(dens,t(res)[1,],ylim=c(-0.01,1.2),pch=20,col='red')
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########11. 2D-3D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Toscano2014.csv", header=T)
res<-tapply(d$R,list(d$prey,d$toadfish.cue.treatment),CV)
dens<-(unique(d$prey))
for (i in c(1:2)) points(dens,t(res)[i,],pch=20,col=4)
allCV2<-c(allCV2,res[!is.na(res[,])])
#########12. 2D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Born-Torrijos2020.csv", header=T)
res<-tapply(d$R,list(d$Density,d$Prey.species),CV)
dens<-(unique(d$Density))
for (i in c(1:3)) points(dens,t(res)[i,],pch=20,col='red')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########13. 3D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/McCard2021.csv", header=T)
d$R<-d$eaten
res<-tapply(d$R,list(d$density,d$prey),CV)
dens<-(unique(d$density))
for (i in c(1:3)) points(dens,t(res)[i,],pch=20,col='forestgreen')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########14. NA  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Nan2023.csv", header=T)
d$R<-d$Neaten_s
d$density<-d$N0
res<-tapply(d$R,list(d$density,d$Prey),CV)
allCV2<-c(allCV2,res[!is.na(res[,])])
dens<-(unique(d$density))
for (i in c(1:3)) points(dens,t(res)[i,],pch=20,col='black')
d$R<-d$Neaten_c
d$density<-d$N0
res<-tapply(d$R,list(d$density,d$Prey),CV)
allCV2<-c(allCV2,res[!is.na(res[,])])
dens<-(unique(d$density))
for (i in c(1:3)) points(dens,t(res)[i,],pch=20,col='black')
#########15. NA  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/cuthbert2022.csv", header=T)
d$R<-d$eaten
d$density<-d$prey
res<-tapply(d$R,list(d$density,d$pred),CV)
dens<-(unique(d$density))
for (i in c(1:3)) points(dens,t(res)[i,],pch=20,col='black')
allCV2<-c(allCV2,res[!is.na(res[,])])
#########16. 3D  Source : from https://datadryad.org/
d<-read.csv("Dataset 2/Mocq2021.csv", header=T)
d$R<-d$N_Prey_Eaten
d$density<-d$N_Prey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='forestgreen')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########1. 2D-3D  Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Chong_2006.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col=4)
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########2. 3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Hossie_2016_cl.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='forestgreen')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########3. 3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Hossie_2016_ev.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='forestgreen')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########4. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Jones_1988.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='red')
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########5. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Kratina_2009.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='red')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########6. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Lang_2012_Po_10.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col=4)
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########7. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Lang_2012_Po_20.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col=4)
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########8. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Lang_2012_Pt_10.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col=4)
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########9. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Lang_2012_Pt_10.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col=4)
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########10. 3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Long_2012.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='forestgreen')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########11. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Medoc_2013.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='red')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########12. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Medoc_2015_be.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='red')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########13. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Medoc_2015_dv.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='red')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########14. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Medoc_2015_pu.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='red')
res<-data.frame(res)
allCV2<-c(allCV2,res[,])
#########15. 3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Prokopenko_2017.csv", header=T)
d<-d0[d0$Npredator==1,]
#d$R<-d$Nconsumed
d$density<-d$Nprey 
res<-tapply(d$R,list(d$density),CV)
dens<-(unique(d$density))
points(dens,t(res),pch=20,col='forestgreen')
res<-na.omit(data.frame(res))
allCV2<-c(allCV2,res[,])
#########16. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Eveleigh_1982_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch = 18,col=4)
allCV2<-c(allCV2,d$R)
#########17. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Griffen_2007_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col=4)
allCV2<-c(allCV2,d$R)
#########18. NA Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Hassan_1976_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='black')
allCV2<-c(allCV2,d$R)
#########19. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Huffaker_1982_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='red')
allCV2<-c(allCV2,d$R)
#########20. 3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Krylov_1992_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='forestgreen')
allCV2<-c(allCV2,d$R)
#########21. NA Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Kumar_1985_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='black')
allCV2<-c(allCV2,d$R)
#########22. 3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Mansour_1991_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='forestgreen')
allCV2<-c(allCV2,d$R)
#########23. 2D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Montoya_2000_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='red')
allCV2<-c(allCV2,d$R)
#########24. NA Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Uttley_1980_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col='black')
allCV2<-c(allCV2,d$R)
#########25. 2D-3D Source : https://github.com/stoufferlab/general-functional-responses (Stouffer & Novak 2021)
d0<-read.csv("Dataset 2/Walde_1984_se.csv", header=T)
d<-d0[d0$Npredator==1,]
d$R<-d$Nconsumed.se*sqrt(d$n)/d$Nconsumed.mean
d$density<-d$Nprey 
d$dens<-(d$density)
points(d$dens,d$R,pch=18,col=4)
allCV2<-c(allCV2,d$R)
########
arrows(3e3,0.0984321, 3e3, 2.5, col = 'red',lwd=3,code=2)
text(x=2.5e3,y=0.8,substitute(paste(bold('  2D (predicted)'))),srt=90, col='red')
arrows(2e3,0.467472, 2e3, 0.01, col = 'forestgreen',lwd=3,code=2)
text(x=1.6e3,y=0.04,substitute(paste(bold('  3D (predicted)'))),srt=90, col='forestgreen')
text(x=1e3,y=0.265,substitute(paste(bold('        1D \n (predicted)'))), col='gray50')
text(x=5e-2,y=2.0,substitute(paste(bold('(b)'))), col='black', cex=1.5)
dev.off()

#other calculations
length(allCV2)
allCV2<-allCV2[allCV2!=0]
plot(density(log10(allCV2)))
median(allCV2)
length(allCV2[allCV2<=1&allCV2>=0.1])/length(allCV2)
length(allCV2[allCV2<=10&allCV2>=0.01])/length(allCV2)
length(allCV2[allCV2>=0.0984321])/length(allCV2)# percent of data that are in the predicted range for Dim 2
length(allCV2[allCV2<=0.467472])/length(allCV2)# percent of data that are in the predicted range for Dim 3


##DATASET 3
### Within individual variation
#########
pdf("CV3.pdf")
op<-par(mfrow=c(1,1),mgp=c(1.5,0.5,0),mar=c(4,3,2,2)+0.1)
#### Adélie Penguin
d<-read.csv("Dataset 3/KrillCaptureRate.csv", header=T)
dCV<-tapply(d$R,d$PenguinID,CV)
res1<-data.frame(dCV)$dCV
######## Cormoran
d<-read.csv("Dataset 3/ImperialCormorant_Dive_Bout.csv",header=T)
res2<-d[d$Scale=="DIVE",]$FR_CV
######### Crayfish within individuals within density
d<-read.csv("Dataset 3/Linzmaier2019.csv", header=T)
d$R<-d$Mussels_Consumed
d$density<-d$Mussels_IN
res3a<-tapply(d$R,list(d$density,d$ID),CV)
m3a<-res3a[1,1]
for(i in 1:7) for (j in 1:41) if(i!=1 & j!= 1) if(!is.na(res3a[i,j])) m3a<-cbind(m3a,res3a[i,j])
###### crayfish within individuals across densities
res3b<-tapply(d$R,list(d$ID),CV)
m3b<-res3b[1]
for(i in 1:41) if(i!=1) if(!is.na(res3b[i])) m3b<-cbind(m3b,res3b[i])
#########
plot(density(res1),xlim=c(0,1.5), ylim=c(0,5.5), col='steelblue3', xlab="Within-individuals coefficient of variation", main="",lwd=4.0)
lines(density(res2),xlim=c(0,1.5), ylim=c(0,14), col='indianred3',lwd=4.0);
lines(density(m3a),xlim=c(0,1.5), ylim=c(0,14), col='goldenrod2',lwd=4.0,lty=1);
lines(density(m3b),xlim=c(0,1.5), ylim=c(0,14), col='goldenrod4',lwd=4.0);
#abline(v=c(0.298,1.47014,0.0984321,0.467472,0.00222414), lwd=3,lty=c(1,0,1,1,0),col=c('gray50','red','red','forestgreen','forestgreen'))
segments(0.298,0.0,0.298,5.0,col='gray50',lwd=2)
segments(0.0984321,5.45,0.0984321,5.8,col='red',lwd=3)
arrows(0.0984321,5.6, 1.5, 5.6, col = 'red',lwd=3,code=2)
text(x=0.7,y=5.45,substitute(paste(bold('2D (predicted)'))),srt=0, col='red')
arrows(0.467472,5.4, -0.05,5.4, col = 'forestgreen',lwd=3,code=2)
segments(0.467472,5.25,0.467472,5.55,col='forestgreen',lwd=3)
text(x=0.22,y=5.25,substitute(paste(bold('3D (predicted)'))),srt=0, col='forestgreen')
text(x=0.265,y=4.4,substitute(paste(bold('1D (predicted)'))),srt=90, col='gray50')
text(x=-0,y=5.1,substitute(paste(bold('(c)'))), col='black', cex=1.5)
legend(x=0.5,y=5, c('Adélie penguin', 'Imperial shag', 'Crayfish (within individuals and densities)','Crayfish (within individuals across densities)'), fill=c('steelblue3', 'indianred3', 'goldenrod2','goldenrod4'))
dev.off()

#Other calculations
allCV3<-c(res1,res2,res3a[!is.na(res3a[,])])
length(allCV3)
plot(density(log10(allCV3)))
median(allCV3)
length(allCV3[allCV3<=1&allCV3>=0.1])/length(allCV3)
length(allCV3[allCV3<=10&allCV3>=0.01])/length(allCV3)
length(allCV3[allCV3>=0.0984321])/length(allCV3)# percent of data that are in the predicted range for Dim 2
length(allCV3[allCV3<=0.467472])/length(allCV3)# percent of data that are in the predicted range for Dim 3

#Boxplot compiling all datasets
pdf("boxplot-CV.pdf")
boxplot(log10(allCV1),log10(allCV2),log10(allCV3),ylim=c(-6,3),yaxt='n',xaxt='n', ylab="Estimated coefficient of variation of the consumption rate")
axis(2,at = c(-6,-4, -2,-1,0, 1, 2),labels=c(expression("10"^-6),expression("10"^-4),expression("10"^-2),expression("10"^-1),expression("10"^0),expression("10"^1),expression("10"^2)))
text(c(2.7,2.7,2.7),c("Dataset 1 \n (FoRAGE database) \n n=3039","Dataset 2 \n \n n=602", "Dataset 3 \n \n n=229"))
dev.off()

pdf("boxplot-CV-with-no-data.pdf")
boxplot(log10(0.298),log10(0.298),log10(0.298),ylim=c(-6,3),yaxt='n',xaxt='n', ylab="Estimated coefficient of variation of the consumption rate")
axis(2,at = c(-6,-4, -2,-1,0, 1, 2),labels=c(expression("10"^-6),expression("10"^-4),expression("10"^-2),expression("10"^-1),expression("10"^0),expression("10"^1),expression("10"^2)))
text(c(2.7,2.7,2.7),c("Dataset 1 \n (FoRAGE database) \n n=3039","Dataset 2 \n \n n=602", "Dataset 3 \n \n n=229"))
abline(h=log(0.298,10),col='gray50', lwd=3)
abline(h=log(0.1,10),col='red', lwd=3)
abline(h=log(0.01,10),col='forestgreen', lwd=3)
dev.off()

pdf("boxplot-CV-with-data.pdf")
boxplot(log10(allCV1),log10(allCV2),log10(allCV3),ylim=c(-6,3),yaxt='n',xaxt='n', ylab="Estimated coefficient of variation of the consumption rate")
axis(2,at = c(-6,-4, -2,-1,0, 1, 2),labels=c(expression("10"^-6),expression("10"^-4),expression("10"^-2),expression("10"^-1),expression("10"^0),expression("10"^1),expression("10"^2)))
text(c(2.7,2.7,2.7),c("Dataset 1 \n (FoRAGE database) \n n=3039","Dataset 2 \n \n n=602", "Dataset 3 \n \n n=229"))
abline(h=log(0.298,10),col='gray50', lwd=3)
abline(h=log(0.1,10),col='red', lwd=3)
abline(h=log(0.01,10),col='forestgreen', lwd=3)
dev.off()

res3a<-tapply(d$R,list(d$density,d$ID),CV)
m<-res3a[1,1]
for(i in 1:7) for (j in 1:41) if(i!=1 & j!= 1) if(!is.na(res3a[i,j])) m<-cbind(m,res3a[i,j])


####
pdf("CV-with-no-data.pdf")
op<-par(mfrow=c(1,1),mgp=c(1.5,0.5,0),mar=c(4,3,2,2)+0.1)
plot(0,0,,xlim=c(0.0001,1e12),ylim=c(0.000001,100.1),pch = 19,col=0, xlab="Initial prey density (log scale)", ylab="Coefficient of variation of the consumption rate",log='xy',yaxt = "n",xaxt = "n")
axis(2,at = c(0.000001, 0.0001,0.01, 0.1, 1,10,100),labels=c(expression("10"^-6),expression("10"^-4),expression("10"^-2),expression("10"^-1),expression("10"^0),expression("10"^1),expression("10"^2)))
axis(1,at = c(1e-3,1,1e3,1e6,1e9),labels=c(expression("10"^-3),expression("10"^0),expression("10"^3),expression("10"^6),expression("10"^9)))
legend("bottomleft", legend = c("2D", "3D", "Mixed"), col = c('red','forestgreen',4), pch = 19, bty = "n")
abline(h=c(0.298,1.47014,0.0984321,0.467472,0.00222414), lwd=3,lty=c(1,0,1,1,0),col=c('gray50','red','red','forestgreen','forestgreen'))
arrows(2e12,0.0984321, 2e12, 100.1, col = 'red',lwd=3,code=2)
text(x=1e12,y=4.0,substitute(paste(bold('2D (predicted)'))),srt=90, col='red')
arrows(1e12,0.467472, 1e12, 0.00001, col = 'forestgreen',lwd=3,code=2)
text(x=0.6e12,y=0.001,substitute(paste(bold('3D (predicted)'))),srt=90, col='forestgreen')
text(x=0.2e11,y=0.19,substitute(paste(bold('1D (predicted)'))), col='gray50')
text(x=1e-4,y=100,substitute(paste(bold('(a)'))), col='black', cex=1.5)
dev.off()
