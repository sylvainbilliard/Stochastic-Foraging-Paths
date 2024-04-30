library(ggplot2)
theme_set(theme_classic() + theme(legend.position = "top")  )

d<-read.csv("Compiled_Results.csv", header=T)
########
pdf("CVsimulation.pdf")
op<-par(mfrow=c(3,2),mgp=c(1.5,0.5,0),mar=c(3,3,2,2)+0.1)
###### Default model with t=10e5
#
df<-subset(d,d$model=="default")
#######
plot(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV,xlim=c(1000,11000),ylim=c(0.001,1.0),pch = 20,col=0, xlab="Sites density", ylab="Coefficient of variation",log='y', yaxt="n")
lines(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV, col='gray50',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==2,]$initial_density,df[df$method=="prediction" & df$dimensionality==2,]$CV, col='pink2',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==3,]$initial_density,df[df$method=="prediction" & df$dimensionality==3,]$CV, col='seagreen3',lwd=2)
points(df[df$method=="simulation" & df$dimensionality==1,]$initial_density,df[df$method=="simulation" & df$dimensionality==1,]$CV, col='gray50',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==2,]$initial_density,df[df$method=="simulation" & df$dimensionality==2,]$CV, col='pink2',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==3,]$initial_density,df[df$method=="simulation" & df$dimensionality==3,]$CV, col='seagreen3',lwd=2,pch = 20)
axis(2,at = c(0.001, 0.01, 0.1, 0.5))
arrows(10300,0.15, 10300, 0.09, col = 'black',lwd=1.2,code=2,length=0.05)
text(x=10360,y=0.13,substitute(paste(italic('t x 100'))), col='black', cex=0.7, adj=0)
arrows(10000,0.05, 10000, 0.005, col = 'black',lwd=1.2,code=2,length=0.05)
text(x=10060,y=0.015,substitute(paste(italic('t x 100'))), col='black', cex=0.7, adj=0)
#text(x=9900,y=0.4,substitute(paste(bold('1D'))), col='gray50')
#text(x=9900,y=0.125,substitute(paste(bold('2D'))), col='red')
#text(x=9900,y=0.008,substitute(paste(bold('3D'))), col='seagreen4')
text(x=1000,y=0.85,substitute(paste(bold('(a): Simple random walk'))), col='black', cex=1, adj=0)
####  default model with t=10e5
#
df<-subset(d,d$model=="default10e5") 
lines(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV, col='gray50',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==2,]$initial_density,df[df$method=="prediction" & df$dimensionality==2,]$CV, col='red',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==3,]$initial_density,df[df$method=="prediction" & df$dimensionality==3,]$CV, col='seagreen4',lwd=2)
points(df[df$method=="simulation" & df$dimensionality==1,]$initial_density,df[df$method=="simulation" & df$dimensionality==1,]$CV, col='gray50',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==2,]$initial_density,df[df$method=="simulation" & df$dimensionality==2,]$CV, col='red',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==3,]$initial_density,df[df$method=="simulation" & df$dimensionality==3,]$CV, col='seagreen4',lwd=2,pch = 20)
#### empty plot
plot.new()
legend("bottomleft", legend = c("Prediction for the simple random walk", "Stochastic simulation"), col = c('black'), pch = c(NA,19), lty=c(1,NA),bty = "n",lwd=2)
legend("left", legend = c("1D", "2D","3D"), col = c('gray50','red','seagreen4'), pch = c(19), lty=c(1),bty = "n",lwd=2)
#rect(xleft = 0, xright = 0.9, ybottom = 0, ytop = 0.8)
######  Model with percolation
#
df<-subset(d,d$model=="percolation")
#######
plot(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV,xlim=c(1000,10000),ylim=c(0.001,1.0),pch = 20,col=0, xlab="Sites density", ylab="Coefficient of variation",log='y', yaxt="n")
lines(df[df$method=="prediction" & df$dimensionality==1 & df$additional_parameter_value==0.5,]$initial_density,df[df$method=="prediction" & df$dimensionality==1 & df$additional_parameter_value==0.5,]$CV, col='gray50',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==2 & df$additional_parameter_value==0.5,]$initial_density,df[df$method=="prediction" & df$dimensionality==2 & df$additional_parameter_value==0.5,]$CV, col='red',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==3 & df$additional_parameter_value==0.5,]$initial_density,df[df$method=="prediction" & df$dimensionality==3 & df$additional_parameter_value==0.5,]$CV, col='seagreen4',lwd=2)
points(df[df$method=="simulation" & df$dimensionality==1,]$initial_density,df[df$method=="simulation" & df$dimensionality==1,]$CV, col='gray50',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==2,]$initial_density,df[df$method=="simulation" & df$dimensionality==2,]$CV, col=c('red'),lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==3,]$initial_density,df[df$method=="simulation" & df$dimensionality==3,]$CV, col=c(rep('seagreen4',5),rep('seagreen3',5),rep('seagreen2',5), rep('seagreen1',5)),lwd=2,pch = 20)
#######
axis(2,at = c(0.001, 0.01, 0.1, 0.5))
text(x=1000,y=0.85,substitute(paste(bold('(b): With percolation'))), col='black', cex=1, adj=0)
######  Model with memory
#
df<-subset(d,d$model=="memory")
####### 
plot(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV,xlim=c(1000,10000),ylim=c(0.001,1.0),pch = 20,col=0, xlab="Sites density", ylab="Coefficient of variation",log='y', yaxt="n")
lines(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV, col='gray50',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==2,]$initial_density,df[df$method=="prediction" & df$dimensionality==2,]$CV, col='red',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==3,]$initial_density,df[df$method=="prediction" & df$dimensionality==3,]$CV, col='seagreen4',lwd=2)
points(df[df$method=="simulation" & df$dimensionality==1,]$initial_density,df[df$method=="simulation" & df$dimensionality==1,]$CV, col='gray50',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==2,]$initial_density,df[df$method=="simulation" & df$dimensionality==2,]$CV, col='red',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==3,]$initial_density,df[df$method=="simulation" & df$dimensionality==3,]$CV, col='seagreen4',lwd=2,pch = 20)
axis(2,at = c(0.001, 0.01, 0.1, 0.5))
text(x=1000,y=0.85,substitute(paste(bold('(c): With memory'))), col='black', cex=1,adj=0)
######  Model with jump
#
df<-subset(d,d$model=="jump")
####### 
plot(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV,xlim=c(1000,10000),ylim=c(0.001,1.0),pch = 20,col=0, xlab="Sites density", ylab="Coefficient of variation",log='y', yaxt="n")
lines(df[df$method=="prediction" & df$dimensionality==1 & df$additional_parameter_value==2,]$initial_density,df[df$method=="prediction" & df$dimensionality==1 & df$additional_parameter_value==2,]$CV, col='gray50',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==2 & df$additional_parameter_value==2,]$initial_density,df[df$method=="prediction" & df$dimensionality==2 & df$additional_parameter_value==2,]$CV, col='red',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==3 & df$additional_parameter_value==2,]$initial_density,df[df$method=="prediction" & df$dimensionality==3 & df$additional_parameter_value==2,]$CV, col='seagreen4',lwd=2)
points(df[df$method=="simulation" & df$dimensionality==1,]$initial_density,df[df$method=="simulation" & df$dimensionality==1,]$CV, col='gray50',lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==2,]$initial_density,df[df$method=="simulation" & df$dimensionality==2,]$CV, col=c(rep('red',5),rep('violetred1',5),rep('palevioletred1',5), rep('pink1',5)),lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==3,]$initial_density,df[df$method=="simulation" & df$dimensionality==3,]$CV, col=c(rep('seagreen4',5),rep('seagreen3',5),rep('seagreen2',5), rep('seagreen1',5)),lwd=2,pch = 20)
axis(2,at = c(0.001, 0.01, 0.1, 0.5))
text(x=1000,y=0.85,substitute(paste(bold('(d): With jump'))), col='black', cex=1,adj=0)
######  Model with drift
#
df<-subset(d,d$model=="drift")
####### 
plot(df[df$method=="prediction" & df$dimensionality==1,]$initial_density,df[df$method=="prediction" & df$dimensionality==1,]$CV,xlim=c(1000,10000),ylim=c(0.001,1.0),pch = 20,col=0, xlab="Sites density", ylab="Coefficient of variation",log='y', yaxt="n")
lines(df[df$method=="prediction" & df$dimensionality==1 & df$additional_parameter_value==0.05,]$initial_density,df[df$method=="prediction" & df$dimensionality==1 & df$additional_parameter_value==0.05,]$CV, col='gray50',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==2 & df$additional_parameter_value==0.05,]$initial_density,df[df$method=="prediction" & df$dimensionality==2 & df$additional_parameter_value==0.05,]$CV, col='red',lwd=2)
lines(df[df$method=="prediction" & df$dimensionality==3 & df$additional_parameter_value==0.05,]$initial_density,df[df$method=="prediction" & df$dimensionality==3 & df$additional_parameter_value==0.05,]$CV, col='seagreen4',lwd=2)
points(df[df$method=="simulation" & df$dimensionality==1,]$initial_density,df[df$method=="simulation" & df$dimensionality==1,]$CV, col=c(rep('gray20',5),rep('gray40',5),rep('gray60',5), rep('gray80',5)),lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==2,]$initial_density,df[df$method=="simulation" & df$dimensionality==2,]$CV,col=c(rep('red',5),rep('violetred1',5),rep('palevioletred1',5), rep('pink1',5)),lwd=2,pch = 20)
points(df[df$method=="simulation" & df$dimensionality==3,]$initial_density,df[df$method=="simulation" & df$dimensionality==3,]$CV,col=c(rep('seagreen4',5),rep('seagreen3',5),rep('seagreen2',5), rep('seagreen1',5)),lwd=2,pch = 20)
axis(2,at = c(0.001, 0.01, 0.1, 0.5))
text(x=1000,y=0.85,substitute(paste(bold('(e): With drift'))), col='black', cex=1,adj=0)
######
dev.off()
