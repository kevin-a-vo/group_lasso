rm(list = ls())

# install.packages("zoo")
# install.packages("RColorBrewer")
# install.packages("gglasso")

# library(gglasso)
# library(RColorBrewer)
# library(zoo)

hist=read.csv("historical_data.csv")
proj=read.csv("projections.csv")

hist=data.frame(Date=as.Date(as.yearqtr(hist[,1])),hist[,-1])
proj=data.frame(Date=proj[,1],proj[,-1])

plot(y=hist$Unemployment.Rate,x=hist$Date,main="Unemployment",lwd=2,col="slateblue",type="l",
     xlab="Time",ylab="Unemployment %")
grid()

#Remove Dates and Unemployment from the model matrix 
X=hist[,c(-1,-4)]
X=as.matrix(X)
Y=hist[,4]
grp=c(1,1,1,2,2,2,2,3,3,3,3,4,4)
fit=gglasso(x=X,y=Y,group=grp,loss='ls')
coef.mat=fit$beta

#Group1 enters the equation
g1=max(which(coef.mat[1,]==0))

#Group2 enters the equation
g2=max(which(coef.mat[4,]==0))

#Group3 enters the equation
g3=max(which(coef.mat[8,]==0))

#Group4 enters the equation
g4=max(which(coef.mat[12,]==0))

#Coefficient Plot. Let's also use some nice colors

cols=brewer.pal(5,name="Set1")

plot(fit$b0,main="Coefficient vs Step",
     ylab="Intercept",xlab="Step (decreasing Lambda =>)",
     col=cols[1],
     xlim=c(-1,100),
     ylim=c(5,max(fit$b0)),
     type="l",lwd=4)
grid()
par(new=T)

x=c(g1,g2,g3,g4)
y=c(fit$b0[g1],fit$b0[g2],fit$b0[g3],fit$b0[g4])

plot(x=x,y=y,pch=13,lwd=2,cex=2,col=cols[-1],
     xlim=c(-1,100),ylim=c(5,max(fit$b0)),
     xaxt='n',yaxt='n',xlab="",ylab="")

lmda=round(fit$lambda[c(g1,g2,g3,g4)],2)
text(x=x-0.5,y=y+0.1,labels=c("Group1","Group2","Group3","Group4"),pos=3,cex=0.9)
text(x=x-0.5,y=y-0.1,labels=paste("Lambda\n=",lmda),pos=1,cex=0.8)