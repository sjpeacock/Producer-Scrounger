############################################################################################################
# Read in data and define functions
############################################################################################################
z<-read.csv("Data/Study2_observations.csv")

# Define bootstrap function to calculate confidence intervals
boot<-function(x, n=1000){
	xi<-matrix(sample(x, n*length(x), replace=TRUE), nrow=n, ncol=length(x))
	mean.x<-apply(xi, 1, mean, na.rm=TRUE)
	ret<-c(mean(x, na.rm=TRUE), quantile(mean.x, c(0.025, 0.975), na.rm=TRUE))
	return(ret)
}

# Which experiments had different sizes and which had the same size?
expdiff<-c(2,3,4,6)
expsame<-c(1,5,7,8,9,10)

############################################################################################################
# A) Comparison of small and large in mixed trials
############################################################################################################

A<-matrix(NA); length(A)<-c(3*3*2); dim(A)<-c(3,2,3)
za<-subset(z, is.element(z$expt_num, expdiff))

#capture small
cs<-tapply(za$expt_num[which(za$obs=="C"&za$by=="S")], za$expt_num[which(za$obs=="C"&za$by=="S")], length)
A[,1,1]<-boot(cs)

#capture large
cl<-tapply(za$expt_num[which(za$obs=="C"&za$by=="L")], za$expt_num[which(za$obs=="C"&za$by=="L")], length)
A[,2,1]<-boot(cl)

#AT small
ats<-tapply(za$expt_num[which((za$obs=="AT"|za$obs=="T")&za$by=="S")], za$expt_num[which((za$obs=="AT"|za$obs=="T")&za$by=="S")], length)
A[,1,2]<-boot(ats)

#AT large
atl<-tapply(za$expt_num[which((za$obs=="AT"|za$obs=="T")&za$by=="L")], za$expt_num[which((za$obs=="AT"|za$obs=="T")&za$by=="L")], length)
atl<-c(atl,0)
A[,2,2]<-boot(atl)

#T small
ts<-tapply(za$expt_num[which(za$obs=="T"&za$by=="S")], za$expt_num[which(za$obs=="T"&za$by=="S")], length)
ts<-rep(0,4)
A[,1,3]<-boot(ts)

#AT large
tl<-tapply(za$expt_num[which(za$obs=="T"&za$by=="L")], za$expt_num[which(za$obs=="T"&za$by=="L")], length)
tl<-c(tl,rep(0,2))
A[,2,3]<-boot(tl)


############################################################################################################
# B) Comparison of same sized and mixed trials
############################################################################################################
B<-matrix(NA); length(B)<-c(3*3*2); dim(B)<-c(3,2,3)

za<-subset(z, is.element(z$expt_num, expdiff))
zb<-subset(z, is.element(z$expt_num, expsame))
#capture mixed
cms<-tapply(za$expt_num[which(za$obs=="C")], za$expt_num[which(za$obs=="C")], length)
B[,1,1]<-boot(cms)
#capture same
css<-tapply(zb$expt_num[which(zb$obs=="C")], zb$expt_num[which(zb$obs=="C")], length)
B[,2,1]<-boot(css)
#AT mixed
atms<-tapply(za$expt_num[which(za$obs=="AT"|za$obs=="T")], za$expt_num[which(za$obs=="AT"|za$obs=="T")], length)
B[,1,2]<-boot(atms)
#AT same
atss<-tapply(zb$expt_num[which(zb$obs=="AT"|zb$obs=="T")], zb$expt_num[which(zb$obs=="AT"|zb$obs=="T")], length)
atss<-c(atss,0)
B[,2,2]<-boot(atss)
#T small
tms<-tapply(za$expt_num[which(za$obs=="T")], za$expt_num[which(za$obs=="T")], length)
tms<-c(tms,0,0)
B[,1,3]<-boot(tms)
#AT large
tss<-tapply(zb$expt_num[which(zb$obs=="T")], zb$expt_num[which(zb$obs=="T")], length)
tss<-c(tss,0)
B[,2,3]<-boot(tss)

############################################################################################################
# Plot Figure 3
############################################################################################################

quartz(width=5.07, height=2.7, pointsize=10)
par(mfrow=c(1,2), mar=c(3,2,2,1), oma=c(0,2,0,0))
a<-barplot2(A[1,,], plot.ci=TRUE, ci.l=A[2,,], ci.u=A[3,,], beside=TRUE, col=colp[c(1,5)], las=1, ci.lwd=1.2)
text(apply(a, 2, mean), -0.8, c("Capture", "Theft \n attempt", "Theft"), xpd=NA)
abline(h=0)
legend("topright", fill=colp[c(1,5)], c("Small coho", "Large coho"), bty="n")
mtext(side=3, adj=0, line=0.5, "a")

b<-barplot2(B[1,,], plot.ci=TRUE, ci.l=B[2,,], ci.u=B[3,,], beside=TRUE, col=c(grey(0.6),0), las=1, ci.lwd=1.2)
abline(h=0)
text(apply(b, 2, mean), -1.7, c("Capture", "Theft \n attempt", "Theft"), xpd=NA)
legend("topright", fill=c(grey(0.6),0), c("Mixed-size", "Same-size"), bty="n")
mtext(side=3, adj=0, line=0.5, "b")
mtext(side=2, outer=TRUE, "Mean observations per trial", line=0.5)

