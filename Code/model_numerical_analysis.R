rm(list=ls())

library(phaseR);library(animation); library(rootSolve)
library(gplots)
source("Code/model_functions.R")

######################################################################################
# Simple model: Figure 4
######################################################################################

lambda<-0.01
gamma<-0.02
alpha<-0.01
G<-10
qstar<-1-gamma/(alpha*G)

q<-seq(0,1,0.01)
y<-E.simple(q, G=10)

quartz(width=5.07, height=2.6, pointsize=10)

par(mfrow=c(1,2), mar=c(4,3,2,1))
plot(q, y[,1], "l", lty=1, xlab="", ylab="", bty="l", las=1, lwd=3, col=grey(0.8), ylim=c(0,1), yaxt="n")
arrows(par()$usr[1], -0, par()$usr[1], par()$usr[2], xpd=NA, length=0.08)
mtext(side=2, line=1, "Payoff")
lines(q, y[,2])
points(q[which(round(y[,1]-y[,2],3)==0)], y[which(round(y[,1]-y[,2],3)==0),2], pch=8)
legend("topright", lwd=c(3,1),col=c(grey(0.8), 1), c(expression(P^"*"), expression(S^"*")), bty="n", xpd=NA)
mtext(side=3, adj=0, line=0.5, "a")

plot(q, dq.simple(q), "l", bty="l", las=1, xlab="", ylab="", yaxt="n")
arrows(par()$usr[1], 0, par()$usr[1], par()$usr[4], xpd=NA, length=0.08)
abline(h=0)
mtext(side=2, line=1, expression(paste(dq, " / d", tau)))
points(q[which(round(y[,1]-y[,2],3)==0)], 0, pch=8)
mtext(side=3, adj=0, line=0.5, "b")

mtext(side=1, expression(paste("Proportion scroungers in group (", italic(q), ")")), outer=TRUE, line=-1)


######################################################################################
# Effect of alpha and group size
######################################################################################

#----------------------------------------------------------------------------------------
# Parameterization
#----------------------------------------------------------------------------------------

# Base parameteers
lambda<-0.01									# Rate of prey capture
alpha<-matrix(c(0.006,0.01,0.002,NA), 2, 2) # Rates of scrounging
theta<-0.5									# Proportion of predators that are small
gamma<-c(0.0165,0.0315) 					# Rates of prey consumption/escape

# Parameters varied in sensitivity analysis
alpha.all<-list(
	matrix(c(0.006,0.01,0.002,0.01), 2, 2), 
	matrix(c(0.006,0.01,0.002,0), 2, 2),
	matrix(rep(0.006,4), 2, 2))

G<-10^seq(0,2,0.02)

#----------------------------------------------------------------------------------------
# Determining equilibria
#----------------------------------------------------------------------------------------

eqm<-data.frame(alpha=rep(1:length(alpha.all), each=length(G)), G=rep(G, length(alpha.all)), q1=rep(NA, length(alpha.all)*length(G)), q2=rep(NA, length(alpha.all)*length(G)))

for(s in 1:length(alpha.all)){ #For each scenario
	for(i in 1:length(G)){ #For each group size
		
		params<-list(lambda=lambda, alpha=alpha.all[[s]], theta=theta, gamma=gamma, G=G[i])
		
		eqm[which(eqm$alpha==s & eqm$G==G[i]),3:4]<-steady(y=c(0.5,0.5), func=dq, parms=params, method="runsteady")$y
			
		}
	}
	

# Plot to check for inconsistencies that might indicate the unstable equilirbium was chosen.
par(mfrow=c(1,3), mar=c(1,1,2,1), oma=c(3,3,0,0))
for(s in 1:length(alpha.all)){
	plot(G, eqm[which(eqm$alpha==s),3], col=2, ylim=c(0,1), "l", bty="l", main=paste("Scenario", LETTERS[s]), ylab="", yaxt="n", xaxt="n")
	lines(G, eqm[which(eqm$alpha==s),4], col=4)
	if(s==1|s==3) axis(side=2) else axis(side=2, labels=FALSE)
	if(s==4|s==3) axis(side=1) else axis(side=1, labels=FALSE)

}
mtext(side=2, outer=TRUE, "Equilibrium proportion of scroungers", line=1.5)
mtext(side=1, outer=TRUE, "Group size", line=1.5)

legend("bottomright", xpd=NA, lwd=1, col=c(2,4), c("Small predators", "Large predators"), bty="n")

#----------------------------------------------------------------------------------------
# Determining payoff
#----------------------------------------------------------------------------

Payoff<-matrix(rep(NA, length(alpha.all)*length(G)*5)); dim(Payoff)<-c(length(alpha.all), length(G), 5)
for(s in 1:length(alpha.all)){
	for(i in 1:length(G)){
		
		# Set parameters and equilibrium (found above)
		params<-list(lambda=lambda, alpha=alpha.all[[s]], theta=theta, gamma=gamma, G=G[i])
		
		qs<-eqm[which(eqm$alpha==s&eqm$G==G[i]),3:4]
		
		# Payoff to each strategy:
		Payoff[s,i,1]<-as.numeric(P1(params, q=qs))
		Payoff[s,i,2]<-as.numeric(P2(params, q=qs))
		Payoff[s,i,3]<-as.numeric(S1(params, q=qs))
		Payoff[s,i,4]<-as.numeric(S2(params, q=qs))
		
		# Average payoff to the group:
		Payoff[s,i,5]<-as.numeric(params[['theta']]*(1-qs[1])*Payoff[s,i,1]+(1-params[['theta']])*(1-qs[2])*Payoff[s,i,2]+params[['theta']]*qs[1]*Payoff[s,i,3]+(1-params[['theta']])*qs[2]*Payoff[s,i,4])
	}
}

#----------------------------------------------------------------------------------------
# Plot Figure 5
#----------------------------------------------------------------------------------------
colp<-c('#e66101','#fdb863','white','#b2abd2','#5e3c99')

BScol<-c(colp[1],colp[5])
BScol2<-colp[c(1,2,4,5)]
G<-10^seq(0,2,0.02)

quartz(width=6.97, height=6, pointsize=10)
par(mfcol=c(3,3), mar=c(4,4,3,1), oma=c(1,2.5,1,1), mgp=c(2.5, 1, 0))	
G5<-15

for(s in 1:length(alpha.all)){
	ind<-which(eqm$alpha==s)
	
	params<-list(lambda=lambda, alpha=alpha.all[[s]], theta=theta, gamma=gamma, G=G5)
		
	qs<-eqm[which(eqm$alpha==s&eqm$G==G5),3:4]
				
	FF<-flowField(dq, x.lim=c(0.01,1), y.lim=c(0.01,1), parameters=params, add=FALSE, xlab="", ylab="", points=10, las=1, main="", bty="l", arrow.head=0.03, lwd=0.5, yaxs="i", xaxs="i", colour=grey(0.6), cex.axis=1.5, cex.lab=1.5)
	segments(x0=c(0,0,1,1), y0=c(0,0,1,1), x1=c(1,0,0,1), y1=c(0,1,1,0), col=BScol[c(2,1,2,1)])
	if(s==1){
		mtext(side=2, "Proportion of large fish", line=5, col=BScol[2])
		mtext(side=2, expression(paste("scrounging (", italic(q[2]), ")")), line=3.6, col=BScol[2])
		}
	N<-nullclines(dq, x.lim=c(0,1), y.lim=c(0,1), parameters=params, col=BScol)
	points(eqm[which(eqm$alpha==s&round(eqm$G)==G5),3], eqm[which(eqm$alpha==s&round(eqm$G)==G5),4], pch=8)		
	mtext(side=3, letters[s], line=0.5, adj=0)
	if(s==2) mtext(side=1, line=3, expression(paste("Proportion of small fish scrounging (", italic(q[1]), ")")), col=BScol[1])
	
	mtext(side=3, paste("Scenario ", LETTERS[s]), line=1.5)
	
	plot(G, eqm[ind,3], col=BScol[1], "l", ylim=c(0,1), las=1, bty="l", ylab="", xlab="", lwd=1.2, main="", cex.axis=1.5)
	lines(G, eqm[ind,4], col=BScol[2], lwd=1.2)
	mtext(side=3, letters[s+3], line=0.5, adj=0)
	if(s==1){
		mtext(side=2, "Equilibrium proportion of", xpd=NA, line=5)
		mtext(side=2, expression(paste("scroungers (", italic(q[i]), "*)")), line=3.6)
		}
	if(s==2) mtext(side=1, line=3, "Group size (G)")
	
	plot(G, Payoff[s,,1], "l", bty="l", ylab="", xlab="", main="", ylim=c(0,0.6), las=1, lwd=3, col=BScol2[2], cex.axis=1.5)
	lines(G, Payoff[s,,2], lwd=3, col=BScol2[3])
	lines(G, Payoff[s,,3], col=BScol2[1], lty=3, lwd=1.2)
	lines(G, Payoff[s,,4], col=BScol2[4],  lty=3, lwd=1.2)
	mtext(side=3, letters[s+6], line=0.5, adj=0)
	if(s==1) mtext(side=2, "Payoff", line=3.6)
	if(s==2) mtext(side=1, line=3, "Group size (G)")
	if(s==3) legend("topright", lwd=c(3,3,1.2,1.2), lty=c(1,1,3,3), col=BScol2[c(2,3,1,4)],  c(expression(italic(P)[1]), expression(italic(P)[2]), expression(italic(S)[1]), expression(italic(S)[2])), bty="n", xpd=NA, cex=1.5)
		
		}


######################################################################################
# Effect of gammas (group size = 15, theta = 0.5)
######################################################################################

#----------------------------------------------------------------------------------------
# Parameters
#----------------------------------------------------------------------------

G<-15 # Fix group size at 15

gamma1<-seq(0.001,0.05, 0.001)
gamma2<-seq(0.001, 0.05, 0.001)

eqm.gamma<-list();length(eqm.gamma)<-length(alpha.all)
for(i in 1:length(alpha.all)){
	eqm.gamma[[i]]<-matrix(rep(NA, length(gamma1)*length(gamma2)*2))
	dim(eqm.gamma[[i]])<-c(length(gamma1), length(gamma2), 2)
	}

#----------------------------------------------------------------------------------------
# Calculate equilibrium q
#----------------------------------------------------------------------------------------

for(s in 1:length(alpha.all)){
	for(i in 1:length(gamma1)){
		for(j in 1:length(gamma2)){
			
			params<-list(lambda=lambda, alpha=alpha.all[[s]], theta=theta, gamma=c(gamma1[i], gamma2[j]), G=G)	
			eqm.gamma[[s]][i,j,]<-steady(y=c(0.55,0.5), func=dq, parms=params, method="runsteady")$y
		}
	}
}

q_gamma<-list(); length(q_gamma)<-9; dim(q_gamma)<-c(3,3)
for(s in 1:3){
	for(i in 1:3){
		if(i<3) q_gamma[[s,i]]<-eqm.gamma[[s]][,,i] else q_gamma[[s,i]]<-0.5*eqm.gamma[[s]][,,2]+0.5*eqm.gamma[[s]][,,1]
	}
}

#----------------------------------------------------------------------------------------
# Plot Figure 6
#----------------------------------------------------------------------------------------
palette2D<-colorRampPalette(c("black", "white"))

quartz(width=6.97, height=4, pointsize=10)
layout(matrix(c(rep(1:2, 2), rep(3:4, 2), rep(5:6, 2), rep(7,3)), nrow=2, ncol=7, byrow=FALSE))
par(mar=c(2,2,1,1), oma=c(3,4,3,1))

for(s in 1:3){
	for(i in 1:2){
		filledContour(x=gamma1, y=gamma2, z=round(q_gamma[[s,i]], 3), xlab="", ylab="", color.palette= palette2D, zlim=c(0,1), axes=FALSE, xlim=c(0,0.05), ylim=c(0,0.05), nlevels=10)
		if(s==1) axis(side=2, las=1, cex.axis=1.3) else axis(side=2, labels=FALSE)
		if(i==2) axis(side=1, cex.axis=1.3) else axis(side=1, labels=FALSE)
		contour(x=gamma1, y=gamma2, z=round(q_gamma[[s,i]], 3), add=TRUE, xlim=c(0,0.05), ylim=c(0,0.05))
		if(i==1) mtext(side=3, line=2, paste("Scenario", LETTERS[s]))
		mtext(side=3, line=0.5, letters[(i-1)*3+s], adj=0)
		points(0.0165,0.0315, col=colp[1], pch=8)
		}
	}

plot.scale.bar(zlim=c(0,1), color.palette=palette2D, nlevels=10, cex.axis=1.5)
mtext(side=4,expression(paste("Equilibrium proportion of scroungers (", italic(q), "*)")), line=3.5)
mtext(side=1, outer=TRUE, expression(paste("Rate of consumption/loss by small predators (", gamma[1], ")")), line=2)
mtext(side=2, outer=TRUE, expression(paste("Rate of consumption/loss by large predators (", gamma[2], ")")), line=2)


######################################################################################
# Effect of theta
######################################################################################

#----------------------------------------------------------------------------------------
# Parameters
#----------------------------------------------------------------------------

# Base parameters
lambda<-0.01									# Rate of prey capture
gamma<-c(0.0165,0.0315) 					# Rates of prey consumption/escape

# Parameters varied in sensitivity analysis
alpha.all<-list(
	matrix(c(0.006,0.01,0.002,0.01), 2, 2), 
	matrix(c(0.006,0.01,0.002,0), 2, 2),
	matrix(rep(0.006,4), 2, 2))

G.all<-10^seq(0,2,0.02)
theta.all<-seq(0,1,0.01)


#----------------------------------------------------------------------------------------
# Calculate equilibrium q
#----------------------------------------------------------------------------------------

eqm.theta<-list();length(eqm.theta)<-length(alpha.all)
for(i in 1:length(alpha.all)){
	eqm.theta[[i]]<-matrix(rep(NA, length(G.all)*length(theta.all)*2))
	dim(eqm.theta[[i]])<-c(length(G.all), length(theta.all), 2)
	}

for(s in 1:length(alpha.all)){ #For each scenario
	for(i in 1:length(G.all)){ #For each group size
		for(j in 1:length(theta.all)){#for each theta
			params<-list(lambda=lambda, alpha=alpha.all[[s]], theta=theta.all[j], gamma=gamma, G=G.all[i])
			eqm.theta[[s]][i,j,]<-steady(y=c(0.55,0.5), func=dq, parms=params, method="runsteady")$y
		}
	}
}

q_theta<-list(); length(q_theta)<-9; dim(q_theta)<-c(3,3)
for(s in 1:3){
	for(k in 1:3){
		if(k<3) q_theta[[s,k]]<-eqm.theta[[s]][,,k] else{
			q_theta[[s,k]]<-matrix(NA, nrow=length(G.all), ncol=length(theta.all))
			for(j in 1:length(theta.all)){
				q_theta[[s,k]][,j]<-theta.all[j]*eqm.theta[[s]][,j,1]+(1-theta.all[j])*eqm.theta[[s]][,j,2]}
				}
	}
}



#----------------------------------------------------------------------------------------
# Plot Figure 7
#----------------------------------------------------------------------------------------

quartz(width=6.97, height=4, pointsize=10)
layout(matrix(c(rep(1:2, 2), rep(3:4, 2), rep(5:6, 2), rep(7,3)), nrow=2, ncol=7, byrow=FALSE))
par(mar=c(2,2,1,1), oma=c(3,4,3,1))


for(s in 1:3){
	for(i in 1:2){
		filledContour(x=G.all, y=theta.all, z=round(q_theta[[s,i]], 3), xlab="", ylab="", color.palette=palette2D, zlim=c(0,1), axes=FALSE, nlevels=10)
		if(s==1) axis(side=2, las=1, cex.axis=1.3) else axis(side=2, labels=FALSE)
		if(i==2) axis(side=1, cex.axis=1.3) else axis(side=1, labels=FALSE)
		contour(x=G.all, y=theta.all, z=round(q_theta[[s,i]], 3), add=TRUE)
		if(i==1) mtext(side=3, line=2, paste("Scenario", LETTERS[s]))
		mtext(side=3, line=0.5, letters[(i-1)*3+s], adj=0)
		}
	}

plot.scale.bar(zlim=c(0,1), color.palette= palette2D, nlevels=10)
mtext(side=4,expression(paste("Equilibrium proportion of scroungers (", italic(q), "*)")), line=3.5)
mtext(side=1, outer=TRUE, expression(paste("Group size (", italic(G), ")")), line=2)
mtext(side=2, outer=TRUE, expression(paste("Proportion of group that is small (", theta, ")")), line=2)
