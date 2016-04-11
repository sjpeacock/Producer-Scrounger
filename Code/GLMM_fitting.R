rm(list=ls())

setwd("~/Google Drive/ProducerScrounger/Producer-Scrounger")

library(cplm)
library(lme4)
library(gplots)
###################################################################################################################
# 1) Read in data
###################################################################################################################

source("Code/observations_per_trial.R")
obs.by.trial$exp_num<-as.factor(obs.by.trial$exp_num)
obs.by.trial$trial_num<-as.factor(obs.by.trial$trial_num)

# Theft attempts per coho per trial in small, medium, large groups of coho
x<-tapply(obs.by.trial$AT.by, obs.by.trial$trial_num, sum, na.rm=TRUE)
y<-numeric(24); for(i in 1:24) y[i]<-sum(obs.by.trial$n.coho[obs.by.trial$trial_num==i])
tapply(x/y, y, mean)
tapply(x/y, y, sd)/sqrt(tapply(y, y, length))

###################################################################################################################
# 2) Fitting models
###################################################################################################################

#create a list in which to store the parameter estimates for the models of:
# 1. strike rate, 2. capture rate, 3. prop. successful strikes, 4. attempted theft rate, 5. theft rate, 6. prop. of successful thefts, 7. rate of being a target of theft, 8. rate of escapes.
est<-list(); length(est)<-8

#Also record the number of trials with observations for each size
n<-list(); length(n)<-8

table2<-data.frame(response=rep(c("strike", "capture", "Prop successful", "AT by", "Thefts by", "Prop successful", "Thefts on", "Escapes"), each=2), model=rep(c("null", "size"), 8), df=rep(NA, 16), NLL=rep(NA, 16), Chisq=rep(NA, 16), ChiDf=rep(NA, 16), pvalue=rep(NA, 16))

A<-list(); length(A)<-8

fits<-list(); length(fits)<-16; dim(fits)<-c(8,2)

#-------------------------------------------------------------------------------------------------------------------
# GLMM for strikes
#-------------------------------------------------------------------------------------------------------------------
i<-1

fit1a<-cpglmm(strikes.per.fish~0+size+(1|part/exp_num/trial_num), data=obs.by.trial)
fit1b<-cpglmm(strikes.per.fish~1+(1|part/exp_num/trial_num), data=obs.by.trial)

fits[[i,1]]<-fit1a
fits[[i,2]]<-fit1b
A[[i]]<-anova(fit1a, fit1b) #Including size IS important!

est[[1]]<-cbind(summary(fit1a)$coefs[,1], summary(fit1a)$coefs[,1]-1.96*summary(fit1a)$coefs[,2], summary(fit1a)$coefs[,1]+1.96*summary(fit1a)$coefs[,2])
est[[1]]<-exp(est[[1]])

n[[1]]<-tapply(obs.by.trial$strikes.per.fish!=0, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

#-------------------------------------------------------------------------------------------------------------------
# GLMM for captures
#-------------------------------------------------------------------------------------------------------------------
i<-2
fit2a<-cpglmm(obs.by.trial$captures.per.fish~0+size+(1|part/exp_num/trial_num), data=obs.by.trial)
fit2b<-cpglmm(obs.by.trial$captures.per.fish~1+(1|part/exp_num/trial_num), data=obs.by.trial)

fits[[i,1]]<-fit2a
fits[[i,2]]<-fit2b
A[[i]]<-anova(fit2a, fit2b) #Including size IS important!

est[[2]]<-cbind(summary(fit2a)$coefs[,1], summary(fit2a)$coefs[,1]-1.96*summary(fit2a)$coefs[,2], summary(fit2a)$coefs[,1]+1.96*summary(fit2a)$coefs[,2])
est[[2]]<-exp(est[[2]])

n[[2]]<-tapply(obs.by.trial$captures.per.fish!=0, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

#-------------------------------------------------------------------------------------------------------------------
# logistic regression for proportion captured
#-------------------------------------------------------------------------------------------------------------------
i<-3

fit3a<-glmer(cbind(obs.by.trial$captures, obs.by.trial$strikes-obs.by.trial$captures)~0+size+(1|part/exp_num/trial_num), data=obs.by.trial, family="binomial")
fit3b<-glmer(cbind(obs.by.trial$captures, obs.by.trial$strikes-obs.by.trial$captures)~1+(1|part/exp_num/trial_num), data=obs.by.trial, family="binomial")
fits[[i,1]]<-fit3a
fits[[i,2]]<-fit3b
A[[i]]<-anova(fit3a, fit3b)

est[[3]]<-cbind(summary(fit3a)$coefficients[,1], summary(fit3a)$coefficients[,1]-1.96*summary(fit3a)$coefficients[,2], summary(fit3a)$coefficients[,1]+1.96*summary(fit3a)$coefficients[,2])
est[[3]]<-plogis(est[[3]])

n[[3]]<-tapply(is.na(obs.by.trial$captures.per.fish/(obs.by.trial$strikes.per.fish-obs.by.trial$captures.per.fish))==FALSE, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

#-------------------------------------------------------------------------------------------------------------------
# GLMM for attempted theft rate
#-------------------------------------------------------------------------------------------------------------------
i<-4

fit4a<-cpglmm(obs.by.trial$AT.by.per.fish~0+size+(1|part/exp_num/trial_num), data=obs.by.trial)
fit4b<-cpglmm(obs.by.trial$AT.by.per.fish~1+(1|part/exp_num/trial_num), data=obs.by.trial)
fits[[i,1]]<-fit4a
fits[[i,2]]<-fit4b
A[[4]]<-anova(fit4a, fit4b) #Including size IS important!

est[[4]]<-cbind(summary(fit4a)$coefs[,1], summary(fit4a)$coefs[,1]-1.96*summary(fit4a)$coefs[,2], summary(fit4a)$coefs[,1]+1.96*summary(fit4a)$coefs[,2])
est[[4]]<-exp(est[[4]])

n[[4]]<-tapply(obs.by.trial$AT.by.per.fish!=0, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'


#-------------------------------------------------------------------------------------------------------------------
# GLMM for successful theft rate
#-------------------------------------------------------------------------------------------------------------------
i<-5

fit5a<-cpglmm(obs.by.trial$T.by.per.fish~0+size+(1|part/exp_num/trial_num), data=obs.by.trial)
fit5b<-cpglmm(obs.by.trial$T.by.per.fish~1+(1|part/exp_num/trial_num), data=obs.by.trial)
fits[[i,1]]<-fit5a
fits[[i,2]]<-fit5b
A[[i]]<-anova(fit5a, fit5b) #Including size IS important!

est[[5]]<-cbind(summary(fit5a)$coefs[,1], summary(fit5a)$coefs[,1]-1.96*summary(fit5a)$coefs[,2], summary(fit5a)$coefs[,1]+1.96*summary(fit5a)$coefs[,2])
est[[5]]<-exp(est[[5]])

n[[5]]<-tapply(obs.by.trial$T.by.per.fish!=0, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

#-------------------------------------------------------------------------------------------------------------------
# Proportion of thefts that were successful
#-------------------------------------------------------------------------------------------------------------------
i<-6

fit6a<-glmer(cbind(obs.by.trial$T.by, obs.by.trial$AT.by-obs.by.trial$T.by)~0+size+(1|part/exp_num/trial_num), data=obs.by.trial, family="binomial")
fit6b<-glmer(cbind(obs.by.trial$T.by, obs.by.trial$AT.by-obs.by.trial$T.by)~1+(1|part/exp_num/trial_num), data=obs.by.trial, family="binomial")
fits[[i,1]]<-fit6a
fits[[i,2]]<-fit6b
A[[i]]<-anova(fit6a, fit6b)

est[[6]]<-cbind(summary(fit6a)$coefficients[,1], summary(fit6a)$coefficients[,1]-1.96*summary(fit6a)$coefficients[,2], summary(fit6a)$coefficients[,1]+1.96*summary(fit6a)$coefficients[,2])
est[[6]]<-plogis(est[[6]])

n[[6]]<-tapply((obs.by.trial$T.by/obs.by.trial$AT.by)==NaN, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

#-------------------------------------------------------------------------------------------------------------------
# Rate of being targeted by scroungers
#-------------------------------------------------------------------------------------------------------------------

i<-7
fit7a<-cpglmm(obs.by.trial$AT.on.per.fish~0+size+(1|part/exp_num/trial_num), data=obs.by.trial)
fit7b<-cpglmm(obs.by.trial$AT.on.per.fish~1+(1|part/exp_num/trial_num), data=obs.by.trial)
fits[[i,1]]<-fit7a
fits[[i,2]]<-fit7b
A[[i]]<-anova(fit7a, fit7b) #Including size IS important!

est[[7]]<-cbind(summary(fit7a)$coefs[,1], summary(fit7a)$coefs[,1]-1.96*summary(fit7a)$coefs[,2], summary(fit7a)$coefs[,1]+1.96*summary(fit7a)$coefs[,2])
est[[7]]<-exp(est[[7]])

n[[7]]<-tapply(obs.by.trial$AT.on.per.fish!=0, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

#-------------------------------------------------------------------------------------------------------------------
# Rate of escapes (prey lost)
#-------------------------------------------------------------------------------------------------------------------
i<-8
fit8a<-cpglmm(obs.by.trial$escapes.per.fish~0+size+(1|part/exp_num/trial_num), data=obs.by.trial)
fit8b<-cpglmm(obs.by.trial$escapes.per.fish~1+(1|part/exp_num/trial_num), data=obs.by.trial)
fits[[i,1]]<-fit8a
fits[[i,2]]<-fit8b
A[[i]]<-anova(fit8a, fit8b) #Including size IS important!

est[[8]]<-cbind(summary(fit8a)$coefs[,1], summary(fit8a)$coefs[,1]-1.96*summary(fit8a)$coefs[,2], summary(fit8a)$coefs[,1]+1.96*summary(fit8a)$coefs[,2])
est[[8]]<-exp(est[[8]])

n[[8]]<-tapply(obs.by.trial$escapes.per.fish!=0, obs.by.trial$size, sum)

table2$df[((i-1)*2+1):(i*2)]<-A[[i]]$Df
table2$NLL[((i-1)*2+1):(i*2)]<-A[[i]]$logLik
table2$ChiDf[((i-1)*2+1):(i*2)]<-A[[i]]$'Chi Df'
table2$Chisq[((i-1)*2+1):(i*2)]<-A[[i]]$Chisq
table2$pvalue[((i-1)*2+1):(i*2)]<-A[[i]]$'Pr(>Chisq)'

###################################################################################################################
# Table: Model comparison
###################################################################################################################
# write.csv(table2, file="table2_cpglmm.csv")

###################################################################################################################
# 3) Plotting results
# - bars are the model estimates for the number of observations per coho per trial
# - numbers on bars are the number of trials in which the given behaviour was observed for the given size class
###################################################################################################################

############################################################################################################
# Figure 1: barplots of obs
############################################################################################################

colp<-c('#e66101','#fdb863','white','#b2abd2','#5e3c99')

ylabs<-list("Strikes", "Captures", "Proportion of strikes \n resulting in captures", "Attempted thefts by", "Successful thefts by", "Proportion of attempted thefts \n that were successful", "Thefts on", "Escapes")

yaxs.pts<-list(seq(0,8,2), seq(0,3,1), seq(0, 0.8, 0.2), seq(0, 0.8, 0.2), seq(0, 0.08, 0.02), seq(0, 1.2, 0.3))
cols<-c(colp[1], 0, colp[5])

quartz(width=5.07, height=4, pointsize=12)
par(mfrow=c(2,3), mar=c(3,4.5,2.5,1), mgp=c(2.5, 1, 0), oma=c(1,0,0,0), mgp=c(3, 0.8, 0))
j<-0
for(i in c(1:2,8,4:5,7)){
	j<-j+1
	bp<-barplot2(est[[i]][3:1,1], plot.ci=TRUE, ci.l=est[[i]][3:1,2], ci.u=est[[i]][3:1,3], names.arg=c("L", "M", "S")[3:1], ylab=ylabs[[i]], xlab="", ci.width=0.2, las=1, yaxt="n", cex.lab=1.2, cex.axis=1.2, col=cols)
	axis(side=2, at=yaxs.pts[[j]], labels=format(yaxs.pts[[j]], nsmall=1), las=1, xpd=NA, cex.axis=1.2)
	abline(h=0)
	mtext(side=3, adj=0, line=1, letters[j], cex=0.83)
	text(bp, est[[i]][3:1,3], pos=3, n[[i]][c(3:1)], xpd=NA)
}
# mtext(side=2, outer=TRUE, "Observations of behaviour per coho per hour", line=0.5, cex=1)
mtext(side=1, outer=TRUE, "Size of coho salmon", line=-0.5, cex=0.83)
