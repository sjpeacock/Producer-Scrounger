###################################################################################################################
# Data organization
###################################################################################################################

setwd("~/Google Drive/ProducerScrounger/Producer-Scrounger")


#-------------------------------------------------------------------------------------------------------------------
# 1) Read-in raw data
#-------------------------------------------------------------------------------------------------------------------

# Size of coho predators in each cohort

data_size<-read.csv("Data/Study1_coho_size.csv")
data_size<-subset(data_size, year==2014)

# n.sizes is a list (length=2) giving the lower and upper forklengths defining small, medium and large size classes and the number of coho (n) in each size class.
n.sizes<-list(); length(n.sizes)<-2
for(i in 1:2){
	n.sizes[[i]]<-matrix(NA, nrow=3, ncol=4)
	colnames(n.sizes[[i]])<-c("S", "M", "L", "Total")
	rownames(n.sizes[[i]])<-c("lower", "upper","n")
	S<-data_size$length[data_size$location==unique(data_size$location)[i]]
	int<-(max(S)-min(S))/3
	n.sizes[[i]][1,1:3]<-round(c(min(S),(min(S)+int),(min(S)+2*int)))
	n.sizes[[i]][2,1:3]<-round(c((min(S)+int),(min(S)+2*int),max(S)))
	
	n.sizes[[i]][3,1]<-length(which(S<=(min(S)+int)))
	n.sizes[[i]][3,2]<-length(which(S>(min(S)+int)&S<=(min(S)+2*int)))
	n.sizes[[i]][3,3]<-length(which(S>(min(S)+2*int)))
	n.sizes[[i]][3,4]<-length(S)
	}
	
# The matrix p gives the proportion of total coho for Study 1A and Study 1B that are in small, medium and large size classes.
# These proportions are used to determine the number of behavioural observations per coho of a given size (below).
p<-matrix(c(n.sizes[[1]][3,1:3]/n.sizes[[1]][3,4], n.sizes[[2]][3,1:3]/n.sizes[[2]][3,4]), 2, 3, byrow=TRUE)	
rownames(p)<-c("Study 1A", "Study 1B")
colnames(p)<-c("S", "M", "L")

#-------------------------------------------------------------------------------------------------------------------
# Summary of experiments incl. numner of predators and prey

data_exp<-read.csv("Data/Study1_summary.csv")

# Trial 1A (lice) is not included in experiment data because we only had audio of observations for that experiment; the number of behaviours was not recorded.

n_1A<-sum(data_exp$n.coho[data_exp$exp<=20])
n_1B<-sum(data_exp$n.coho[data_exp$exp>20])

n<-rbind(p[1,]*n_1A, p[2,]*n_1B) # "Number" of coho "used" in each size category.

#-------------------------------------------------------------------------------------------------------------------
# Recorded observations during first hour
data_obs<-read.csv("Data/Study1_observations.csv")

data_obs$elapsed_time<-as.POSIXlt(data_obs$elapsed_time, format="%M:%S") # formats time as time in R

# Split experiments into Study 1A and Study 1B, which used independent cohorts of coho predators 
data_obs$part<-factor(c(rep("A", dim(data_obs[data_obs$expt_num<=7,])[1]), rep("B", dim(data_obs[data_obs$expt_num>7,])[1])))

# Split the theft observations into size of the scrounger and size of the target
data_obs$T_by<-rep(NA, dim(data_obs)[1])
data_obs$T_on<-rep(NA, dim(data_obs)[1])
ind<-which(data_obs$obs=="AT"|data_obs$obs=="T")
X<-strsplit(as.character(data_obs$by[ind]), split="/")
for(i in 1:length(ind)){
	data_obs$T_by[ind[[i]]]<-X[[i]][1]
	data_obs$T_on[ind[[i]]]<-X[[i]][2]
}
data_obs$T_by[which(data_obs$T_by=="?")]<-NA
data_obs$T_on[which(data_obs$T_on=="?")]<-NA

#-------------------------------------------------------------------------------------------------------------------
# 2) Create new "per-trial" data frame
#-------------------------------------------------------------------------------------------------------------------

data_obs$trial_num<-paste(data_obs$expt_num, data_obs$treatment)
nt<-length(unique(data_obs$trial_num))
for(i in 1:nt){
	data_obs$trial_num[data_obs$trial_num==unique(data_obs$trial_num)[i]]<-i
}

obs.by.trial<-data.frame(
	exp_num=rep(data_obs$expt_num[data_obs$obs_num==1], each=3), 
	part=rep(data_obs$part[data_obs$obs_num==1], each=3), 
	trial_num=rep(1:nt, each=3), 
	n.coho=rep(data_exp$n.coho[data_exp$n.coho>0], each=3),
	treatment=rep(data_obs$treatment[data_obs$obs_num==1], each=3), 
	net_pen=rep(data_obs$net_pen[data_obs$obs_num==1], each=3), 
	size=rep(c("S", "M", "L"), nt), 
	strikes=rep(0, nt*3), captures=rep(0, nt*3))

obs.by.trial$n.coho<-c(obs.by.trial$n.coho[obs.by.trial$part=="A"]*rep(p[1,], 12), obs.by.trial$n.coho[obs.by.trial$part=="B"]*rep(p[2,], 12))

#-------------------------------------------------------------------------------------------------------------------
# Strikes and captures per trial

for(i in 1:nt){
	I<-tapply(data_obs$obs[data_obs$trial_num==i&(data_obs$obs=="S"|data_obs$obs=="C")], data_obs$by[data_obs$trial_num==i&(data_obs$obs=="S"|data_obs$obs=="C")], length)
	obs.by.trial$strikes[obs.by.trial$trial_num==i&obs.by.trial$size=="S"]<-as.numeric(I[which(names(I)=="S")])
	obs.by.trial$strikes[obs.by.trial$trial_num==i&obs.by.trial$size=="M"]<-as.numeric(I[which(names(I)=="M")])
	obs.by.trial$strikes[obs.by.trial$trial_num==i&obs.by.trial$size=="L"]<-as.numeric(I[which(names(I)=="L")])
	
	I<-tapply(data_obs$obs[data_obs$trial_num==i&(data_obs$obs=="C")], data_obs$by[data_obs$trial_num==i&(data_obs$obs=="C")], length)
	obs.by.trial$captures[obs.by.trial$trial_num==i&obs.by.trial$size=="S"]<-as.numeric(I[which(names(I)=="S")])
	obs.by.trial$captures[obs.by.trial$trial_num==i&obs.by.trial$size=="M"]<-as.numeric(I[which(names(I)=="M")])
	obs.by.trial$captures[obs.by.trial$trial_num==i&obs.by.trial$size=="L"]<-as.numeric(I[which(names(I)=="L")])
	
}

#-------------------------------------------------------------------------------------------------------------------
# Theft attempts per trial

obs.by.trial$AT.by<-rep(NA, dim(obs.by.trial)[1])
obs.by.trial$T.by<-rep(NA, dim(obs.by.trial)[1])
obs.by.trial$AT.on<-rep(NA, dim(obs.by.trial)[1])
obs.by.trial$T.on<-rep(NA, dim(obs.by.trial)[1])

for(i in 1:nt){
	I<-tapply(data_obs$obs[data_obs$trial_num==i&(data_obs$obs=="AT"|data_obs$obs=="T")], data_obs$by[data_obs$trial_num==i&(data_obs$obs=="AT"|data_obs$obs=="T")], length)
	I<-I[which(is.na(I)==FALSE)]
		if(length(I)>0){
		I.names<-strsplit(names(I), "/")
		by<-length(I); on<-by
		for(j in 1:length(I)){
			by[j]<-I.names[[j]][1]
			on[j]<-I.names[[j]][2]
			}
		
		if(length(which(by=="S"))>0) obs.by.trial$AT.by[obs.by.trial$trial_num==i&obs.by.trial$size=="S"]<-sum(as.numeric(I[which(by=="S")]))
		if(length(which(by=="M"))>0) obs.by.trial$AT.by[obs.by.trial$trial_num==i&obs.by.trial$size=="M"]<-sum(as.numeric(I[which(by=="M")]))
		if(length(which(by=="L"))>0) obs.by.trial$AT.by[obs.by.trial$trial_num==i&obs.by.trial$size=="L"]<-sum(as.numeric(I[which(by=="L")]))
		
		if(length(which(on=="S"))>0) obs.by.trial$AT.on[obs.by.trial$trial_num==i&obs.by.trial$size=="S"]<-sum(as.numeric(I[which(on=="S")]))
		if(length(which(on=="M"))>0) obs.by.trial$AT.on[obs.by.trial$trial_num==i&obs.by.trial$size=="M"]<-sum(as.numeric(I[which(on=="M")]))
		if(length(which(on=="L"))>0) obs.by.trial$AT.on[obs.by.trial$trial_num==i&obs.by.trial$size=="L"]<-sum(as.numeric(I[which(on=="L")]))
		
		}
		
	I<-tapply(data_obs$obs[data_obs$trial_num==i&data_obs$obs=="T"], data_obs$by[data_obs$trial_num==i&data_obs$obs=="T"], length)
	I<-I[which(is.na(I)==FALSE)]
	if(length(I)>0){
		I.names<-strsplit(names(I), "/")
		by<-length(I)
		for(j in 1:length(I)) by[j]<-I.names[[j]][1]
			
		if(length(which(by=="S"))>0) obs.by.trial$T.by[obs.by.trial$trial_num==i&obs.by.trial$size=="S"]<-sum(as.numeric(I[which(by=="S")]))
		if(length(which(by=="M"))>0) obs.by.trial$T.by[obs.by.trial$trial_num==i&obs.by.trial$size=="M"]<-sum(as.numeric(I[which(by=="M")]))
		if(length(which(by=="L"))>0) obs.by.trial$T.by[obs.by.trial$trial_num==i&obs.by.trial$size=="L"]<-sum(as.numeric(I[which(by=="L")]))
		}	
	}

#-------------------------------------------------------------------------------------------------------------------
# Escapes per trial
obs.by.trial$escapes<-rep(NA, dim(obs.by.trial)[1])

for(i in 1:nt){
	I<-tapply(data_obs$obs[data_obs$trial_num==i&data_obs$obs=="L"], data_obs$by[data_obs$trial_num==i&data_obs$obs=="L"], length)
	
	obs.by.trial$escapes[obs.by.trial$trial_num==i&obs.by.trial$size=="S"]<-as.numeric(I[which(names(I)=="S")])
	obs.by.trial$escapes[obs.by.trial$trial_num==i&obs.by.trial$size=="M"]<-as.numeric(I[which(names(I)=="M")])
	obs.by.trial$escapes[obs.by.trial$trial_num==i&obs.by.trial$size=="L"]<-as.numeric(I[which(names(I)=="L")])
		
}

#-------------------------------------------------------------------------------------------------------------------
# Per-fish rates
obs.by.trial$strikes.per.fish<-obs.by.trial$strikes/obs.by.trial$n.coho
obs.by.trial$captures.per.fish<-obs.by.trial$captures/obs.by.trial$n.coho
obs.by.trial$AT.by.per.fish<-obs.by.trial$AT.by/obs.by.trial$n.coho
obs.by.trial$T.by.per.fish<-obs.by.trial$T.by/obs.by.trial$n.coho
obs.by.trial$AT.on.per.fish<-obs.by.trial$AT.on/obs.by.trial$n.coho
obs.by.trial$escapes.per.fish<-obs.by.trial$escapes/obs.by.trial$n.coho

#-------------------------------------------------------------------------------------------------------------------
# 3) Write dataframe to csv
#-------------------------------------------------------------------------------------------------------------------

obs.by.trial[which(is.na(obs.by.trial)==TRUE, arr.ind=TRUE)]<-0
# write.csv(obs.by.trial, file="Study1_observations_by_trial.csv")

