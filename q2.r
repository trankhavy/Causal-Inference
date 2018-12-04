# CONSIDER USING THE JUPYTER NOTEBOOK WITH R-SERVER KERNEL (NEVER R-SAGE KERNEL)
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo)
attach(foo)
### Outcome is "pbs2s3": "democracy" and "peace" within 2 years after the end of the war
### codebook is here: http://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf

### Treatment indicator is "untype4": "multidimensional peacekeeping/peacebuilding"

### How many treated units? How many controls? How do you feel about SUTVA?
fit <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap+develop
           +exp+decade+treaty+untype4, family="binomial")
interaction_term = wardur*untype4
fit_interaction <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap+develop
                            +exp+decade+treaty+untype4+interaction_term, family="binomial")
effect = rep(0,64)
effect_interaction = rep(0,64)
i = 0
for (duration in seq(from=0,to=315,by=5)){
  i = i + 1
  treat = data.frame(wartype=mean(wartype),logcost=mean(logcost),wardur=duration,factnum=mean(factnum),
                       factnum2=mean(factnum2),trnsfcap=mean(trnsfcap),develop=mean(develop),exp=mean(exp),
                       decade=mean(decade),treaty=mean(treaty),untype4=1)
  control = data.frame(wartype=mean(wartype),logcost=mean(logcost),wardur=duration,factnum=mean(factnum),
                       factnum2=mean(factnum2),trnsfcap=mean(trnsfcap),develop=mean(develop),exp=mean(exp),
                       decade=mean(decade),treaty=mean(treaty),untype4=0)
  treat_interaction = data.frame(wartype=mean(wartype),logcost=mean(logcost),wardur=duration,factnum=mean(factnum),
                                 factnum2=mean(factnum2),trnsfcap=mean(trnsfcap),develop=mean(develop),exp=mean(exp),
                                 decade=mean(decade),treaty=mean(treaty),untype4=1,interaction_term=duration)
  control_interaction = data.frame(wartype=mean(wartype),logcost=mean(logcost),wardur=duration,factnum=mean(factnum),
                                   factnum2=mean(factnum2),trnsfcap=mean(trnsfcap),develop=mean(develop),exp=mean(exp),
                                   decade=mean(decade),treaty=mean(treaty),untype4=0,interaction_term=0)

  effect[i] = predict(fit,treat,type="response") - predict(fit,control,type="response")
  effect_interaction[i] = predict(fit_interaction,treat_interaction,type="response") - predict(fit_interaction,control_interaction,type="response")
}

# Plot out result
xticks = seq(0,315,5)
yticks = seq(0,0.8,0.1)
par(lty="dotted")
effect_plot = plot(xticks,effect,xlim=c(5,315),ylim=c(0,0.8),type='l',xaxt="n",yaxt="n",xaxs="i",yaxs="i",
                   xlab = "Duration of war in months",ylab="Marginal effects of UN peacebuilding operations",
                   cex.lab=1)
par(lty="solid")
lines(xticks,effect_interaction,xaxt="n",yaxt="n",xaxs="i",yaxs="i")
axis(side=1, at=xticks,labels=xticks,las=1)
axis(side=2,at=yticks,labels=yticks,las=1)
axis(side=3,at=xticks,labels=FALSE)
axis(side=4,at=yticks,label=FALSE)
text(155,0.5,"original model")
text(90,0.15,"model with interaction term")