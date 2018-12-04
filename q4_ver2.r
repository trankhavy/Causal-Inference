foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

df <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
# Process treatment and outcome input as binary indicator 
df["untype"] <- ifelse(foo$untype=="None",0,1)
df["pbs2l"] <- ifelse(foo$pbs2l=="Failure",0,1)
df["pbs5l"] <- ifelse(foo$pbs5l=="Failure",0,1) 
df <- df[complete.cases(df),]

# Check for NA
which(is.na(df))
attach(df)

# Logistic regression
lr_pbs2l <- glm(pbs2l ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+untype,family="binomial",data=df)
effect_pbs2l = mean(predict(lr_pbs2l,df[which(df$untype==1),],type="response")) - mean(predict(lr_pbs2l,df[which(df$untype==0),],type="response"))
lr_pbs5l <- glm(pbs5l ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade+untype,family="binomial",data=df)
effect_pbs5l = mean(predict(lr_pbs5l,df[which(df$untype==1),],type="response")) - mean(predict(lr_pbs5l,df[which(df$untype==0),],type="response"))
# Check balance 
mb <- MatchBalance(untype ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade,data=df,nboots=500)

# Propensity score matching
library(Matching)

# Lenient peace building process 2 years
set.seed(123)
pscore <- glm(untype ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+decade,family="binomial")
mout_pscore2 <- Match(Y=pbs2l,Tr=untype,X=pscore$fitted.values,estimand="ATT",BiasAdjust=TRUE)
summary(mout_pscore2,full=TRUE)
mb_pscore2 <- MatchBalance(untype ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+
                          decade,data=df,match.out=mout_pscore2,nboots=500)

# Lenient peace building process 5 years
set.seed(123)
mout_pscore5 <- Match(Y=pbs5l,Tr=untype,X=pscore$fitted.values,estimand="ATT",BiasAdjust=TRUE)
summary(mout_pscore5,full=TRUE)
mb_pscore5 <- MatchBalance(untype ~ wartype+logcost+wardur+factnum+factnum2+trnsfcap+treaty+develop+exp+
                           decade,data=df,match.out=mout_pscore5,nboots=500)

# Genetic Matching
# pbs2l
set.seed(123)
X = cbind(df[,c(11:16, 18:21)])
genout2 <- GenMatch(Tr=untype,X=X,pop.size=300,max.generations=150,wait.generations=25, estimand = "ATT")
mout_genmatch2 <- Match(Y=pbs2l,Tr=untype,X=X,Weight.matrix = genout2, estimand = "ATT",BiasAdjust=TRUE)
summary(mout_genmatch2,full=TRUE)

mb2  <- MatchBalance(untype ~  wartype + logcost + wardur + factnum + 
                    factnum2 + trnsfcap + treaty + develop + exp + decade, 
                    data=df, match.out = mout_genmatch2, nboots=500)

# pbs5l
set.seed(123)
X = cbind(df[,c(11:16, 18:21)])
genout5 <- GenMatch(Tr=untype,X=X,pop.size=300,max.generations=150,wait.generations=25, estimand = "ATT")
mout_genmatch5 <- Match(Y=pbs5l,Tr=untype,X=X,Weight.matrix = genout5, estimand = "ATT",BiasAdjust = TRUE)
summary(mout_genmatch5,full=TRUE)

mb5  <- MatchBalance(untype ~  wartype + logcost + wardur + factnum + 
                     factnum2 + trnsfcap + treaty + develop + exp + decade, 
                     data=df, match.out = mout_genmatch5, nboots=500)
