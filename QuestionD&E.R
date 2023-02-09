
# Authors : Giovanni Scognamiglio and Martina Trigilia
# Course  : Statistics For Data Science
# Teacher : Salvatore Ruggieri 

#lib and functions
library(caret)
library(dplyr)
library(car)
library(MASS)
library(multcomp)
library(ROCR)
library(DescTools)
library(glmnet)
library(ROSE)
library(arm)
library(splitstackshape)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(arm)
library(pROC)
library(ggstatsplot)

#rm(list=ls())

par(mar=c(4,4,1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(resetPar())


load('aidaFull.clean2.RData')
aida = aida_full
#rm(aida_full)
gc()

###################
# DATA PREPARATION
###################

###cleaning Strategy 1
cols.Last.avail = sapply(colnames(aida), function(x) grepl('Last.avail',x, fixed=T))
cols.Year1 = sapply(colnames(aida), function(x) grepl('Year...1',x, fixed=T))
cols.Year2 = sapply(colnames(aida), function(x) grepl('Year...2',x, fixed=T))
aida.temp.Last = aida[,cols.Last.avail]
aida.temp.year1 = aida[,cols.Year1]
aida.temp.year2 = aida[,cols.Year2]
colnames(aida.temp.Last)
colnames(aida.temp.year1)
colnames(aida.temp.year2)
#View(aida.temp.Last)
#View(aida.temp.year1)231
#new_cols = ((aida.temp.Last - aida.temp.year1)/aida.temp.year1)
new_cols = (aida.temp.Last - aida.temp.year1)
#new_cols <- do.call(data.frame, lapply(new_cols,function(x) replace(x, is.infinite(x), 1)))
#View(new_cols)
group = names(new_cols)
gsub("Last.avail", ".Avg.trend",group)
names(new_cols) = gsub("Last.avail", ".Avg.trend",names(new_cols))
names(new_cols)
aida.temp = cbind(aida[,!cols.Year1 & !cols.Year2], new_cols)
aida.temp
#aida.temp = na.omit(aida.temp)
aida = aida.temp
sum(is.na(aida))
todrop <- c("ATECO.2007code","Total.assetsth.EURLast.avail..yr","Company.name","File","Legal.status","Province","Tax.code.number","Incorporation.year",
            "Registered.office.address...Commune.ISTAT.code","Registered.office.address...Region")
aida = aida[,!(names(aida) %in% todrop)]
colnames(aida)
rm(todrop,aida.temp,aida.temp.Last,aida.temp.year1,aida.temp.year2, new_cols)
gc()
rm(aida_full)
years_selected <- c(2010:2017)
aida = aida[aida$Last.accounting.closing.date %in% years_selected,]

todrop <- c()
# drop columns with too much missing values
for (x in colnames(aida)){
  
  mv <- sum(is.na(aida[x]))
  if (mv >= 200000) 
    todrop <- append(todrop,x)
}
todrop
aida.temp = aida[,!(names(aida) %in% todrop)]
aida.temp <- na.omit(aida.temp) 
aida.temp
nrow(aida.temp[aida.temp$Last.accounting.closing.date %in% c(2010:2016),])
nrow(aida.temp[aida.temp$Last.accounting.closing.date %in% c(2017),])

aida = aida.temp ; 
rm(aida.temp)
sum(is.na(aida))
gc()
####################################


###################
# FEATURE SELECTION
###################

######feature selection: dropping variables indepent from target
ind_vars_to_drop = c()
for (n in names(aida)) {
  temp_var = aida[,n]
  if (is.numeric(temp_var)) temp_var = ntile(temp_var,4)
  U = table(temp_var, aida$Failed)
  ass.res = GTest(U) 
  if (ass.res$p.value > 0.05/length(names(aida)))  ind_vars_to_drop=append(ind_vars_to_drop, n)
}
ind_vars_to_drop
# one independent var found: Debt.EBITDA.ratio..Avg.trend..yr
aida = aida[,!names(aida)=='Debt.EBITDA.ratio..Avg.trend..yr']
rm(ass.res,ind_vars_to_drop)
##

######VIF feature selection
# multi-collinearity --> two or more predictor variables are highly correlated to each other
# so that they do not provide independent information in the model
gc()
aida_vif = subset(aida, select = -c(Last.accounting.closing.date) )
worst_vif_vars = character()
#repeat bottom code for repeating VIF selection one by one
aida_vif = aida_vif[,!names(aida_vif)%in%worst_vif_vars]
model <- glm(Failed ~ ., data = aida_vif, family = binomial(link = "logit"))
vif_values <- vif(model)
vif_values <- vif_values[,"GVIF^(1/(2*Df))"]
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", col = "steelblue",las=2, ylim=c(0,10))
abline(h = 5, lwd = 1.5, lty = 2)
names(sort(vif_values))
worst_vif = names(sort(vif_values, decreasing=T)[1])
worst_vif
worst_vif_vars = append(worst_vif_vars, worst_vif)
worst_vif_vars

#removing worst vif vars from aida
#-> only one var found to remove: "Net.financial.positionth.EURLast.avail..yr"
aida = aida[, !names(aida)=='Net.financial.positionth.EURLast.avail..yr']

#checking corr just for safety but we're not dropping anything else as VIF is now low everywhere
aida_corr = subset(aida, select = -c(Failed,Legal.form,ATECO.NAME,Location))
View(aida_corr)
mat_cor <- cor(aida_corr)
mat_cor[abs(mat_cor) < 0.8] = NA
View(mat_cor)
rm(aida_vif, model, mat_cor, aida_corr)
##

#####STEP AIC feature selection
aida_aic = subset(aida, select = -c(Last.accounting.closing.date) )
aida_aic = sample_n(aida_aic, 10000)
fit <- glm(Failed ~ ., data = aida_aic, family = binomial(link = "logit"))
summary(fit)
#variable selection: stepwise regression 
step = stepAIC(fit, direction="backward")
summary(step)
rm(aida_aic, step, fit)
##-> no variable found to drop with step AIC


###################
# Binary probabilistic classifiers (Regression, Penalized regression, and RandomForest)
###################

#correct names
names(aida)[names(aida) == 'Location.Nord-est'] <- 'Location.Nord.est'
names(aida)[names(aida) == 'Location.Nord-ovest'] <- 'Location.Nord.ovest'
names(aida)[names(aida) == 'Legal.form.S.R.L. one-person'] <- 'Legal.form.S.R.L.one.person'
names(aida)[names(aida) == 'Legal.form.S.R.L. simplified'] <- 'Legal.form.S.R.L.simplified'


#create train and validation set
aida.train = aida[!aida$Last.accounting.closing.date %in% c(2010,2011,2015,2016,2017),]
aida.train = aida.train = subset(aida.train, select = -c(Last.accounting.closing.date) )
aida.val = aida[aida$Last.accounting.closing.date == 2015,]
aida.val = subset(aida.val, select = -c(Last.accounting.closing.date) )
data = list(train=aida.train, val=aida.val)
rm(aida.train, aida.val)

###data stability - covariate shift
table(data$train$Failed)[2]/length(data$train$Failed)
table(data$val$Failed)[2]/length(data$val$Failed)
#random subsampling of validation set
num_of_active_to_drop = 14500
temp_index = rownames(data$train[data$train$Failed==1,])
sample_index = sample(temp_index, num_of_active_to_drop)
temp_data.train = data$train[!rownames(data$train) %in% sample_index,]
#compare new data stability with val
table(temp_data.train$Failed)
table(temp_data.train$Failed)[1]/table(temp_data.train$Failed)[2]
table(data$val$Failed)[1]/table(data$val$Failed)[2]
#update data$train
data$train = temp_data.train
nrow(data$train)
rm(temp_data.val)

#dummy encoding
mt = dummyVars(~ Location + Legal.form + ATECO.NAME, data = data$train)
mv = dummyVars(~ Location + Legal.form + ATECO.NAME, data = data$val)
dt = predict(mt, data$train)
dv = predict(mv, data$val)
data.encoded = data
data.encoded$train = data.encoded$train[!names(data.encoded$train)%in%c('Location', 'Legal.form', 'ATECO.NAME')]
data.encoded$val = data.encoded$val[!names(data.encoded$val)%in%c('Location', 'Legal.form', 'ATECO.NAME')]
data.encoded$train = cbind(data.encoded$train, dt)
data.encoded$val = cbind(data.encoded$val, dv)

rm(aida_vif, dt,dv, model, mt, mv, o)

########
###Regression
#######
fit <- glm(Failed ~ ., data = data$train, family = binomial(link = "logit"))
summary(fit)
#plot(fit)
binnedplot(fitted(fit), 
           residuals(fit, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = NULL, 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

# post-hoc test
p_h = glht(fit)
# adjusted p-value for multiple comparison
u = summary(p_h)
View(u)
u$test$coefficients
u$test$sigma
u$test$pvalues
lr_df = data.frame(feature = names(u$test$coefficients), coefficient = round(u$test$coefficients,3), sd = round(u$test$sigma,3), p.values = round(u$test$pvalues,3))
lr_df = lr_df[lr_df$p.values<0.05,]
View(lr_df)
write.csv(x = lr_df, file='lr_df5.csv')
View(lr_df)
su = summary(fit)$coefficients[,4]
insignificants = names(su[su>0.05/length(su)])
insignificants
ncol(data$train)
length(su)
length(insignificants)
# confidence intervals 
confint(p_h, level=0.95)

#insignificant vars
#"Cash.Flowth.EURLast.avail..yr"           "Debt.equity.ratio.Last.avail..yr"       
#[3] "EBITDAth.EURLast.avail..yr"              "LeverageLast.avail..yr"                 
#[5] "Net.working.capitalth.EURLast.avail..yr" "Profit..loss.th.EURLast.avail..yr"      
#[7] "Total.assetsth.EURLast.avail..yr"        "Cash.Flowth.EUR.Avg.trend..yr"          
#[9] "EBITDAth.EUR.Avg.trend..yr"              "Leverage.Avg.trend..yr"                 
#[11] "Net.working.capitalth.EUR.Avg.trend..yr" "Solvency.ratio......Avg.trend..yr" 

#caret regression
#insignificants = insignificants[!insignificants%in%c('LocationSud','Legal.formS.R.L.','Legal.formS.C.A.R.L.P.A.','Legal.formSocial cooperative company')]
#data$train = data$train[!names(data$train)%in%insignificants]
noresampling = trainControl(method="none")
lr.fit = train(Failed ~ ., data = data$train, 
               method = "glm", trControl=noresampling,
               # pass-trough options
               family=binomial(logit))
summary(lr.fit)

#-> regression without removing insignificants: auc 0.671
#-> regression removing insignificants: auc 0.670

# auc roc on validation set
lr.prob = predict(lr.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
lr.pconf = lr.prob[,2] # scores
lr.prediction = prediction(lr.pconf, data$val$Failed)
lr.roc = performance(lr.prediction, "tpr", "fpr")
#plot(lr.roc, colorize=T); abline(a=0, b= 1)
# AUC: performance returns an object whose members (called 'slots') are accessed with '@'
o = performance(lr.prediction,"auc")
o@y.values[[1]]

# built-in metrics
lr.pred = predict(lr.fit, newdata = data$val)
lr.pred
confusionMatrix(lr.pred, data$val$Failed, positive="1")
# other metrics
precision(lr.pred, data$val$Failed, relevant="1") # or PPV
recall(lr.pred, data$val$Failed, relevant="1") # or sensitivity
F_meas(lr.pred, data$val$Failed, relevant="1")
# Brier score
mean((lr.pconf - (as.numeric(data$val$Failed)-1))^2)
# calibration plot: P(Y=1|s(W) \in [a, b])
cal_data = calibration(data$val$Failed ~ lr.pconf, class="1")
plot(cal_data)

nbins=11
bins = seq(1/nbins, 1, 1/nbins)
id2bin = cut(lr.pconf, breaks=c(0,bins), labels=1:nbins)
bin.total = c(table(id2bin)) # c() to transform to vector
bin.pos = tapply(as.numeric(data$val$Failed)-1, id2bin, sum)
y = bin.pos/bin.total
x = ( c(0,bins[-nbins])+bins)/2 # midpoints
plot(x*100, y*100, type='o', xlim=c(0,100), ylim=c(0,100), 
     col="blue", xlab="Prediction Confidence", ylab="Perc. of Positives")
abline(coef = c(0,1),col="grey")
# add confidence interval
lines(cal_data$data$midpoint, cal_data$data$Lower, col="red")
lines(cal_data$data$midpoint, cal_data$data$Upper, col="red")
s_b = tapply(lr.pconf, id2bin, mean)
y_b = y
binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
binECE

# more details on predictions
featurePlot(x=data.frame(lr.pconf), y=data$val$Failed, plot='density', auto.key = list(columns = 2))
#by7 increaseing cutoff to 0.7 we reach much higher accuracy
cutoff = 0.5
lr.pred.cutoff = factor(ifelse(lr.pconf>=cutoff, 1, 0), levels=levels(data$val$Failed))
confusionMatrix(lr.pred.cutoff, data$val$Failed, positive="1")

# using ROCR for metrics at cutoff
lr.prediction = prediction(lr.pconf, data$val$Failed)
# acc at cutoff
lr.acc = performance(lr.prediction, "acc"); plot(lr.acc, ylim=c(0,1))
# tpr at cutoff
lr.tpr = performance(lr.prediction, "tpr"); plot(lr.tpr, ylim=c(0,1))
# f1 at cutoff
lr.f1 = performance(lr.prediction, "f"); plot(lr.f1, ylim=c(0,1))
# roc curve
lr.roc = performance(lr.prediction, "tpr", "fpr")
plot(lr.roc, colorize=T); abline(a=0, b= 1)
# AUC: performance returns an object whose members (called 'slots') are accessed with '@'
o = performance(lr.prediction,"auc")
o@y.values[[1]]


### cross validation sample of AUC ROC
# repeated cross validation with custom summary function
# setting repeated cross-validation
lev = c("class0", "class1")
data$train$Failed = factor(data$train$Failed); levels(data$train$Failed) = lev
data$val$Failed = factor(data$val$Failed); levels(data$val$Failed) = lev
lr.pred2 = as.factor(lr.pred); levels(lr.pred2) = lev
dat = data.frame(obs=data$val$Failed, pred=lr.pred2, 
                 class0=lr.prob[,1], class1=lr.prob[,2])
twoClassSummary(dat, lev)
# custom summary
custSummary = function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}
custSummary(dat, lev)

rcv = trainControl(method="repeatedcv", repeats=5, number=10,
                   classProbs = TRUE, summaryFunction=custSummary)
set.seed(30) # reproducibility of folds
lr2.fit = train(Failed~., data = data$train, 
                method = "glm",  trControl=rcv, metric="AUC", # custom metric
                # pass-trough options
                family=binomial(logit))

lr2.fit
lr2.fit$resample # details over 5x10 folds
lr2.folds = lr2.fit$resample[['AUC']]
mean(lr2.folds) # reported is mean performance
t.test(lr2.folds) # accuracy confidence interval

##################

#############
# Penalized regression
############

cv.elastic <- cv.glmnet(model.matrix(Failed~., data$train)[,-1], data$train$Failed, alpha = 1, family = "binomial", type.measure = 'auc') 
coef(cv.elastic)
cv.elastic$lambda.min 
cv.elastic$lambda.1se


elastic <- glmnet(model.matrix(Failed~., data$train)[,-1], data$train$Failed, alpha = 1, lambda=0.003, family = "binomial", type.measure = 'auc') 
elastic
ma = coef(elastic)
summary(elastic)
ma
summary(pr.fit)
length(ma[,1])
length(names(ma[,1][ma[,1]==0]))
pushed_to_zero.lasso = names(ma[,1][ma[,1]==0]) 
#23 vars got pushed to zero by lasso regularization
#[1] "Cash.Flowth.EURLast.avail..yr"           "Debt.equity.ratio.Last.avail..yr"       
#[3] "Legal.formS.R.L."                        "Legal.formSocial cooperative company"   
#[5] "LeverageLast.avail..yr"                  "Net.working.capitalth.EURLast.avail..yr"
#[7] "Number.of.employeesLast.avail..yr"       "Total.assetsth.EURLast.avail..yr"       
#[9] "ATECO.NAMEB - Miniere"                   "ATECO.NAMED - Energia"                  
#[11] "ATECO.NAMEE - Fornitura Acqua"           "ATECO.NAMEF - Costruzioni"              
#[13] "ATECO.NAMEH - Trasporti"                 "ATECO.NAMEP - Istruzione"               
#[15] "ATECO.NAMEQ - Sanità"                    "ATECO.NAMES - Altri Servizi"            
#[17] "Cash.Flowth.EUR.Avg.trend..yr"           "Current.ratio.Avg.trend..yr"            
#[19] "EBITDAth.EUR.Avg.trend..yr"              "Leverage.Avg.trend..yr"                 
#[21] "Number.of.employees.Avg.trend..yr"       "Profit..loss.th.EUR.Avg.trend..yr"      
#[23] "Solvency.ratio......Avg.trend..yr"

#comparing pushed_to_zero.lasso with the insignificants in linear regression
ins_vs_lasso = sapply(insignificants, function(x) x%in%pushed_to_zero.lasso)
length(ins_vs_lasso[ins_vs_lasso==T])
length(ins_vs_lasso)
#-> most vars insignificant in linear reg have been pushed to zero by the lasso reg.

elastic <- glmnet(model.matrix(Failed~., data$train)[,-1], data$train$Failed, alpha = 1, lambda=0.0001243635, family = "binomial", type.measure = 'auc') 
coef(elastic)

cv.elastic.lasso <- cv.glmnet(model.matrix(Failed~., data$train)[,-1], data$train$Failed, alpha = 1, family = "binomial", type.measure = 'auc') 
coef(cv.elastic.lasso)


# largest value of lambda such that error is within 1 standard error of the minimum
# best lambda
lambda.min <- cv.elastic.lasso$lambda.min 
lambda.min

# value of lambda that gives minimum mean cross-validated error
# simplest model
lambda.1se <- cv.elastic.lasso$lambda.1se 
lambda.1se
# fit the model with LAMBDA.1SE

#-> regression without removing insignificants: auc 0.671
#-> regression removing insignificants: auc 0.670


#repeated cross-validation
sample.train = sample_n(data.cat$train, 100000)
lasso_cv= trainControl(method="cv", 
                       number=5, 
                       classProbs = TRUE,
                       summaryFunction=custSummary)
set.seed(30)
lasso.fit = train(Failed~., data = data.cat$train, 
                  method = "glmnet", 
                  trControl=lasso_cv, 
                  metric="AUC", tuneGrid = expand.grid(alpha=1 ,lambda=0.0015))
lasso.fit = train(Failed~., data = data$train, 
                   method = "glmnet", 
                   trControl=noresampling, 
                   metric="AUC", tuneGrid = expand.grid(alpha=1 ,lambda=0.003))
pr.fit = lasso.fit
lassot.fit
lasso.F <- lassot.fit

#### The final values used for the model were alpha = 1 and lambda = 0.003
#auc obtained = 0.6712414

summary(lasso.F)
coef(lasso.F)


# Display regression coefficients
coef(lassot.fit)
lasso.resample <-lassot.fit$resample 
lasso.folds <- lasso.F$resample[['AUC']]
mean(lasso.folds) # mean AUC

# PREDICT ON VAL SET
lasso.pred = predict(lasso.fit, newdata = data$val)
lasso.prob = predict(lasso.fit, newdata = data$val, type="prob")
lasso.pconf = lasso.prob[,2]

# confusion matrix
confusionMatrix(lasso.pred, data$val$Failed, positive="Failed")

# calibration plot on test set
lasso.cal_data = calibration(data$val$Failed ~ lasso.pconf, class="1")

# calibration plot on test test
plot(lasso.cal_data$data$midpoint, main = "Calibration Plot on val set", lasso.cal_data$data$Percent, col="blue", type="o", xlab="Bin Midpoint", ylab="Observed Event Percentage", ylim=c(0,100))
abline(coef = c(0,1),col="black", lwd=1, lty=2)
nbins=11
bins = seq(1/nbins, 1, 1/nbins)
id2bin = cut(lasso.pconf, breaks=c(0,bins), labels=1:nbins)
bin.total = c(table(id2bin)) # c() to transform to vector
bin.pos = tapply(as.numeric(data$val$Failed)-1, id2bin, sum)
y = bin.pos/bin.total
x = ( c(0,bins[-nbins])+bins)/2 # midpoints
plot(x*100, y*100, type='o', xlim=c(0,100), ylim=c(0,100), 
     col="blue", xlab="Prediction Confidence", ylab="Perc. of Positives")
abline(coef = c(0,1),col="grey")
# add confidence interval
lines(lasso.cal_data$data$midpoint, lasso.cal_data$data$Lower, col="red")
lines(lasso.cal_data$data$midpoint, lasso.cal_data$data$Upper, col="red")
s_b = tapply(lasso.pconf, id2bin, mean)
y_b = y
binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
binECE


# confidence interval on test
lines(lasso.cal_data$data$midpoint, lasso.cal_data$data$Lower, col="red")
lines(lasso.cal_data$data$midpoint, lasso.cal_data$data$Upper, col="red")

legend("topleft",inset = 0.001,legend=c("perfect calibration", "conf interval", "test set"),
       col=c("black", "red", "green"), lty=c(2,1,1), cex=0.8, box.lty=0)

# ROCR

lasso.prediction = prediction(lasso.pconf, data$val$Failed)

# acc at cutoff
lasso.acc = performance(lasso.prediction, "acc"); plot(lasso.acc)
# tpr at cutoff
lasso.tpr = performance(lasso.prediction, "tpr"); plot(lasso.tpr)
# f1 at cutoff
lasso.f1 = performance(lasso.prediction, "f"); plot(lasso.f1)

# ROC Curve
lasso.roc = performance(lasso.prediction, "tpr", "fpr")

plot(lasso.roc, main= "ROC Curve - Test Set", colorize = T); abline(a=0, b= 1) # random classifier
plot(lr.roc, add=T)
# AUC
o_test = performance(lasso.prediction,"auc")
o_test@y.values[[1]]
 
#########
##parametric model scoring performance on validation set
auc.lr = performance(lr.prediction,"auc")@y.values[[1]]
auc.lr
auc.pr = performance(lasso.prediction,"auc")@y.values[[1]]
auc.pr
cal_data.lr = calibration(data$val$Failed ~ lr.pconf, class="1")
cal_data.pr = calibration(data$val$Failed ~ lasso.pconf, class="1")
ggplot(cal_data.pr, bwidth = 2, dwidth = 3) 
ggplot(cal_data.lr, bwidth = 2, dwidth = 3)

calPlotData <- calibration(data$val$Failed ~ lr.pconf + lasso.pconf, class="1")
L.R = lr.pconf ;Penalized.L.R = lasso.pconf
calPlotData <- calibration(data$val$Failed ~ L.R + Penalized.L.R, class="1")
plot(calPlotData)
ggplot(calPlotData)

roc = roc(data$val$Failed  ~ L.R + Penalized.L.R)
ggroc(roc, size=0.5)

ggplot(calPlotData) + ggroc(roc)
plot2 <- ggplot(calPlotData) + theme(legend.position="bottom") + ggtitle("Calibration plot")
plot1 <- ggroc(roc) + theme(legend.position="bottom") + ggtitle("Roc Curve")
plot_grid(plot1, plot2, labels = "AUTO")

#################
# NON PARAMETRIC MODELS
################

###RandomForest

#gridsearch for best param
getModelInfo('rf')
auc_res = c() 
for (n in seq(from=2, to=5, by=1)) {
  tunegrid = expand.grid(.mtry=n) 
  noresampling = trainControl(method="none")
  rf.fit = train(Failed~., data = data$train,  
                 method = "rf", trControl=noresampling, metric="AUC", tuneGrid=tunegrid,
                 # pass-trough options
                 ntree=50)
  rf.pconf = predict(rf.fit, newdata = data$val, type="prob")[,2] # as predict_proba in scikit-learn
  rf.prediction = prediction(rf.pconf, data$val$Failed)
  o = performance(rf.prediction,"auc")
  print(o@y.values[[1]])
  auc_res = append(auc_res, o@y.values[[1]])
}
#-> best param is .mtry = 3

#single hold out test on validation set
tunegrid = expand.grid(.mtry=3) 
noresampling = trainControl(method="none")
rf.fit = train(Failed~., data = data$train,  
               method = "rf", trControl=noresampling, metric="AUC", tuneGrid=tunegrid,
               # pass-trough options
               ntree=50)
rf.prob = predict(rf.fit, newdata = data$val, type="prob") 
rf.pconf = rf.prob[,2] # as predict_proba in scikit-learn
rf.prediction = prediction(rf.pconf, data$val$Failed)
rf.roc = performance(rf.prediction,"tpr","fpr")
plot(rf.roc, add=T, col="red")
o = performance(rf.prediction,"auc")
print(o@y.values[[1]])

plot(density(rf.pconf))


# built-in metrics
rf.pred = predict(rf.fit, newdata = data$val)
rf.pred
confusionMatrix(rf.pred, data$val$Failed, positive="1")
# other metrics
precision(rf.pred, data$val$Failed, relevant="1") # or PPV
recall(rf.pred, data$val$Failed, relevant="1") # or sensitivity
F_meas(rf.pred, data$val$Failed, relevant="1")
# Brier score
mean((rf.pconf - (as.numeric(data$val$Failed)-1))^2)
# calibration plot: P(Y=1|s(W) \in [a, b])
cal_data = calibration(data$val$Failed ~ rf.pconf, class="1")
plot(cal_data)
s_b = tapply(rf.pconf, id2bin, mean)
y_b = y
binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
binECE
rf.binECE = binECE

nbins=11
bins = seq(1/nbins, 1, 1/nbins)
id2bin = cut(rf.pconf, breaks=c(0,bins), labels=1:nbins)
bin.total = c(table(id2bin)) # c() to transform to vector
bin.pos = tapply(as.numeric(data$val$Failed)-1, id2bin, sum)
y = bin.pos/bin.total
s_b = tapply(rf.pconf, id2bin, mean)
y_b = y
binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
binECE

# more details on predictions
featurePlot(x=data.frame(rf.pconf), y=data$val$Failed, plot='density', auto.key = list(columns = 2))

# cutoff analysis
cutoff = 0.80
rf.pred.cutoff = factor(ifelse(rf.pconf>=cutoff, 1, 0), levels=levels(data$val$Failed))
confusionMatrix(rf.pred.cutoff, data$val$Failed, positive="1")
# using ROCR for metrics at cutoff
rf.prediction = prediction(rf.pconf, data$val$Failed)
# acc at cutoff
rf.acc = performance(rf.prediction, "acc"); plot(rf.acc, ylim=c(0,1))
# tpr at cutoff
rf.tpr = performance(rf.prediction, "tpr"); plot(rf.tpr, ylim=c(0,1))
# f1 at cutoff
rf.f1 = performance(rf.prediction, "f"); plot(rf.f1, ylim=c(0,1))
# roc curve
rf.roc = performance(rf.prediction, "tpr", "fpr")
plot(rf.roc, colorize=T); abline(a=0, b= 1)
# AUC: performance returns an object whose members (called 'slots') are accessed with '@'
o = performance(rf.prediction,"auc")
o@y.values[[1]]


### cross validation sample of AUC ROC
lev = c("class0", "class1")
data.cat = data
data.cat$train$Failed = factor(data$train$Failed); levels(data.cat$train$Failed) = lev
data.cat$val$Failed = factor(data$val$Failed); levels(data.cat$val$Failed) = lev
rf.pred2 = as.factor(rf.pred); levels(rf.pred2) = lev
dat = data.frame(obs=data$val$Failed, pred=rf.pred2, 
                 class0=rf.prob[,1], class1=rf.prob[,2])
twoClassSummary(dat, lev)
# custom summary
custSummary = function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}
custSummary(dat, lev)

rcv = trainControl(method="repeatedcv", repeats=5, number=10,
                   classProbs = TRUE, summaryFunction=custSummary)
tunegrid = expand.grid(.mtry=3) # fixed value, no grid search
set.seed(30) # same folds as lf2.fit
rf.fit = train(Failed~., data = data$train,  
               method = "rf", trControl=rcv, metric="AUC", tuneGrid=tunegrid,
               # pass-trough options
               ntree=50)
rf.fit
rf.fit$resample # details over 5x10 folds
rf.folds = rf.fit$resample[['AUC']]
mean(rf.folds)
t.test(rf.folds) # confidence interval

################
# Adaboost
###############
lev = c("class0", "class1")
data.cat = data
data.cat$train$Failed = factor(data$train$Failed); levels(data.cat$train$Failed) = lev
data.cat$val$Failed = factor(data$val$Failed); levels(data.cat$val$Failed) = lev

#single hold out test on validation set
names(data.cat$train) == names(data.cat$val)
data.ada = data
data.ada$train = data.ada$train[,!names(data.ada$train) %in% c('Location','ATECO.NAME','Legal.form')]
auc_res4 = c()
for (n in c(10,50,100)) {
  tunegrid = expand.grid(iter = n, maxdepth=4, nu=1) 
  noresampling = trainControl(method="none")
  ada.fit = train(Failed~., data = data.ada$train,  
                 method = "ada", trControl=noresampling,tuneGrid=tunegrid, metric="AUC")
  #summary(ada.fit)
  ada.prob = predict(ada.fit, newdata = data.ada$val, type="prob") 
  ada.pconf = ada.prob[,2] # as predict_proba in scikit-learn
  summary(ada.pconf)
  ada.prediction = prediction(ada.pconf, data.ada$val$Failed)
  ada.roc = performance(ada.prediction,"tpr","fpr")
  plot(ada.roc, col="red")
  o = performance(ada.prediction,"auc")
  print(o@y.values[[1]])
  auc_res4 = append(auc_res4, o@y.values[[1]])
}
#-> best param found: 200 trees with max_depth=4
train_temp = data$train
train_temp$Location = c()

data.ada = data
data.ada$train = data$train[,!names(data$train) %in% c('Location','ATECO.NAME','Legal.form')]
tunegrid = expand.grid(iter = 150, maxdepth=4, nu=1) 
noresampling = trainControl(method="none")
ada.fit = train(Failed~. , data =  data.ada$train,  
               method = "ada", trControl=noresampling,tuneGrid=tunegrid, metric="AUC")
#summary(ada.fit)
ada.prob = predict(ada.fit, newdata = data.ada$val, type="prob") 
ada.pconf = ada.prob[,2] # as predict_proba in scikit-learn
summary(ada.pconf)
ada.prediction = prediction(ada.pconf, data$val$Failed)
ada.roc = performance(ada.prediction,"tpr","fpr")
plot(ada.roc, col="red")
o = performance(ada.prediction,"auc")
print(o@y.values[[1]])


plot(density(ada.pconf))

data.cat$train[,!names(data$train) =='Failed']

# built-in metrics
ada.pred = predict(ada.fit, newdata = data$val)
ada.pred
nbins=11
bins = seq(1/nbins, 1, 1/nbins)
id2bin = cut(ada.pconf, breaks=c(0,bins), labels=1:nbins)
bin.total = c(table(id2bin)) # c() to transform to vector
bin.pos = tapply(as.numeric(data$val$Failed)-1, id2bin, sum)
y = bin.pos/bin.total
s_b = tapply(ada.pconf, id2bin, mean)
y_b = y
binECE = sum(bin.total*abs(y_b-s_b))/sum(bin.total)
binECE

# more details on predictions
featurePlot(x=data.frame(ada.pconf), y=data$val$Failed, plot='density', auto.key = list(columns = 2))

# cutoff analysis
cutoff = 0.80
ada.pred.cutoff = factor(ifelse(ada.pconf>=cutoff, 1, 0), levels=levels(data$val$Failed))
confusionMatrix(ada.pred.cutoff, data$val$Failed, positive="1")
# using ROCR for metrics at cutoff
ada.prediction = prediction(ada.pconf, data$val$Failed)
# acc at cutoff
ada.acc = penbormance(ada.prediction, "acc"); plot(ada.acc, ylim=c(0,1))
# tpr at cutoff
ada.tpr = penbormance(ada.prediction, "tpr"); plot(ada.tpr, ylim=c(0,1))
# f1 at cutoff
ada.f1 = penbormance(ada.prediction, "f"); plot(ada.f1, ylim=c(0,1))
# roc curve
ada.roc = penbormance(ada.prediction, "tpr", "fpr")
plot(ada.roc, colorize=T); abline(a=0, b= 1)
plot(rf.roc, add=T); 
# AUC: penbormance returns an object whose members (called 'slots') are accessed with '@'
o = performance(ada.prediction,"auc")
o@y.values[[1]]


### cross validation sample of AUC ROC
lev = c("class0", "class1")
data$train$Failed = factor(data$train$Failed); levels(data$train$Failed) = lev
data$val$Failed = factor(data$val$Failed); levels(data$val$Failed) = lev
ada.pred2 = as.factor(ada.pred); levels(ada.pred2) = lev
dat = data.frame(obs=data$val$Failed, pred=ada.pred2, 
                 class0=ada.prob[,1], class1=ada.prob[,2])
twoClassSummary(dat, lev)
# custom summary
custSummary = function(data, lev=NULL, model=NULL) {
  prediction = prediction(data$class1, data$obs)
  o = performance(prediction,"auc")
  auc = o@y.values[[1]]
  c(AUC = auc)
}
custSummary(dat, lev)

rcv = trainControl(method="repeatedcv", repeats=5, number=10,
                   classProbs = TRUE, summaryFunction=custSummary)
tunegrid = expand.grid(iter = 150, maxdepth=4, nu=1) 
set.seed(30) # same folds as lf2.fit
ada.fit = train(Failed~., data = data$train,  
               method = "ada", trControl=rcv, metric="AUC", tuneGrid=tunegrid)
ada.fit
ada.fit$resample # details over 5x10 folds
ada.folds = ada.fit$resample[['AUC']]
mean(ada.folds)
t.test(ada.folds) # confidence interval
# which is better between lr and ada?

#########
##NON-parametric model scoring performance on validation set
auc.rf = performance(rf.prediction,"auc")@y.values[[1]]
auc.rf
auc.ada = performance(ada.prediction,"auc")@y.values[[1]]
auc.ada
cal_data.rf = calibration(data$val$Failed ~ rf.pconf, class="1")
cal_data.ada = calibration(data$val$Failed ~ ada.pconf, class="1")
ggplot(cal_data.rf, bwidth = 2, dwidth = 3) 
ggplot(cal_data.ada, bwidth = 2, dwidth = 3)

calPlotData <- calibration(data$val$Failed ~ rf.pconf + ada.pconf, class="1")
R.F = rf.pconf ;ADA = ada.pconf
calPlotData <- calibration(data$val$Failed ~ R.F + ADA, class="1")
plot(calPlotData)
ggplot(calPlotData)

roc = roc(data$val$Failed  ~ R.F + ADA)
ggroc(roc, size=0.5)

ggplot(calPlotData) + ggroc(roc)
plot2 <- ggplot(calPlotData) + theme(legend.position="bottom") + ggtitle("Calibration plot")
plot1 <- ggroc(roc) + theme(legend.position="bottom") + ggtitle("Roc Curve")
plot_grid(plot1, plot2, labels = "AUTO")


#########################
# RATING MODEL
###########

threshold <- c("B"= 0.10, "C"= 0.20, "D" = 0.30,
               "E" = 0.40, "F"=0.50,"G" = 0.60, "H"= 0.80,
               "I" = 0.9, "L" = 1)

rating_model <- function(score) {
  
  #if (score <= 0.05) 
   # return ("A")
  
  if (score <= 0.10 && score > 0.05)
    return ("B")
  
  if (score <= 0.20 && score > 0.10)
    return ("C")
  
  if (score <= 0.30 && score > 0.20)
    return("D")
  
  if (score <= 0.40 && score > 0.30)
    return("E")
  
  if (score <= 0.50 && score > 0.40)
    return("F")
  
  if (score <= 0.60 && score > 0.50)
    return("G")
  
  if (score <= 0.80 && score > 0.60)
    return("H")
  
  if (score <= 0.90 && score > 0.80)
    return("I")
  
  else 
    return ("L")
}
#temp_cat = 'A'
bin.test <- function(temp_cat) {
  
  x <- nrow(dat_rating[dat_rating$class == temp_cat & dat_rating$Failed == 1,])  
  n <- nrow(dat_rating[dat_rating$class == temp_cat ,]) 
  print(n)
  print(x/n)
  p <- threshold[[temp_cat]]
  print(p)
  binom.test(x, n, p, alternative = "greater", conf.level = bonf_alfa.class)
}

#select classifier (lr.fit, pr.fit, rf.fit, ada.fit)
temp_fit = lr.fit

pconf = predict(temp_fit, newdata = data$val, type="prob")[,2]
lr_var = pconf

rating_class <- sapply(lr_var, rating_model) 
table(rating_class)

dat_rating = data.frame(Failed = data$val$Failed, score = lr_var,class = rating_class)
dat_rating$Failed

m_class <- length(threshold)
bonf_alfa.class <- 1-(0.05/m_class)

bin.test_rating.list <- sapply(names(threshold), bin.test) 
#see results:
bin.test_rating.list
threshold

class(bin.test_rating.list)
data_temp=data.frame(bin.test_rating.list)
View(data_temp)
save(data_temp,file='lr_reg_results.rdata')
load('lr_reg_resultstest.rdata')
lr_reg_resultstest

#extra stuff
pvalues_alt_less = bin.test_rating.list['p.value',]
pvalues_alt_less = as.numeric(pvalues_alt_less)
list_model_pvalues <- append(list_model_pvalues, list(1-as.numeric(pvalues_alt_less)))

list_model_pvalues

View(dat_rating[dat_rating$class=='E',])
summary(dat_rating$score[dat_rating$class=='A'])
summary(dat_rating$score[dat_rating$class=='B'])
summary(dat_rating$score[dat_rating$class=='C'])
summary(dat_rating$score[dat_rating$class=='D'])
summary(dat_rating$score[dat_rating$class=='E'])



#################
# Classifiers comparison
#################

data_folds = data.frame(LR = lr2.folds, PR = lasso.folds, RF = rf.folds, ADA = ada.folds)

auc.rf = performance(rf.prediction,"auc")@y.values[[1]]
auc.rf
auc.ada = performance(ada.prediction,"auc")@y.values[[1]]
auc.ada

L.R = lr.pconf; Penalized.L.R = lasso.pconf; R.F = rf.pconf ;ADA = ada.pconf

roc = roc(data$val$Failed  ~ L.R + Penalized.L.R + R.F + ADA)
ggroc(roc, size=0.5)

ggplot(calPlotData) + ggroc(roc)
plot2 <- ggplot(calPlotData) + theme(legend.position="bottom") + ggtitle("Calibration plot")
plot1 <- ggroc(roc) + theme(legend.position="bottom") + ggtitle("Roc Curve")
plot_grid(plot1, plot2, labels = "AUTO")


# AUC
plot(lr.roc, col=3)
plot(lasso.roc, add=T, col="blue")
plot(rf.roc, add=T, col="red")
performance(rf.prediction,"auc")@y.values[[1]]
dat = data.frame(obs=data$val$Failed, pred=rf.pred, 
                 class0=rf.prob[,1], class1=rf.prob[,2])
twoClassSummary(dat, lev)
plot(calibration(data$val$Failed ~ rf.pconf, class="1"))

# check on fold data
boxplot(lr2.folds, lasso.folds, rf.folds, ada.folds)


results <- resamples(list(LR = lr2.fit, PR = lasso.fit, RF = rf.fit))
summary(results)
# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


# Testing multiple samples: non-parametric test
# Package SCMAMP: see https://github.com/b0rxa/scmamp
# installation
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("b0rxa/scmamp")
# end installation
library(scmamp)
data(data_gh_2008)
dataset = data_folds2[,!names(data_folds)=='id']
View(dataset)
plotDensities(data=dataset, size=1.1)

merged = data.frame(AUC.ROC=c(t(dataset)), classifier=rep(colnames(dataset), nrow(dataset)))
ggplot(merged, aes(x=AUC.ROC, colour=classifier)) + 
  geom_density(size=0.7)

boxplot(dataset)
# req: normality REJECTED => cannot use ANOVA
sapply(dataset, function(r) shapiro.test(r)$p.value)
# req: homogeneity of variances
View(t(dataset))
merged = data.frame(acc=c(t(dataset)), clf=rep(colnames(dataset), nrow(dataset)))
View(merged)
bartlett.test(acc~clf, data=merged)

ggplot(merged, aes(x = classifier, y = AUC.ROC)) + geom_boxplot()
ggplot(merged, aes(x = reorder(classifier,AUC.ROC), y = AUC.ROC)) + geom_boxplot()
levels(merged$classifier) = c('PR','LR', 'RF', 'ADA')

merged$classifier <- factor(merged$classifier, levels = c('PR','LR', 'RF', 'ADA'))

par(mar=c(1,1,1,1))


ggstatsplot::ggwithinstats(
  merged,
  x = classifier,
  y = AUC.ROC,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  results.subtitle = T,
  conf.level = 0.95,
  output = "subtitle"
)

ggstatsplot::ggwithinstats(
  merged,
  x = classifier,
  y = AUC.ROC,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = F,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.5, alpha = 0.5),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggplot.component = list(theme(text = element_text(size = 12))),
  ggsignif.args = list(textsize =5, tip_length = 0.01),
  output = "plot"
) 

ggstatsplot::ggwithinstats(
  merged,
  x = classifier,
  y = AUC.ROC,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = F,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.5, alpha = 0.5),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggplot.component = list(theme(text = element_text(size = 12))),
  ggsignif.args = list(textsize =2, tip_length = 0.01, annotation=c('xxx')),
  output = "plot"
) 

geom_label_repel(size = 5)

ggstatsplot::ggwithinstats(
  merged,
  x = classifier,
  y = AUC.ROC,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = F,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.5, alpha = 0.5),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggplot.component = list(theme(text = element_text(size = 12))),
  ggsignif.args = list(textsize =2.5, tip_length = 0.01, annotation=c('xxx')),
  #ylim=c(0.07,0.016),
  output = "plot"
) 







# Friedman non-parametric test
friedmanTest(dataset)
# Post-hoc Nemenyi 
test = nemenyiTest(dataset, alpha=0.05)
test
test$diff.matrix
abs(test$diff.matrix) > test$statistic # significant tests
plotCD(dataset, alpha=0.001, cex=1.25, , control="LR")
plotCD(dataset, alpha=0.05, cex=1.25, control="C4.5")

t.test(dataset$LR)
t.test(dataset$PR)
t.test(dataset$RF)


# Paired tests
# paired t-test - lower p-value than unpaired
t.test(lr2.folds, lasso.folds, paired=TRUE) # or
t.test(lr2.folds - lasso.folds, mu=0) # one-sample t-test on differences
# paired Wilcoxon test
wilcox.test(lr2.folds, lasso.folds, paired=TRUE)



library(rstatix)
merged
res.aov <- anova_test(data = merged, dv = acc,wid=rownames(merged), within = clf)
get_anova_table(res.aov)

rownames(merged)

##################################################
save(aida, file = "aidaD.RData")

sample()



x <- nrow(dat_rating[dat_rating$class == 'A' & dat_rating$Failed == "Failed",])  
x
n <- nrow(dat_rating[dat_rating$class == 'A' ,]) 
print(x);print(n)
print(x/n)
p <- threshold[c]
print(p)
binom.test(x, n, p, alternative = "less", conf.level = bonf_alfa.class)


library(datarium)
  
data("selfesteem", package = "datarium")
selfesteem
selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

selfesteem
merged$id = as.factor(rownames(merged))
merged$clf = as.factor(merged$clf)

res.aov <- anova_test(data = selfesteem, dv = score, wid = id, within = time)
get_anova_table(res.aov)

res.aov <- anova_test(data = merged, dv = acc, wid = id, within = clf)
get_anova_table(res.aov)


class(selfesteem)


selfesteem$id = merged$id
selfesteem$time = merged$clf
selfesteem$score = merged$acc




anova_test(merged ~ )

data("ToothGrowth")
df <- ToothGrowth

# One-way ANOVA test
#:::::::::::::::::::::::::::::::::::::::::

data_folds2 = data_folds

data_folds2$id = 1:nrow(data_folds2)

data_folds_prova <- data_folds2 %>%
  gather(key = "classifier", value = "prob", LR, PR, RF, ADA) %>%
  convert_as_factor(classifier,id)
head(data_folds_prova,10)

res.aov <- anova_test(data = data_folds_prova, wid = id, dv = prob, within = classifier)

get_anova_table(res.aov)

pwc <- data_folds_prova %>%
  pairwise_t_test(
    prob ~ classifier, paired = TRUE
  )
pwc
npwc = pwc[c(3,4),]

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "classifier")

data$names <- factor(data$names , levels=c("LR", "PR", "RF", "B"))
new_order <- with(data_folds_prova, reorder(classifier , prob, median , na.rm=T))
unique(new_order)
bxp = ggboxplot(data_folds_prova, x = "classifier", y = "prob", add = "point", order=unique(new_order))
bxp
bxp + 
  stat_pvalue_manual(pwc[c(1,4,6),]) +
  labs(
    #subtitle = get_test_label(res.aov, detailed = TRUE),
    #caption = get_pwc_label(pwc)
  )



library(tidyverse)
library(ggpubr)
library(rstatix)

lr.fit = train(income~age+education+occupation+sex+race, data = data$train, 
               method = "glm", family=binomial(logit))
lr.pred = predict(lr.fit, newdata = data$val)
confusionMatrix(lr.pred, data$val$income, positive="1")

tau=0.45
sel = g(lr.prob, tau)
cov0 = mean(sel)
cm = confusionMatrix(lr.pred[sel], data$val$income[sel], positive="1")
#we can use the proportion CI
cm$overall[['AccuracyLower']]
cm$overall[['AccuracyUpper']]


cm$overall["Accuracy"]
risk0 = 1-cm$overall["Accuracy"]


data_folds
test = t.test(data_folds$LR)
lr_mean_ci = test$conf.int[c(1,2)]
lr_mean_ci

test = t.test(data_folds$PR)
pr_mean_ci = test$conf.int[c(1,2)]
pr_mean_ci

lr_mean_ci - pr_mean_ci

t.test(data_folds$LR - data_folds$PR)



lr_mean_ci[1] - pr_mean_ci[1]




ggwithinstats(
  merged_delta,
  x = clf,
  y = acc,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = T,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.3, alpha = 0.7),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggsignif.args = list(textsize =0.00001, tip_length = 0.01),
)





























########################
# Question E part1
#########################

# selective classification
View(lr.prob)
g = function(prob, tau) tau > pmin(prob[1], prob[2])
taus = seq(0.05, 0.5, 0.05)
classi = c()
coverage = c()
misc = c()
brier = c()
imprec = c()
#repeat here for every classifier
temp_classi = 'ADA'
temp_fit = ada.fit
temp_prob = predict(temp_fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
temp_pred = predict(temp_fit, newdata = data$val)
for (tau in taus) {
  sel = g(temp_prob, tau)
  cov0 = mean(sel)
  coverage = append(coverage, cov0)
  cm = confusionMatrix(temp_pred[sel], data$val$Failed[sel], positive="1")
  risk0 = 1-cm$overall["Accuracy"]
  misc = append(misc, risk0)
  truey = as.numeric(data$val$Failed[sel])-1
  risk1 = mean((temp_prob[sel,2]-truey)^2) # 1-cm$byClass["Precision"]
  brier = append(brier, risk1)
  risk2 = 1-cm$byClass["Precision"]
  imprec = append(imprec, risk2)
  classi = append(classi, temp_classi)
}

tau=0.45
sel = g(lr.prob, tau)
cov0 = mean(sel)
cov0
cm = confusionMatrix(lr.pred[sel], data$val$Failed[sel], positive="1")

cm$overall["Accuracy"]
risk0 = 1-cm$overall["Accuracy"]

cm = confusionMatrix(lr.pred, data$val$Failed, positive="1")
cm$overall["Accuracy"]

check_cov = function(x) {
  sel = g(lr.prob, x)
  cov0 = mean(sel)
  return(cov0)
}
for(i in seq(0.01,0.4,0.001)){
  if(0.5 == round(check_cov(i),2)) {
    print(i)
    break
  }
}
x = 40
round(check_cov(0.169),2)
check_cov(0.169)

round(0.4,2)

# coverage-risk curve for misclassification error
plot(coverage, misc, type='b', )
# add tau as labels: pos=3 on top of points
text(coverage, misc, labels=taus, cex=0.9, font=2, pos=3) 

temdaf = data.frame(coverage = coverage, misclassification.error = misc, classifier = classi, taus = taus)
tempdf = data.frame(coverage = coverage, misclassification.error = misc, classifier = classi, taus = taus)

ggplot(tempdf, aes(x=coverage, y= misclassification.error))
  #geom_line(linetype ='dashed', aes(color=classi))+
  #geom_point( aes(color=classi)) +
  #scale_y_continuous(name='Misclassification error', breaks = seq(0,0.4, by = 0.05))
library(ggrepel)
Tau = taus
names(tempdf)[names(tempdf)=='taus'] = 'Tau'
names(temdaf)[names(temdaf)=='taus'] = 'Tau'


plot2 =ggplot(tempdf, aes(x=coverage, y= misclassification.error, group = classifier, label=Tau))+
  geom_line(linetype ='dashed', aes(color=classifier)) +
  geom_point( aes(color=classifier))  +
  scale_y_continuous(name='Misclassification error (%)', breaks=seq(0,0.5,0.05), limits =c(0,0.4))+
  theme(legend.position="top") +
  scale_x_continuous(name='Coverage (%)')

#tempdf = temdaf[temdaf$classifier =='LR',]

plot1 = ggplot(temdaf, aes(x=coverage, y= misclassification.error, label=Tau, color=Tau))+
  geom_line() + #ylim(c(0.1,0.5)) +
  geom_point() + geom_text(hjust=0.5, vjust=-1) +
  scale_color_gradient(low="blue", high="red")+ 
  theme(legend.position="top") +
  scale_y_continuous(name='Misclassification error (%)', breaks=seq(0,0.5,0.05), limits =c(0.1,0.4)) +
  scale_x_continuous(name='Coverage (%)')

plot_grid(plot1, plot2, labels = "AUTO")

library(wesanderson)
#geom_label_repel(box.padding   = 0.7, 
                   point.padding = 0.5,
                   segment.color = 'grey50')

# coverage-risk curve for Brier-score (or L2)
plot(coverage, brier, type='b')
text(coverage, brier, labels=taus, cex=0.9, font=2, pos=3) 

# coverage-risk curve for imprecision
plot(coverage, imprec, type='b', ylim=c(0,.5))
text(coverage, imprec, labels=taus, cex=0.9, font=2, pos=3) 


quantile(data$val, 0:50/50)

head(data$val)

index = as.numeric(rownames(data$val))
equal_sized_bins = cut(index, breaks=quantile(index,seq(0,1,0.1),include.lowest=T))
equal_sized_bins = unique(equal_sized_bins)

tenth = round(nrow(data$val)/10)

val_seq = seq(0,nrow(data$val), round(nrow(data$val)/10))

data$val[i:i+1276,]
[c(i, i+(round(nrow(data$val)/10)),]

acc1 = c()
acc0.5 = c()
for(i in val_seq[-11]) {
  temp_val = data$val[i:(i+1276),]
  print(i)
  prob = predict(lr.fit, newdata = temp_val, type="prob") # as predict_proba in scikit-learn
  pred = predict(lr.fit, newdata = temp_val)
  cm1 = confusionMatrix(pred, temp_val$Failed, positive="class1")
  acc1 = append(acc1, cm1$overall["Accuracy"])
  tau=0.37
  sel = g(prob, tau)
  cov0 = mean(sel)
  cov0
  cm0.5 = confusionMatrix(pred[sel], temp_val$Failed[sel], positive="class1")
  acc0.5 = append(acc0.5, cm0.5$overall["Accuracy"])
}

acc1
acc0.5
i+1276
nrow(data$val[i:(i+1276),])

lr.prob = predict(lr.fit, newdata = data$val, type="prob") # as predict_proba in scikit-learn
lr.pconf = lr.prob[,2] # scores
lr.pred = predict(lr.fit, newdata = data$val)
confusionMatrix(lr.pred, data$val$Failed, positive="class1")
g = function(prob, tau) tau > pmin(prob[1], prob[2])
taus = seq(0.05, 0.5, 0.05)
coverage = c()
misc = c()
brier = c()
imprec = c()
for (tau in taus) {
  sel = g(lr.prob, tau)
  cov0 = mean(sel)
  coverage = append(coverage, cov0)
  cm = confusionMatrix(lr.pred[sel], data$val$Failed[sel], positive="1")
  risk0 = 1-cm$overall["Accuracy"]
  misc = append(misc, risk0)
  truey = as.numeric(data$val$Failed[sel])-1
  risk1 = mean((lr.prob[sel,2]-truey)^2) # 1-cm$byClass["Precision"]
  brier = append(brier, risk1)
  risk2 = 1-cm$byClass["Precision"]
  imprec = append(imprec, risk2)
}

tau=0.371
sel = g(lr.prob, tau)
cov0 = mean(sel)
cov0
cm = confusionMatrix(lr.pred[sel], data$val$Failed[sel], positive="class1")

cm$overall["Accuracy"]
risk0 = 1-cm$overall["Accuracy"]

cm = confusionMatrix(lr.pred, data$val$income, positive="1")
cm$overall["Accuracy"]

check_cov = function(x) {
  sel = g(lr.prob, x)
  cov0 = mean(sel)
  return(cov0)
}

find_tau = function(){
  for(i in seq(0.25,0.45,0.001)){
    if(0.5 == round(check_cov(i),2)) {
      print(i)
      break
    }
  }
}
find_tau()
i
x = 40
round(check_cov(0.37),2)
check_cov(0.169)

round(0.4,2)

# coverage-risk curve for misclassification error
plot(coverage, misc, type='b', ylim=c(0,.25))
# add tau as labels: pos=3 on top of points
text(coverage, misc, labels=taus, cex=0.9, font=2, pos=3)


library(splitstackshape)


dat1 <- data.frame(ID = 1:100,
                   A = sample(c("AA", "BB", "CC", "DD", "EE"), 100, replace = TRUE),
                   B = rnorm(100), C = abs(round(rnorm(100), digits=1)),
                   D = sample(c("CA", "NY", "TX"), 100, replace = TRUE),
                   E = sample(c("M", "F"), 100, replace = TRUE))

table(dat1$E)/length(dat1$E)
table(a$E)/length(a$E)

g = function(prob, tau) {
  tau > pmin(prob[1], prob[2])
}

check_cov = function(prob, tau) {
  sel = g(prob, tau)
  cov0 = mean(sel)
  return(cov0)
}

find_tau = function(prob) {
  for(temp_tau in seq(0.4,0.3,-0.0001)){
    txvar = check_cov(prob, temp_tau)
    if(0.47 < txvar & txvar < 0.48) {
      return(temp_tau)
    }}
  for(temp_tau in seq(0.4,0.3,-0.0001)){
    txvar = check_cov(prob, temp_tau)
    if(0.46 < txvar & txvar < 0.49) {
      return(temp_tau)
    }}
  for(temp_tau in seq(0.4,0.3,-0.0001)){
    txvar = check_cov(prob, temp_tau)
    if(0.45 < txvar & txvar < 0.50) {
      return(temp_tau)
    }}
  for(temp_tau in seq(0.45,0.2,-0.0001)){
    txvar = check_cov(prob, temp_tau)
    if(0.4 < txvar & txvar < 0.55) {
      return(temp_tau)
    }}
  print('tau not found for coverage 50%')
}
find_tau(prob)
check_cov(prob,0.3424)
0.486296
0.4757053
0.5168363
0.5329154

round(check_cov(prob,0.355),2)

############
# stratified cross samples of the validation set
data$val
temp_val = data$val
temp_val$ID = seq(1, nrow(temp_val))
dat=temp_val
num_reps = 5
flds = list()
for (n in seq(1,5)){
  temp_flds=list()
  dat=temp_val
  for(i in 1:10){
    j=10-(i-1)
    if(j>1){
      a=stratified(dat, "Failed", size = 1/j)
      temp_flds[[i]]=a$ID
      dat=dat%>%filter(ID %in% setdiff(dat$ID,a$ID))
    } else{
      temp_flds[[i]]=dat$ID  
    }
  }
  flds = append(flds,temp_flds)
}
length(flds)
for (i in flds) print(length(i))
length(temp_flds)

print(table(data$val$Failed)/length(data$val$Failed))
for (i in seq(1, length(flds))) {
  print(table(data$val[flds[[i]],"Failed"])/length(data$val[flds[[i]],"Failed"]))
}

acc1 = c()
acc0.5 = c()
mod.fit = ada.fit
for (i in seq(1, length(flds))) {
  temp_val = data$val[flds[[i]],]
  print(i)
  prob = predict(mod.fit, newdata = temp_val, type="prob") # as predict_proba in scikit-learn
  pred = predict(mod.fit, newdata = temp_val)
  cm1 = confusionMatrix(pred, temp_val$Failed, positive="1")
  acc1 = append(acc1, cm1$overall["Accuracy"])
  #tau=0.372
  tau = find_tau(prob)
  print(tau)
  print(check_cov(prob,tau))
  sel = g(prob, tau)
  cov0 = mean(sel)
  cov0
  cm0.5 = confusionMatrix(pred[sel], temp_val$Failed[sel], positive="1")
  acc0.5 = append(acc0.5, cm0.5$overall["Accuracy"])
}

acc1.ada = acc1
acc0.5.ada = acc0.5


predict(ada.fit, newdata = temp_val, type="prob") # as predict_proba in scikit-learn


mean(acc0.5.lr)
plot(density(acc0.5.ada),xlim=c(0.6,0.9))
lines(density(acc1.ada))
acc.diff.lr = acc0.5.lr - acc1.lr
acc.diff.pr = acc0.5.pr - acc1.pr
acc.diff.rf = acc0.5.rf - acc1.rf
acc.diff.ada = acc0.5.ada - acc1.ada



length(acc.diff.pr)

#delta_df = data.frame(delta_LR = acc.diff.lr, delta_PR = acc.diff.pr, delta_RF = acc.diff.rf, delta_ADA = acc.diff.ada)
delta_df50bis = data.frame(delta_LR = acc.diff.lr, delta_PR = acc.diff.pr, delta_RF = acc.diff.rf, delta_ADA = acc.diff.ada)

View(delta_df)
boxplot(delta_df)
boxplot(delta_df50)
boxplot(delta_df50bis)
### multi comp
library(scmamp)
dataset = delta_df50bis
View(dataset)
plotDensities(data=dataset, size=1.1)
boxplot(dataset)
# req: normality REJECTED => cannot use ANOVA
sapply(dataset, function(r) shapiro.test(r)$p.value)
# req: homogeneity of variances
merged_delta = data.frame(acc=c(t(dataset)), clf=rep(colnames(dataset), nrow(dataset)))
bartlett.test(acc~clf, data=merged_delta)
# Friedman non-parametric test
friedmanTest(dataset)
# Post-hoc Nemenyi 
test = nemenyiTest(dataset, alpha=0.05)
test
test$diff.matrix
abs(test$diff.matrix) > test$statistic # significant tests
plotCD(dataset, alpha=0.05, cex=1.25, control="delta_PR")
plotCD(dataset, alpha=0.05, cex=1.25, control="C4.5")



summary(lr.fit)

ggbetweenstats(
  data = merged_delta,
  x = clf,
  y = acc,
  type = "parametric", # ANOVA or Kruskal-Wallis
  conf.level = 0.95,
  p.adjust.method = 'holm',
  pairwise.annotation = "p.value",
  pairwise.comparisons = T,
  pairwise.display = "significant",
  centrality.plotting = F,
  bf.message = F,
  plot.type = "box",
  results.subtitle = T,
  var.equal = T,
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.60),
    alpha = 0.4,
    size = 3,
    stroke = 0
  )
)

#paired (repeated)
ggwithinstats(
  merged_delta,
  x = clf,
  y = acc,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = T,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.3, alpha = 0.7),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggsignif.args = list(textsize =0.00001, tip_length = 0.01),
)

#save(delta_df50, file = 'delta_df50.rdata')

load('delta_df50.rdata')

boxplot(data_folds)

boxplot(delta_df50)

dataset = delta_df50
dataset$id = 1:nrow(dataset)
data_folds_prova <- dataset %>%
  gather(key = "classifier", value = "prob", delta_LR, delta_PR, delta_RF, delta_ADA) %>%
  convert_as_factor(classifier,id)
head(data_folds_prova,10)

res.aov <- anova_test(data = data_folds_prova, wid = id, dv = prob, within = classifier)

get_anova_table(res.aov)

pwc <- data_folds_prova %>% pairwise_t_test(prob ~ classifier, paired = TRUE)



pairwise_t_test(prob ~ classifier, paired = TRUE)

pwc
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "classifier")

new_order <- with(data_folds_prova, reorder(classifier , prob, median , na.rm=T))
unique(new_order)

bxp = ggboxplot(data_folds_prova, x = "classifier", y = "prob", add = "point", order=unique(new_order))
bxp = ggboxplot(data_folds_prova, x = "classifier", y = "prob", add = "point")
bxp
bxp + 
  stat_pvalue_manual(pwc[,]) +
  labs(
    #subtitle = get_test_label(res.aov, detailed = TRUE),
    #caption = get_pwc_label(pwc)
  )
pwc[1,]

.pardefault <- par()
par(.pardefault)

names(merged_delta) =c('Accuracy', 'classifier')
merged_delta$classifier = factor(merged_delta$classifier, levels = c("delta_LR", "delta_PR", "delta_RF", "delta_ADA"))

ggstatsplot::ggwithinstats(
  merged_delta,
  x = classifier,
  y = Accuracy,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  results.subtitle = T,
  conf.level = 0.95,
  output = "subtitle"
)

ggstatsplot::ggwithinstats(
  merged_delta,
  x = classifier,
  y = Accuracy,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "ns",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = F,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.5, alpha = 0.5),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggplot.component = list(theme(text = element_text(size = 12))),
  ggsignif.args = list(textsize =4.5, tip_length = 0.01),
  ylim=c(0.07,0.016),
  output = "plot"
) 

dataset

ggstatsplot::ggwithinstats(
  merged_delta,
  x = classifier,
  y = AUC.ROC,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  title = NULL,
  bf.message = F,
  results.subtitle = F,
  conf.level = 0.95,
  centrality.plotting = F,
  violin.args = list(width = 0, alpha = 0),
  boxplot.args = list(width = 0.5, alpha = 0.5),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.4),
    alpha = 0.4,
    size = 3,
    stroke = 0
  ),
  ggplot.component = list(theme(text = element_text(size = 12))),
  ggsignif.args = list(textsize =2, tip_length = 0.01, annotation=c('xxx')),
  output = "plot"
) 

###############

nrow(aida[aida$Last.accounting.closing.date==2016,])
table(aida$Failed[aida$Last.accounting.closing.date==2016])/nrow(aida[aida$Last.accounting.closing.date==2016,])









