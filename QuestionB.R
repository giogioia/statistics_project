
# Authors : Giovanni Scognamiglio and Martina Trigilia
# Course  : Statistics For Data Science
# Teacher : Salvatore Ruggieri 


library(DescTools)
library(ks)
library(BSDA)
library(nortest)
library(tseries)
library(plotrix)  
library(ggplot2)
library(cowplot)


#get samples function
tryf = function(data,n) {
  fhat = kde(data)
  samp = rkde(n, fhat) 
  ks_res = ks.test(samp,data)
  return(list('ks_res'=ks_res,'samp'=samp))
}

get_best_sample = function(data, n) {
  smallest_d = 1
  for (x in 1:10) {
    ks_output = tryf(data, n)
    dstat = unname(unlist(ks_output[['ks_res']]['statistic']))
    #print(dstat)
    if (dstat < smallest_d) {
      smallest_d = dstat
      best_ns0 = ks_output[['samp']]
    }
  }
  return(best_ns0)
}

load('aida.Finale.RData')

# let's fix the Failed at 1
aida <- aida[aida$Failed==1,]

ggplot(aida, aes(x = Last.accounting.closing.date)) +
  geom_bar()

year_X = aida$Last.accounting.closing.date==2014

year_Y = aida$Last.accounting.closing.date==2016
aida_X = aida[aida$Last.accounting.closing.date==2014,]
aida_Y = aida[aida$Last.accounting.closing.date==2016,]

### PLOT SIZE and AGE Distribution
aida_new = aida[aida$Last.accounting.closing.date %in% c(2014,2016),]
aida_new = aida_new[aida_new$Legal.form %in% industry_form,]

colnames(aida_new)[which(names(aida_new) == "Last.accounting.closing.date")] <- "Year"  
aida_new$Year <- as.factor(aida_new$Year)

size_dens1 <- ggplot(aida_new, aes(x=Size, color=Year)) +
  geom_density()

age_dens1 <- ggplot(aida_new, aes(x=Age, color=Year)) +
  geom_density()

plot_grid(size_dens1, age_dens1, labels = "AUTO")

# PLOTS LEGAL FORM
ggplot(aida_new[aida_new$Legal.form=="S.R.L. simplified",], aes(x=Size, color=Year)) +
  geom_density()

ggplot(aida_new[aida_new$Location=="S.R.L. simplified",], aes(x=Age, color=Year)) +
  geom_density()


############ QUESTION 1.0 SIZE ############ 

# let's take the two distribution of Size to compare

aida_SX = aida_X$Size
aida_SY = aida_Y$Size

# we reject H0, that both sample come the same distribution
ks.test(aida_SX, aida_SY) # D = 0.022567, p-value = 6.003e-10

#checking for normality

qqnorm(aida_SX, pch = 1, frame = FALSE)
qqline(aida_SX, col = "steelblue", lwd = 2)

qqnorm(aida_SY, pch = 1, frame = FALSE)
qqline(aida_SY, col = "steelblue", lwd = 2)

#extracting smaller samples
sX = get_best_sample(aida_SX,5000)
sY = get_best_sample(aida_SY,5000)

shX <- shapiro.test(sX) #p-value = 0.01687
shY <- shapiro.test(sY) #p-value = 0.0007962

### GENERAL DATA assumption.

# we reject H0, that the samples have the same mean
z.test(aida_SX, aida_SY, sigma.x=sd(aida_SX), sigma.y=sd(aida_SY)) # p-value = 2.2e-16 (95%CI: 0.112 0.170)

Z = mean(aida_SX) - mean(aida_SY)
fullX_adjusted = aida_SX - Z

ks.test(fullX_adjusted,aida_SY)

wilcox.test(aida_SX, aida_SY, conf.int = T) # p-value =2.2e-16,  95%CI(0.01 0.15), DELTA: 0.122

# every sample has large sample size, so we can apply for general assump. 
table(aida$Legal.form[year_X])
table(aida$Legal.form[year_Y])

table(aida$Location[year_X])
table(aida$Location[year_Y])

# CALCULATE BONFERRONI CORRECTION ON CI FOR LEGAL FORM AND LOCATION

industry_form <- c("Consortium","S.C.A.R.L.", "S.P.A.", "S.R.L.", "S.R.L. one-person")

# Bonferroni Correction For Multi Test on Legal Form
m_form <- length(industry_form) + 1
bonf_alfa.form <- 1-(0.05/m_form)

locations <- levels(aida$Location)

# Bonferroni Correction For Multi Test on Location
m_loc <- nlevels(aida$Location) + 1
bonf_alfa.loc <- 1-(0.05/m_loc)


get_ztest.location = function(location, aida_attr){
  
  aida_location = aida$Location==location
  
  year_X = aida$Last.accounting.closing.date==2014
  
  year_Y = aida$Last.accounting.closing.date==2016
  
  fullX = aida_attr[year_X & aida_location]
  fullY = aida_attr[year_Y & aida_location]
  
  print(location)
 
  # same shape test for wilcox assumption

  print("wilcox.test")
    
  test_list <- wilcox.test(fullX, fullY, conf.int = T, conf.level = bonf_alfa.form ) # p-value = 2.004e-05,  95%CI(0.03241141 0.08756097), DELTA: 0.05999382
  
  return(test_list)
  
}

get_ztest.form = function(form, aida_attr){
  
  aida_form = aida$Legal.form==form 
  
  year_X = aida$Last.accounting.closing.date==2014
  
  year_Y = aida$Last.accounting.closing.date==2016
  
  fullX = aida_attr[year_X & aida_form]
  fullY = aida_attr[year_Y & aida_form]

  test_list <- wilcox.test(fullX, fullY, conf.int = T, conf.level = bonf_alfa.form ) # p-value = 2.004e-05,  95%CI(0.03241141 0.08756097), DELTA: 0.05999382
  return(test_list)
  
}

########### QUESTION B.1 Size ######## 

# Legal Form
print("Size")
print("Industry Form")

wilcox.test(aida_SX, aida_SY, conf.int = T, conf.level = bonf_alfa.form) 

t_list.form_size <- sapply(industry_form, get_ztest.form, aida$Size) 

t_list.form_size['conf.int',]
t_list.form_size['estimate',]
t_list.form_size['p.value',]


########### QUESTION B.1 Size ######## 

## Location

print("Size")
print("Location")
#wilc adjusted
wilcox.test(aida_SX, aida_SY, conf.int = T, conf.level = bonf_alfa.loc) 

list.loc_size <- sapply(locations, get_ztest.location, aida$Size) 

list.loc_size['estimate',]
list.loc_size['p.value',]


############ QUESTION 1.0 AGE ############## 

# let's take the two distribution of Age to compare

aida_AX = aida_X$Age[aida_X$Legal.form %in% industry_form]
aida_AY = aida_X$Age[aida_Y$Legal.form %in% industry_form]

# we reject H0 that both sample come the same distribution
ks.test(aida_AX, aida_AY) 

### GENERAL DATA

# z.test(aida_AX, aida_AY, sigma.x=sd(aida_AX), sigma.y=sd(aida_AY)) #  p-value < 2.2e-16 (95%CI: 11.49  11.35)

#  wilx assump. --> same shape, checked before

wilcox.test(aida_AX, aida_AY, conf.int = T) 

########### QUESTION B. 1 Age ######## 

## Legal Form
wilcox.test(aida_AX, aida_AY, conf.int = T, conf.level = bonf_alfa.form) 
list.form_age <- sapply(industry_form, get_ztest.form, aida$Age) 

list.form_age["conf.int",]
list.form_age['estimate',]
list.form_age['p.value',]


########### QUESTION B.2 ######## Age

## LOcation
wilcox.test(aida_AX, aida_AY, conf.int = T, conf.level = bonf_alfa.loc) 
list.loc_age <- sapply(locations, get_ztest.location, aida$Age) 

list.loc_age[c('conf.int',"method"),]
list.loc_age["conf.int",]
list.loc_age['estimate',]
list.loc_age['p.value',]

