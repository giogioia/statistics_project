
# Authors : Giovanni Scognamiglio and Martina Trigilia
# Course  : Statistics For Data Science
# Teacher : Salvatore Ruggieri 

#lib and functions
library(DescTools)
library(ks)
library(BSDA)
library(nortest)
library(tseries)
library(plotrix)    


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


############ QUESTION 1.0 ############

#### SIZE ###
year_selected = aida$Last.accounting.closing.date==2016

aida = aida[aida$Last.accounting.closing.date==2016,]


mu_size <- ddply(aida, "Failed", summarise, grp.mean=mean(Size))
p <- ggplot(aida, aes(x=Size,color=Failed)) + 
  geom_density(lwd=0.8)+ 
  labs(x="Size", y = "Density")
size_plot <- p + geom_vline(data=mu, aes(xintercept=grp.mean, color=Failed),
             linetype="dashed") +scale_color_manual(values=c("#007500", "#FF0000"))
 

 p <- ggplot(aida, aes(x=Age,color=Failed)) + 
   geom_density(lwd=0.8)+ 
   labs(x="Age", y = "Density")
 age_plot <- p +scale_color_manual(values=c("#007500", "#FF0000"))

library(cowplot)
plot_grid(size_plot, age_plot, labels = "AUTO")



########

ks.test(aida$Size, pnorm, mean(aida$Size), sd(aida$Size)) #D = 0.12054, p-value < 2.2e-16
 
aidaS_0 = aida$Size[aida$Failed==0]

aidaS_1 = aida$Size[aida$Failed==1]

mean(aidaS_0); mean(aidaS_1)

#do we reject that both sample come the same dist?
ks.test(aidaS_1, aidaS_0) #D = 0.12054, p-value < 2.2e-16

#checking for normality
# KS TEST, reject H0, data does not follow a normal distribution
ks.test(aidaS_1, pnorm, mean(aidaS_1), sd(aidaS_1)) #D = 0.0087027, p-value = 0.003329
ks.test(aidaS_0, pnorm, mean(aidaS_0), sd(aidaS_0)) #D = 0.030286, p-value < 2.2e-16

# Anderson-test, (reject H0) data does not follow a normal distribution
ad.test(aidaS_1) # A = 7.7405, p-value < 2.2e-16
ad.test(aidaS_0) # A = 67.434, p-value < 2.2e-16

# Jarque Bera (reject H0)
jarque.bera.test(aidaS_1)
jarque.bera.test(aidaS_0) # X-squared = 615.09,  p-value < 2.2e-16

# QQ-plot, reject normal distribution 

qqnorm(aidaS_0, pch = 1, frame = FALSE)
qqline(aidaS_0, col = "steelblue", lwd = 2)

qqnorm(aidaS_1, pch = 1, frame = FALSE)
qqline(aidaS_1, col = "steelblue", lwd = 2)

#extracting smaller samples
s1 = get_best_sample(aidaS_1,5000)
s0 = get_best_sample(aidaS_0,5000)

sh1 <- shapiro.test(s1) #p-value = 0.0004097
sh0 <- shapiro.test(s0) #p-value = 2.2e-16
# sh1
# sh0


###NOT NORMAL -- General Assump.
z.test(aidaS_0, aidaS_1, sigma.x=sd(aidaS_0), sigma.y=sd(aidaS_1)) #p-value < 2.2e-16 (95%CI: 0.4196349 0.4738494)



### --> we see that the more precise CI is provided by the large number z.test. 
#We are 95% confident that the true difference of the mean size of Active and Failed is between 0.44 and 0.49
industry_form <- levels(aida$Legal.form)
unique.ateco.name <- levels(aida$ATECO.NAME) 

m_form <- nlevels(aida$Legal.form) + 1

# Bonferroni Correction For Multi Test on Legal Form
bonf_alfa.form <- 1-(0.05/m_form)

m_ateco <- nlevels(aida$ATECO.NAME)

# Bonferroni Correction For Multi Test on ATECO NAME
bonf_alfa.ateco <- 1-(0.05/m_ateco)

get_ztest.ateco = function(ateco.name, aida_attr){
  aida_ateco = aida$ATECO.NAME==ateco.name 
  aidaS_0 = aida_attr[aida$Failed==0 & aida_ateco]
  aidaS_1 = aida_attr[aida$Failed==1 & aida_ateco]
  # large sample, general data assum. 
  ztest <- z.test(aidaS_0, aidaS_1, sigma.x=sd(aidaS_0), sigma.y=sd(aidaS_1), conf.level = bonf_alfa.ateco)
  
  return(ztest)
  
}

get_ztest.form = function(form, aida_attr){
  aida_form = aida$Legal.form==form 
  aidaS_0 = aida_attr[aida$Failed==0 & aida_form]
  aidaS_1 = aida_attr[aida$Failed==1 & aida_form]
  # large sample, general data assum. 
  ztest <- z.test(aidaS_0, aidaS_1, sigma.x=sd(aidaS_0), sigma.y=sd(aidaS_1), conf.level = bonf_alfa.form)
  return(ztest)
  
}

############ QUESTION A.1 - Size) ############

table(aida[,c('Legal.form','Failed')])

##Legal Form SIZE

z.test(aidaS_0, aidaS_1, sigma.x=sd(aidaS_0), sigma.y=sd(aidaS_1), conf.level = bonf_alfa.form) #p-value < 2.2e-16 (95%CI: 0.4196349 0.4738494)

z_list.form_size = sapply(industry_form, get_ztest.form, aida$Size) 

z_list.form_size['conf.int',]
z_list.form_size['estimate',]
z_list.form_size['p.value',]


##Ateco Name SIZE


table(aida[,c('ATECO.NAME','Failed')])

z_list.ateco_size <- sapply(unique.ateco.name, get_ztest.ateco, aida$Size) 

z_list.ateco_size['conf.int',]
z_list.ateco_size['estimate',]
z_list.ateco_size['p.value',]



############ QUESTION A.1 - Age) ############

aidaA_0 = aida$Age[aida$Failed==0]
aidaA_1 = aida$Age[aida$Failed==1]


p <- ggplot(data=aida, aes(x=Age, color=Failed)) +
  geom_density(adjust=1, alpha=0.2) 
p + scale_color_manual(values=c("#00FF00", "#FF0000"))

mean(aidaA_1) # 10.92
mean(aidaA_0) # 8.16

# we reject that both sample come the same dist
ks.test(aidaA_0, aidaA_1) # D = 0.15674, p-value < 2.2e-16



###General Data, Large Sample
### --> we see that the more precise CI is provided by the large number z.test. 
z.test(aidaA_0,aidaA_1, sigma.x=sd(aidaA_0), sigma.y=sd(aidaA_1)) #p-value < 2.2e-16 (95%CI: -2.92 -2.62)

##############

##Legal Form AGE

mean(aida$Size[aida$Failed == 1])
mean(aida$Size[aida$Failed == 0])


z_list.form_age = sapply(industry_form, get_ztest.form, aida$Age) 

z_list.form_age['conf.int',]
z_list.form_age['estimate',]
z_list.form_age['p.value',]


##Ateco Name

z_list.ateco_age <- sapply(unique.ateco.name, get_ztest.ateco, aida$Age) 

z_list.ateco_age['p.value',]
z_list.ateco_age['conf.int',]
z_list.ateco_age['estimate',]
