
# Authors : Giovanni Scognamiglio and Martina Trigilia
# Course  : Statistics For Data Science
# Teacher : Salvatore Ruggieri 

#lib and functions
library(DescTools)
library(scmamp)
library(BSDA)
library(PMCMRplus)
library(classInt)
library(nortest)
library(tseries)
library(ggplot2)
library(car)
library(EMT)
library(Hmisc)
library(dplyr)

load('aida.Finale.RData')

year_selected = aida$Last.accounting.closing.date==2016

# classIntervals(aida$Size, n=3, style="quantile")

# fix an year 
aida = aida[year_selected,]

aida$Size.bin <- cut2(aida$Size, g=3)

aida$Size.bin <- as.character(aida$Size.bin)

aida$Size.bin[aida$Size.bin == "[ 5.63,10.8)"] <- "Small"
aida$Size.bin[aida$Size.bin == "[10.81,12.5)"] <- "Medium"
aida$Size.bin[aida$Size.bin == "[12.46,18.9]"] <- "Large"

aida$Size.bin <- factor(aida$Size.bin,levels = c("Small","Medium","Large"))

ggplot(aida, aes(x=Size.bin)) +
  geom_bar(fill="steelblue") +
  labs(x="Size",y="Count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4, hjust=0.8))


######## Size ########

# 1) What is the probability of failures conditional to age of firms 
# at a specific year (2016)?

failed = aida$Failed==1

# Probability P(Failure=1|Size=x) is P(Failure=1,Size=x)/P(Size=x)
list_values = c()
list_keys = c()

for (size in levels(aida$Size.bin)){
  
  p <- nrow(aida[aida$Size.bin==size & failed,])/nrow(aida[aida$Size.bin==size,])
  list_values <- append(list_values,p)
  list_keys <- append(list_keys,size)
  
}

SizeCond <- data.frame (
  size = list_keys,  
  probability = list_values
)

rm(list_keys)
rm(list_values)

probSize <- SizeCond$probability

locations <- levels(aida$Location)

# Bonferroni Correction For Multi Test on Size 

bonf_alfa.size <- 1-(0.05/3)

data_small = aida$Failed[aida$Size.bin == "Small"]
data_medium= aida$Failed[aida$Size.bin == "Medium"]

trials_small <- length(data_small)
succ_small <- table(data_small)[2]

trials_medium <- length(data_small)
succ_medium <- table(data_small)[2]

binom.test(succ_small,trials_small,SizeCond$probability[SizeCond$size=="Medium"],conf.level = bonf_alfa.size, alternative = 'g')
binom.test(succ_small,trials_small,SizeCond$probability[SizeCond$size=="Large"],conf.level = bonf_alfa.size, alternative = 'g')
binom.test(succ_medium,trials_medium,SizeCond$probability[SizeCond$size=="Large"],conf.level = bonf_alfa.size, alternative = 'g')


# Barplot Conditional Probability of Failure given age
size_pro_cond <- ggplot(SizeCond, aes(x=factor(size, level = levels(aida$Size.bin)), y=probability)) + 
  geom_bar(stat = "identity", fill="#CD5C5C") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label=round(probability, digits = 2)), vjust=1.6, color="white", size=3.5)+
  xlab("Size") + ylab("P(Failed=Yes | Size = x)") +
  scale_y_continuous(limits=c(0,0.7))


# Probability P(Failure=1|Size = x & Location =l) is P(Failure=1,Size=x,Location =l)/P(Size=x,Location =l)
order_key <- levels(aida$Size.bin)
trials_loc <- c()
succ_loc <- c()

rel_prod = function(size_value,loc) {
  data = aida$Failed[aida$Size.bin== size_value & aida$Location == loc]
  return((table(data)/length(data))[[2]])
}


locations <- levels(aida$Location)
# Bonferroni Correction For Multi Test on Location
m_loc <- nlevels(aida$Location) 
bonf_alfa.loc <- 1-(0.05/m_loc)
bin_tests <- c()

get_bin.test = function(loc,size) {
  data = aida$Failed[aida$Size.bin == size & aida$Location == loc]
  trials_loc <- length(data)
  succ_loc <- table(data)[2]
  l_b <- binom.test(succ_loc,trials_loc,SizeCond$probability[SizeCond$size==size],conf.level = bonf_alfa.loc)
  bin_tests <- append(bin_tests,l_b)
  return(bin_tests)
}

bin_small <- sapply(levels(aida$Location),get_bin.test,"Small")
SizeCond$probability[SizeCond$size=="Small"]
bin_medium <- sapply(levels(aida$Location),get_bin.test,"Medium")
SizeCond$probability[SizeCond$size=="Medium"]
bin_large <- sapply(levels(aida$Location),get_bin.test,"Large")
SizeCond$probability[SizeCond$size=="Large"]

bin_small["p.value",]
bin_medium["p.value",]
bin_large["p.value",]

bin_small["estimate",]
bin_medium["estimate",]
bin_large["estimate",]




list_res = c(probSize)
list_loc = c(replicate(3, "Size"))

for (loc in levels(aida$Location)){
  
  res = sapply(order_key, rel_prod, loc)
  l_l <- replicate(3, loc)
  list_loc <- append(list_loc,l_l)
  list_res <- unname(append(list_res, res))
  
}

SizeCondLocation <- data.frame (
  Size = order_key,
  Prob = list_res,
  Group = list_loc
)

ggplot(SizeCondLocation, aes(x=Group, y=Prob, color=Group)) +
  geom_boxplot() +
  labs(x="Group", y = "Probability")

ggplot(subset(SizeCondLocation, Group == "Sud"), aes(x=factor(Size, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Size=x & Location=Sud) in 2016") + 
  xlab("Size") + ylab("P(Failed=Yes | Size=x & Location=Sud)")

ggplot(subset(SizeCondLocation, Group == "Centro"), aes(x=factor(Size, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Size=x & Location=Centro) in 2016") + 
  xlab("Size") + ylab("P(Failed=Yes | Size=x & Location=Centro)")

ggplot(subset(SizeCondLocation, Group == "Nord-ovest"), aes(x=factor(Size, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Size=x & Location=Nord Ovest) in 2016") + 
  xlab("Size") + ylab("P(Failed=Yes | Size=x & Location=Nord Ovest)")

ggplot(subset(SizeCondLocation, Group == "Nord-est"), aes(x=factor(Size, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x & Location=Nord Est) in 2016") + 
  xlab("Size") + ylab("P(Failed=Yes | Size=x & Location=Nord Est)")

ggplot(subset(SizeCondLocation, Group == "Isole"), aes(x=factor(Size, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Size=x & Location=Isole) in 2016") + 
  
  xlab("Size") + ylab("P(Failed=Yes | Size=x & Location=Isole)")


########### LEGAL FORM

# Probability P(Failure=1|Size=x & LegalForm = Form) is P(Failure=1,Age=x,Legal Form = form)/P(Age=x,Legal Form =form)

form <- levels(aida$Legal.form)
# Bonferroni Correction For Multi Test  on Location

m_form <- nlevels(aida$Legal.form) 
bonf_alfa.form <- 1-(0.05/m_form)
bin_tests <- c()
get_bin.test = function(form,size) {
  data = aida$Failed[aida$Size.bin == size & aida$Legal.form == form]
  trials <- length(data)
  succ <- table(data)[2]
  l_b <- binom.test(succ,trials,SizeCond$probability[SizeCond$size==size],conf.level = bonf_alfa.form)
  bin_tests <- append(bin_tests,l_b)
  return(bin_tests)
}

bin_small <- sapply(levels(aida$Legal.form),get_bin.test,"Small")
bin_medium <- sapply(levels(aida$Legal.form),get_bin.test,"Medium")
bin_large <- sapply(levels(aida$Legal.form),get_bin.test,"Large")

bin_small["p.value",]
bin_medium["p.value",]
bin_large["p.value",]

bin_small["estimate",]
bin_medium["estimate",]
bin_large["estimate",]


rel_prodForm = function(size_value,form) {
  data = aida$Failed[aida$Size.bin== size_value & aida$Legal.form == form]
  return((table(data)/length(data))[[2]])
}

list_res = c(probSize)
list_form = c(replicate(30, "Size"))

for (form in levels(aida$Legal.form)){
  
  res = sapply(order_key, rel_prodForm, form)
  l_f <- replicate(30, form)
  print(l_f)
  list_form <- append(list_form,l_f)
  list_res <- unname(append(list_res, res))
  
}

SizeCondForm <- data.frame (
  Size = order_key,
  Prob = list_res,
  Group = list_form
)


ggplot(SizeCondForm[SizeCondForm$Size=="Medium",], aes(x=Group, y=Prob, color=Group)) +
  geom_boxplot() +
  labs(title="Plot of Probability per Legal Form Medium Size",x="Group", y = "Probability")

###########

# Probability P(Failure=1|Age=x & Ateco.Name = ateco) is P(Failure=1,Age=x & Ateco.Name = ateco)/P(Age=x,Ateco.Name = ateco)

### ATECO NAME 

ateco.name <- levels(aida$ATECO.NAME)
# Bonferroni Correction For Multi Test  on Location
m_ateco <- nlevels(aida$ATECO.NAME) 
bonf_alfa.ateco <- 1-(0.05/m_ateco)

get_bin.test = function(ateco,size) {
  data = aida$Failed[aida$Size.bin == size & aida$ATECO.NAME == ateco]
  trials <- length(data)
  succ <- table(data)[2]
  l_b <- binom.test(succ,trials,SizeCond$probability[SizeCond$size==size],conf.level = bonf_alfa.ateco)
  bin_tests <- append(bin_tests,l_b)
  return(bin_tests)
}

bin_tests <- c()

bin_small <- sapply(ateco.name,get_bin.test,"Small")
bin_medium <- bin_tests <- sapply(ateco.name,get_bin.test,"Medium")
bin_large <- bin_tests <- sapply(ateco.name,get_bin.test,"Large")

bin_small["p.value",]
bin_medium["p.value",]
bin_large["p.value",]

bin_small["estimate",]
bin_medium["estimate",]
bin_large["estimate",]


rel_prodAteco = function(size_value,ateco.name) {
  data = aida$Failed[aida$Size.bin== size_value & aida$ATECO.NAME == ateco.name]
  return((table(data)/length(data))[[2]])
}

list_res = c(probSize)
list_ateco = c(replicate(30, "Size"))

for (ateco.name in levels(aida$ATECO.NAME)){
  
  res = sapply(order_key, rel_prodAteco, ateco.name)
  l_a <- replicate(30, ateco.name)
  list_ateco <- append(list_ateco,l_a)
  list_res <- unname(append(list_res, res))
  
}

SizeCondAteco <- data.frame (
  Size = order_key,
  Prob = list_res,
  Group = list_ateco
)

ggplot(SizeCondAteco, aes(x=Group, y=Prob, color=Group)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Plot of Probability per Ateco Name",x="Group", y = "Probability")




###########################AGE###################


aida$Age.bin <- cut2(aida$Age, g=3)

aida$Age.bin <- as.character(aida$Age.bin)

levels(aida$Age.bin)

aida$Age.bin <- factor(aida$Age.bin,levels = c("[ 0,  4)","[ 4, 11)","[11,114]"))

ggplot(aida, aes(x=Age.bin)) +
  geom_bar(fill="steelblue") +
  labs(x="Age",y="Count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4, hjust=0.8))

######## Age ########

# 1) What is the probability of failures conditional to age of firms 
# at a specific year (2016)?

failed = aida$Failed==1

# Probability P(Failure=1|Age=x) is P(Failure=1,Age=x)/P(Age=x)
list_values = c()
list_keys = c()

for (Age in levels(aida$Age.bin)){
  
  p <- nrow(aida[aida$Age.bin==Age & failed,])/nrow(aida[aida$Age.bin==Age,])
  list_values <- append(list_values,p)
  list_keys <- append(list_keys,Age)
  
}

AgeCond <- data.frame (
  Age = list_keys,  
  probability = list_values
)

rm(list_keys)
rm(list_values)

probAge <- AgeCond$probability

# Barplot Conditional Probability of Failure given age
age_cond_prob <- ggplot(AgeCond, aes(x=factor(Age, level = levels(aida$Age.bin)), y=probability)) + 
  geom_bar(stat = "identity", fill="#CD5C5C") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(aes(label=round(probability, digits = 2)), vjust=1.6, color="white", size=3.5)+
  xlab("Age") + ylab("P(Failed=Yes | Age = x)") + 
  scale_y_continuous(limits=c(0,0.7))

library(cowplot)
plot_grid(size_pro_cond, age_cond_prob, labels = "AUTO")

bonf_alfa.Age <- 1-(0.05/3)

data_young = aida$Failed[aida$Age.bin == "[ 0,  4)"]

data_medium= aida$Failed[aida$Age.bin == "[ 4, 11)"]

trials_young <- length(data_young)
succ_young <- table(data_young)[2]

trials_medium <- length(data_young)
succ_medium <- table(data_young)[2]

binom.test(succ_young,trials_young,AgeCond$probability[AgeCond$Age=="[ 4, 11)"],conf.level = bonf_alfa.Age, alternative = 'l')
binom.test(succ_young,trials_young,AgeCond$probability[AgeCond$Age=="[11,114]"],conf.level = bonf_alfa.Age, alternative = 'l')
binom.test(succ_medium,trials_medium,AgeCond$probability[AgeCond$Age=="[11,114]"],conf.level = bonf_alfa.Age, alternative = 'l')

# Probability P(Failure=1|Age = x & Location =l) is P(Failure=1,Age=x,Location =l)/P(Age=x,Location =l)

order_key <- levels(aida$Age.bin)

rel_prod = function(Age_value,loc) {
  data = aida$Failed[aida$Age.bin== Age_value & aida$Location == loc]
  return((table(data)/length(data))[[2]])
}


locations <- levels(aida$Location)
# Bonferroni Correction For Multi Test  on Location
m_loc <- nlevels(aida$Location) 
bonf_alfa.loc <- 1-(0.05/m_loc)

get_bin.test = function(loc,Age) {
  data = aida$Failed[aida$Age.bin == Age & aida$Location == loc]
  trials <- length(data)
  succ <- table(data)[2]
  l_b <- binom.test(succ,trials,AgeCond$probability[AgeCond$Age==Age],conf.level = bonf_alfa.ateco)
  bin_tests <- append(bin_tests,l_b)
  return(bin_tests)
}

bin_tests <- c()

bin_young <- sapply(locations,get_bin.test,"[ 0,  4)")
bin_medium <-  sapply(locations,get_bin.test,"[ 4, 11)")
bin_large <- sapply(locations,get_bin.test,"[11,114]")

bin_young["p.value",]
bin_medium["p.value",]
bin_large["p.value",]

bin_young["estimate",]
bin_medium["estimate",]
bin_large["estimate",]


list_res = c(probAge)
list_loc = c(replicate(3, "Age"))

for (loc in levels(aida$Location)){
  
  res = sapply(order_key, rel_prod, loc)
  l_l <- replicate(3, loc)
  list_loc <- append(list_loc,l_l)
  list_res <- unname(append(list_res, res))
  
}

AgeCondLocation <- data.frame (
  Age = order_key,
  Prob = list_res,
  Group = list_loc
)

ggplot(AgeCondLocation, aes(x=Group, y=Prob, color=Group)) +
  geom_boxplot() +
  labs(x="Group", y = "Probability")

ggplot(subset(AgeCondLocation, Group == "Sud"), aes(x=factor(Age, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x & Location=Sud) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x & Location=Sud)")

ggplot(subset(AgeCondLocation, Group == "Centro"), aes(x=factor(Age, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x & Location=Centro) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x & Location=Centro)")

ggplot(subset(AgeCondLocation, Group == "Nord-ovest"), aes(x=factor(Age, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x & Location=Nord Ovest) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x & Location=Nord Ovest)")

ggplot(subset(AgeCondLocation, Group == "Nord-est"), aes(x=factor(Age, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x & Location=Nord Est) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x & Location=Nord Est)")

ggplot(subset(AgeCondLocation, Group == "Isole"), aes(x=factor(Age, level = order_key), y=Prob)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("P(Failed=Yes | Age=x & Location=Isole) in 2016") + 
  xlab("Age") + ylab("P(Failed=Yes | Age=x & Location=Isole)")


########### LEGAL FORM

# Probability P(Failure=1|Age=x & LegalForm = Form) is P(Failure=1,Age=x,Legal Form = form)/P(Age=x,Legal Form =form)

form <- levels(aida$Legal.form)
# Bonferroni Correction For Multi Test  on Location
m_form <- nlevels(aida$Legal.form) 
bonf_alfa.form <- 1-(0.05/m_form)

get_bin.test = function(form,age) {
  data = aida$Failed[aida$Age.bin == age & aida$Legal.form == form]
  trials <- length(data)
  succ <- table(data)[2]
  l_b <- binom.test(succ,trials,AgeCond$probability[AgeCond$Age==Age],conf.level = bonf_alfa.form)
  bin_tests <- append(bin_tests,l_b)
  return(bin_tests)
}

bin_tests <- c()

bin_young <- sapply(form,get_bin.test,"[ 0,  4)")
bin_medium <-  sapply(form,get_bin.test,"[ 4, 11)")
bin_large <- sapply(form,get_bin.test,"[11,114]")

bin_young["p.value",]
bin_medium["p.value",]
bin_large["p.value",]

bin_young["estimate",]
bin_medium["estimate",]
bin_large["estimate",]

rel_prodForm = function(Age_value,form) {
  data = aida$Failed[aida$Age.bin== Age_value & aida$Legal.form == form]
  return((table(data)/length(data))[[2]])
}

list_res = c(probAge)
list_form = c(replicate(30, "Age"))

for (form in levels(aida$Legal.form)){
  
  res = sapply(order_key, rel_prodForm, form)
  l_f <- replicate(30, form)
  print(l_f)
  list_form <- append(list_form,l_f)
  list_res <- unname(append(list_res, res))
  
}

AgeCondForm <- data.frame (
  Age = order_key,
  Prob = list_res,
  Group = list_form
)


ggplot(AgeCondForm, aes(x=Group, y=Prob, color=Group)) +
  geom_boxplot() +
  labs(x="Group", y = "Probability")

###########

# Probability P(Failure=1|Age=x & Ateco.Name = ateco) is P(Failure=1,Age=x & Ateco.Name = ateco)/P(Age=x,Ateco.Name = ateco)

### ATECO NAME 

ateco.name <- levels(aida$ATECO.NAME)
# Bonferroni Correction For Multi Test  on Location
m_ateco <- nlevels(aida$ATECO.NAME) 
bonf_alfa.ateco <- 1-(0.05/m_ateco)

get_bin.test = function(ateco,Age) {
  data = aida$Failed[aida$Age.bin == Age & aida$ATECO.NAME == ateco]
  trials <- length(data)
  succ <- table(data)[2]
  l_b <- binom.test(succ,trials,AgeCond$probability[AgeCond$Age==Age],conf.level = bonf_alfa.form)
  bin_tests <- append(bin_tests,l_b)
  return(bin_tests)
}

bin_tests <- c()

bin_young <- sapply(ateco.name,get_bin.test,"[ 0,  4)")
bin_medium <-  sapply(ateco.name,get_bin.test,"[ 4, 11)")
bin_old <- sapply(ateco.name,get_bin.test,"[11,114]")

bin_young["p.value",]
bin_medium["p.value",]
bin_old["p.value",]

bin_young["estimate",]
bin_medium["estimate",]
bin_old["estimate",]

rel_prodAteco = function(Age_value,ateco.name) {
  data = aida$Failed[aida$Age.bin== Age_value & aida$ATECO.NAME == ateco.name]
  return((table(data)/length(data))[[2]])
}

list_res = c(probAge)
list_ateco = c(replicate(30, "Age"))

for (ateco.name in levels(aida$ATECO.NAME)){
  
  res = sapply(order_key, rel_prodAteco, ateco.name)
  l_a <- replicate(30, ateco.name)
  list_ateco <- append(list_ateco,l_a)
  list_res <- unname(append(list_res, res))
  
}

AgeCondAteco <- data.frame (
  Age = order_key,
  Prob = list_res,
  Group = list_ateco
)

ggplot(AgeCondAteco[AgeCondAteco$Age=="[ 0,  4)",], aes(x=Group, y=Prob, color=Group)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Plot of Probability per Ateco Name, [ 0,  4)",x="Group", y = "Probability")
