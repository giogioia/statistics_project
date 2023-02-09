# authors: Giovanni Scognamiglio; Martina Trigilia

# Load library
library(ggplot2) 
library(forcats) 
library(readxl) 
library(DescTools)
library(fitdistrplus)
library(ks)
library(BSDA)
library(plyr)
library(MASS)
###########
# AIDA data loading
###########

# working dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

load("aida.RData")

# sub all blank spaces with dot
names(aida) <- make.names(names(aida), unique=TRUE)

table(aida$Legal.status)/nrow(aida) 
# I valori di Legal Status merger e demerger possono essere eliminati  
merger_vect <- c("Dissolved (demerger)", "Dissolved (merger)")
aida <- aida[!(aida$Legal.status %in% merger_vect),] 

aida$Legal.status <- droplevels(aida$Legal.status)

# Define binary variable Failed: 1 for failed firm, 0 otherwise
active_status_vect <- c("Active", "Active (receivership)", "Active (default of payments)")
aida$Failed <- ifelse(aida$Legal.status %in% active_status_vect, 0, 1)
#sort(table(aida$Failed)*100/nr, decreasing=T)
# Create levels for Failed
aida$Failed <- factor(aida$Failed)
# levels(aida$Failed)

# remove NA in LEGAL FORM
#è importante droppare prima i NAN perchè sennò non si possono droppare i levels!
aida = aida[!is.na(aida$Legal.form),] 

# table(aida[,c('Legal.form')])

## LEGAL FORM ACCORPIAMO I VALORI POCHI IN OTHER
accorpate = c('S.A.P.A.','Foundation','Foreign company','S.C.A.R.I.',
              'Public agency',  'Mutual aid society', 'Association',
              "S.N.C.", "S.A.S.")

# un-factorize
aida$Legal.form <- as.character(aida$Legal.form)

aida$Legal.form[aida$Legal.form %in% accorpate]<- "Other"

aida$Legal.form[aida$Legal.form %in% c("S.C.A.R.L.P.A.", "Social cooperative company")] <- "S.C.A.R.L."

# re-factorize 
aida$Legal.form <- as.factor(aida$Legal.form)

levels(aida$Legal.form)

# bar count of Legal Form
ggplot(aida, aes(x = forcats::fct_infreq(Legal.form), fill = Failed)) +
  geom_bar() + 
  labs( 
       x="Legal Form", y = "Count") +
  coord_flip()

# CREAZIONE VARIABILE AGE
aida <- aida[!(is.na(aida['Incorporation.year'])),] 

#269 rows for this condition (aida[aida$Incorporation.year > aida$Last.accounting.closing.date,])
aida <- aida[!(aida$Incorporation.year > aida$Last.accounting.closing.date),]
aida$Age <- aida$Last.accounting.closing.date - aida$Incorporation.year

# rimuoviamo dal dataset le aziende che hanno un età negativa
aida <- aida[!(aida$Age<0),] 


#### AGE PLOT
df <- data.frame(Age = aida$Age)

# overlay histogram and normal density
# parameter fitting for a gamma family

library(actuar) # for dpareto, use actuar instead of VGAM (which runs into errors with fitdist)
# parameter fitting for a power-law family
data = aida$Age
actuar::fpareto = fitdist(data, "pareto", start=list(shape=2, scale=1)) 
fpareto
plot.legend = c("exp", "pareto")
lest = list(fexp, fpareto)
denscomp(lest, legendtext = plot.legend, main=NULL) # compare densities
cdfcomp(lest, legendtext = plot.legend) # compare cumulative
qqcomp(lest, legendtext = plot.legend) # compare quantiles


# LOCATION

regioni_istat <- read_excel("Elenco-comuni-italiani.xls")

regioni_istat <- unique(regioni_istat[, c('Denominazione Regione', 'Ripartizione geografica')])

names(regioni_istat)[names(regioni_istat) == 'Denominazione Regione'] <- "Registered.office.address...Region"

#convert region variable to character
aida$Registered.office.address...Region <- as.character(aida$Registered.office.address...Region)

aida <- merge(regioni_istat,aida,by="Registered.office.address...Region")

names(aida)[names(aida) == 'Ripartizione geografica'] <- "Location"

#make location a factor
aida$Location <- as.factor(aida$Location)

levels(aida$Location)

# PRE-PROCESSING VARIABILE ATECO

# droppiamo i NA di Ateco_2007Code
aida = aida[!is.na(aida$ATECO.2007code),] 

aida = aida[!(aida$ATECO.2007code=="000000"),] 

# nrow(aida[aida$ATECO.2007code=="000000",]) #193 rows with missing Ateco.Code

aida$ATECO.2007code <- substr(aida$ATECO.2007code, 1, 2)

get_ATECO.NAME = function(ateco.value) {
  
  if(ateco.value %in% c("01","02","03"))
    return ("A")
  
  if(ateco.value %in% c("05", "06", "07", "08", "09"))
    return ("B")
  
  if(ateco.value %in% as.character(c(10:33)))
    return("C")
  
  if(ateco.value=="35")
    return("D")
  
  if(ateco.value %in% as.character(c(36:39)))
    return("E")
  
  if(ateco.value %in% as.character(c(41:43)))
    return("F")
  
  if(ateco.value %in% as.character(c(45:47)))
    return("G")
  
  if(ateco.value %in% as.character(c(49:53)))
    return("H")
  
  if(ateco.value %in% as.character(c(55:56)))
    return("I")
  
  if(ateco.value %in% as.character(c(58:63)))
    return("J")
  
  if(ateco.value %in% as.character(c(64:66)))
    return("K")
  
  if(ateco.value=="68")
    return("L")
  
  if(ateco.value %in% as.character(c(69:75)))
    return("M")
  
  if(ateco.value %in% as.character(c(77:82)))
    return("N")
  
  if(ateco.value=="84")
    return("O")
  
  if(ateco.value=="85")
    return("P")
  
  if(ateco.value %in% as.character(c(86:88)))
    return("Q")
  
  if(ateco.value %in% as.character(c(90:93)))
    return("R")
  
  if(ateco.value %in% as.character(c(94:96)))
    return("S")
  
  if(ateco.value %in% as.character(c(97:98)))
    return("T")
  
  if(ateco.value=="99")
    return("U")
  
}

aida$ATECO.NAME <- sapply(aida$ATECO.2007code, get_ATECO.NAME) 

aida$ATECO.NAME <- as.factor(aida$ATECO.NAME)

aida.name_toremove <- c("T", "O", "U")

aida <- aida[!(aida$ATECO.NAME %in% aida.name_toremove),] 
aida$ATECO.NAME <- droplevels(aida$ATECO.NAME)

list_res = c()

rel_prodAteco = function(ateco) {
  data = aida$Failed[aida$ATECO.NAME == ateco]
  return((table(data)/length(data))[[2]])
}

list_res = c()
list_res = unname(sapply(levels(aida$ATECO.NAME), rel_prodAteco))

CondAteco <- data.frame (
  Prob = list_res,
  Group = levels(aida$ATECO.NAME)
)

ggplot(CondAteco, aes(reorder(Group,-Prob,sum),Prob)) + geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x="Ateco Name", y = "Conditional Probability")

rm(CondAteco);rm(list_res);rm(rel_prodAteco)

# un-factorize
aida$ATECO.NAME <- as.character(aida$ATECO.NAME)

aida$ATECO.NAME[aida$ATECO.NAME %in% c("F", "N")]<- "FN"
aida$ATECO.NAME[aida$ATECO.NAME %in% c("G", "M")]<- "GM"
aida$ATECO.NAME[aida$ATECO.NAME %in% c("J", "C")]<- "JC"
aida$ATECO.NAME[aida$ATECO.NAME %in% c("P", "B")]<- "PB"
aida$ATECO.NAME[aida$ATECO.NAME %in% c("K", "D")]<- "KD"
aida$ATECO.NAME[aida$ATECO.NAME %in% c("E", "L")]<- "EL"

# re-factorize 
aida$ATECO.NAME <- as.factor(aida$ATECO.NAME)
#levels(aida$ATECO.NAME)

# bar count of Ateco Name
ggplot(aida, aes(x = forcats::fct_infreq(ATECO.NAME))) +
  geom_bar(fill = "#CD5C5C") + 
  labs(x="Ateco Name", y = "Count") +
  coord_flip()

# CREAZIONE E PRE-PROCESSIG DELLA VARIABILE SIZE
aida = aida[!is.na(aida$Total.assetsth.EURLast.avail..yr),]

aida_backup = aida

aida = aida_backup
aida_backup$Size = ifelse(aida$Total.assetsth.EURLast.avail..yr==0, 0, log(aida$Total.assetsth.EURLast.avail..yr*1000))
x = ifelse(aida$Total.assetsth.EURLast.avail..yr==0, 0, log(aida$Total.assetsth.EURLast.avail..yr*1000))
plot(density(aida_backup$Size))
iqr = IQR(aida_backup$Size)
quan = quantile(aida_backup$Size,c(0.25,0.75))
quan
lowlim = quan[1]-2*iqr
uplim = quan[2]+2*iqr
aida_backup  = aida_backup[aida_backup$Size >lowlim & aida_backup$Size<uplim,]
aida  = aida[x >lowlim & x<uplim,]

#### 

mu <- mean(aida_backup$Size)
sd <- sd(aida_backup$Size)

  size_before <-ggplot(data=aida_backup, aes(x=Size)) +
  geom_density(adjust=1, alpha=0.2) + 
  labs(x="Size before cleaning", y = "Density")

x = aida$Total.assetsth.EURLast.avail..yr
sort(table(x[log(x)<3 & log(x)>2]), decreasing=T)[1:10]
tt = sort(table(x[log(x)<4 & log(x)>2]), decreasing=T)[1:500]
names.tt = as.numeric(names(tt))

#IMPUTING 10s
table(aida$Failed)/length(aida$Failed)
table(aida$Failed[x ==10])/length(aida$Failed[x==10])
# 10 is in 8th quantile and has prob fail = 39%
qu = quantile(x,c(0.07,0.09))
table(aida$Failed[x>qu[1] & x<qu[2]])/length(aida$Failed[x>qu[1] & x<qu[2]])
#values around 8th quantile have much higher prob fail = 52%
qu = quantile(x,c(0.25,0.35))
table(aida$Failed[x>qu[1] & x<qu[2]])/length(aida$Failed[x>qu[1] & x<qu[2]])
length(aida$Failed[x>qu[1] & x<qu[2]])
#adequate quantile location for 10s is 25 and 35th quantile
qu = quantile(x,c(0.25,0.35))
tnum = length(aida$Total.assetsth.EURLast.avail..yr[x ==10])
aida$Total.assetsth.EURLast.avail..yr[x ==10] = runif(tnum,qu[1],qu[2])
#aida$Total.assetsth.EURLast.avail..yr[x ==10] = sample(aida$Total.assetsth.EURLast.avail..yr[x>qu[1] & x<qu[2]], length(aida_full$Total.assetsth.EURLast.avail..yr[x ==10]))

#IMPUTING 1s
table(aida$Failed)/length(aida$Failed)
table(aida$Failed[x ==1])/length(aida$Failed[x==1])
# 1 is in 1.5th quantile and has prob fail = 33%
qu = quantile(x,c(0.01,0.02))
qu
table(aida$Failed[x>qu[1] & x<qu[2]])/length(aida$Failed[x>qu[1] & x<qu[2]])
#values around 1.5th quantile have much higher prob fail = 57%
qu = quantile(x,c(0.35,0.45))
table(aida$Failed[x>qu[1] & x<qu[2]])/length(aida$Failed[x>qu[1] & x<qu[2]])
#adequate quantile location for 10s is 35 and 45th quantile
qu = quantile(x,c(0.35,0.45))
tnum = length(aida$Total.assetsth.EURLast.avail..yr[x ==1])
aida$Total.assetsth.EURLast.avail..yr[x ==1] = runif(tnum,qu[1],qu[2])
#aida$Total.assetsth.EURLast.avail..yr[x ==1] = sample(aida$Total.assetsth.EURLast.avail..yr[x>qu[1] & x<qu[2]], length(aida$Total.assetsth.EURLast.avail..yr[x ==1]))

aida2 = aida

aida=aida2
x = ifelse(aida$Total.assetsth.EURLast.avail..yr==0, 0, round(log(aida$Total.assetsth.EURLast.avail..yr*1000),5))
#x = round(log(aida$Total.assetsth.EURLast.avail..yr),5)
plot(density(x))
tt = sort(table(x[x<9.607755 & x>8.907755]), decreasing=T)[1:850]
names.tt = as.numeric(names(tt))
#IMPUTING tt
table(aida$Failed)/length(aida$Failed)
table(aida$Failed[x %in% names.tt])/length(aida$Failed[x %in% names.tt])
# 1 is in 1.5th quantile and has prob fail = 33%
summary(names.tt)
qu = quantile(x,c(0.04,0.08))
qu
table(aida$Failed[x>qu[1] & x<qu[2]])/length(aida$Failed[x>qu[1] & x<qu[2]])
length(aida$Failed[x>qu[1] & x<qu[2]])
#adequate quantile location
#qu = quantile(x,c(0.03,0.15))
#qu
tnum = length(aida$Total.assetsth.EURLast.avail..yr[x %in% names.tt])
table(aida$Failed)/length(aida$Failed)
table(aida$Failed[x %in% names.tt])/length(aida$Failed[x %in% names.tt])
length(aida$Failed[x %in% names.tt])
qu = quantile(x,c(0.025,0.25))
#qu = quantile(x,c(0.009,0.3))
qu
qu[1];qu[2]
quantile(x, 0.5)
tlis = c()
temp_num = rnorm(1e6, median(x), 1.9)
for (i in temp_num) {
  if (i > qu[1] & i < qu[2]) {
    tlis = append(tlis, i)
    if (length(tlis) == tnum) break
    print(length(tlis))
  }
}
x[x %in% names.tt] = sample(tlis, tnum)

dat <- data.frame(Size = x)
#Plot.
size_after <- ggplot(dat, aes(x = Size)) + geom_density(alpha = 0.5) +
  stat_function(fun = dnorm,
                args = list(mean = mean(dat$Size),
                            sd = sd(dat$Size)),
                col = "red",
                size = 1) +
  labs(x="Size after cleaning", y = "Density")

library(cowplot)
plot_grid(size_before, size_after, labels = "AUTO")

aida$Size <- x

aida_full = aida

save(aida, file = "aida.Finale.RData")

rm(regioni_istat)
