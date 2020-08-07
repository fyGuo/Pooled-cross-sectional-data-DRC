########This is the R.file to get the regression resuls
# Load the package
library(survey)
library(dplyr)
library(survey)
df<-read.csv("merge_final.csv")
# correct an error in the data
table(df$year)
table(df$province)
df$year[df$year==2011]<-2001
# let's check the sampling cluster
summary(df$cluster)
unique(df$cluster)
cid<-data.frame(cluster=unique(df$cluster),
                cid=1:length(unique(df$cluster)))


df<-merge(df,cid,by="cluster")
length(unique(df$cid))
#person id
df<-mutate(df,id=1:dim(df)[1])
df$ANCp[df$ANCp=="No"]<-0
df$ANCp[df$ANCp=="Yes"]<-1
df$ANCp<-as.numeric(df$ANCp)
df$dvp[df$dvp=="No"]<-0
df$dvp[df$dvp=="Yes"]<-1
df$dvp<-as.numeric(df$dvp)
summary(df)
str(df)
sum(df$wmweight)
# let's check the missing values in the data
sum(is.na(df))
# So there is 41 NA, let's find what are they
sum(is.na(df$X))
sum(is.na(df$hID))
sum(is.na(df$cluster))
sum(is.na(df$livebirths))
sum(is.na(df$deadchild))
df[which(is.na(df$deadchild)),]
df<-df[!is.na(df$deadchild),]
sum(is.na(df$marital))
sum(is.na(df$welevel))
sum(is.na(df$ANC_doc))
sum(is.na(df$wscore))
sum(is.na(df$wmage))
sum(is.na(df$province))
sum(is.na(df$urban))
sum(is.na(df$helevel))
df$year[is.na(df$helevel)] %>% table
table(df$year)
df<-df[!is.na(df$helevel),]
is.na(df) %>% sum
sum(df$wmweight)
#change the marital status order
table(df$marital)
df$marital<-factor(df$marital,levels =c("Single_now","living_married") )
df$windex5<-factor(df$windex5,levels = c("poorest","poor",
                                         "middle","wealth","wealthest"))
# change the wealth index group order
table(df$year,df$windex5)
table(df$windex5)
df$windex5<-factor(df$windex5,levels = c("poorest","poor",
                                         "middle","wealth","wealthest"))


#####################################################
# In this section we apply the survey package to account for 
#the women weight
#svy design

dfu<-svydesign(id=~cluster,weights = ~wmweight,
               data=df[df$urban=="urban",])
dfr<-svydesign(id=~cluster,weights = ~wmweight,
               data=df[df$urban=="rural",])
df<-dfr<-svydesign(id=~cluster,weights = ~wmweight,
                   data=df)

########################################################
# Everything is ready, let's do the regression
############## ANCp
# OVerall model
moda<-svyglm(ANCp ~livebirths+
                          as.factor(wmage)+
                          as.factor(deadchild)+
                          as.factor(marital)+
                          as.factor(welevel)+
                          as.factor(windex5)+
                          as.factor(province)+as.factor(urban)+
                          as.factor(helevel)+
                          as.factor(hhsex)+
                          as.factor(year),
                  family = "binomial",df)
# to get the p-value
summary(moda)
# to get the Odds Ratio
summary(moda)$coefficients %>% exp()
############################
# ANCP in urban
modau<-svyglm(ANCp ~livebirths+
                     as.factor(wmage)+
                     as.factor(deadchild)+
                     as.factor(marital)+
                     as.factor(welevel)+
                     as.factor(windex5)+
                     as.factor(province)+
                     as.factor(helevel)+
                     as.factor(hhsex)+
                     as.factor(year),
             family = "binomial",dfu)
summary(modau)
summary(modau)$coefficients %>% exp()
# ANCp in rural
modar<-svyglm(ANCp ~livebirths+
                      as.factor(wmage)+
                      as.factor(deadchild)+
                      as.factor(marital)+
                      as.factor(welevel)+
                      as.factor(windex5)+
                      as.factor(province)+
                      as.factor(helevel)+
                      as.factor(hhsex)+
                      as.factor(year),
              family = "binomial",dfr)
summary(modar)
summary(modar)$coefficients %>% exp()
###########
# skilled attendance at delivery in overall
modd<-svyglm(dvp ~livebirths+ANCp+
                    as.factor(wmage)+
                    as.factor(deadchild)+
                    as.factor(marital)+
                    as.factor(welevel)+
                    as.factor(windex5)+
                    as.factor(province)+as.factor(urban)+
                    as.factor(helevel)+
                    as.factor(hhsex)+
                    as.factor(year),
            family = "binomial",df)
summary(modd)
summary(modd)$coefficients %>% exp()
#skilled attendance at delivery in urban
modu<-svyglm(dvp ~livebirths+ANCp+
                     as.factor(wmage)+
                     as.factor(deadchild)+
                     as.factor(marital)+
                     as.factor(welevel)+
                     as.factor(windex5)+
                     as.factor(province)+
                     as.factor(helevel)+
                     as.factor(hhsex)+
                     as.factor(year),
             family = "binomial",dfu)
summary(modu)
summary(modu)$coefficients %>% exp()
#skilled attendance at delivery in rural
modr<-svyglm(dvp ~livebirths+ANCp+
                     as.factor(wmage)+
                     as.factor(deadchild)+
                     as.factor(marital)+
                     as.factor(welevel)+
                     as.factor(windex5)+
                     as.factor(province)+
                     as.factor(helevel)+
                     as.factor(hhsex)+
                     as.factor(year),
             family = "binomial",dfr)
summary(modr)
summary(modr)$coefficients %>% exp()
