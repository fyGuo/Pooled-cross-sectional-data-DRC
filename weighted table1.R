library(dplyr)
library(survey)
df<-read.csv("merge_final.csv")
table(df$year)
table(df$province)
df$year[df$year==2011]<-2001
# df cluster 
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

df1<-svydesign(id=~cluster,weights = ~wmweight,
               data=df[df$year==2001,])
df2<-svydesign(id=~cluster,weights = ~wmweight,
               data=df[df$year==2010,])
df3<-svydesign(id=~cluster,weights = ~wmweight,
               data=df[df$year==2018,])
df<-svydesign(id=~cluster,weights = ~wmweight,
               data=df)


# let's see the age first
svytable(~wmage+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~wmage+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~wmage+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~wmage+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~wmage+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~wmage+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~wmage+year,design=df,statistic="adjWald")
# Education
svytable(~welevel+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~welevel+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~welevel+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~welevel+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~welevel+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~welevel+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~welevel+year,design=df,statistic="adjWald")

######

# marital status
svytable(~marital+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~marital+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~marital+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~marital+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~marital+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~marital+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~marital+year,design=df,statistic="adjWald")


########
# residence
svytable(~urban+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~urban+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~urban+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~urban+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~urban+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~urban+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~urban+year,design=df,statistic="adjWald")


# deadchild
svytable(~deadchild+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~deadchild+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~deadchild+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~deadchild+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~deadchild+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~deadchild+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~deadchild+year,design=df,statistic="adjWald")




##total lives
svyquantile(~livebirths,design=df1,quantile(c(0,0.5,1)))
svyquantile(~livebirths,design=df2,quantile(c(0,0.5,1)))
svyquantile(~livebirths,design=df3,quantile(c(0,0.5,1)))
# province
svytable(~province+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~province+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~province+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~province+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~province+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~urban+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~urban+year,design=df,statistic="adjWald")
#  household header's sex
svytable(~hhsex+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~hhsex+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~hhsex+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~hhsex+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~hhsex+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~hhsex+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~hhsex+year,design=df,statistic="adjWald")
# household header's edcuaton
svytable(~helevel+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~helevel+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~helevel+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~helevel+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~helevel+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~helevel+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~helevel+year,design=df,statistic="adjWald")
# wealth 
svytable(~windex5+year,design=df)[,"2001"] %>% round(digits = 0)
svytable(~windex5+year,design=df)[,"2001"] %>% prop.table() %>%
        round(digits = 4)
svytable(~windex5+year,design=df)[,"2010"] %>% round(digits = 0)
svytable(~windex5+year,design=df)[,"2010"] %>% prop.table() %>%
        round(digits = 4)
svytable(~windex5+year,design=df)[,"2018"] %>% round(digits = 0)
svytable(~windex5+year,design=df)[,"2018"] %>% prop.table() %>%
        round(digits = 4)
svychisq(~windex5+year,design=df,statistic="adjWald")

