# prepraration for the adjusted prevalence
library(dplyr)
library(foreign)
df<-read.csv("merge4_6.csv")
table(df$province)
dim(df)
table(df$year)

sapply(df, function(x){is.na(x) %>% sum})
df<-df[!is.na(df$helevel),]
sapply(df, function(x){is.na(x) %>% sum})
df<-df[!is.na(df$marital),]
sapply(df, function(x){is.na(x) %>% sum})
df<-df[!is.na(df$ANC),]
sapply(df, function(x){is.na(x) %>% sum})
df<-df[!is.na(df$ANC_doc),]
sapply(df, function(x){is.na(x) %>% sum})
#####################
# the data is ready now
df<-df[,-1]
str(df)

# create upy: urabn_province_year
df<-mutate(df,upy=paste(year,urban,province,sep="_"))
# creat pwy: year_wealth_province
df<-mutate(df,wpy=paste(year,windex5,province,sep="_"))
df$ANCp[df$ANCp=="Yes"]<-1
df$ANCp[df$ANCp=="No"]<-0
df$ANCp<-as.numeric(df$ANCp)
df$dvp[df$dvp=="Yes"]<-1
df$dvp[df$dvp=="No"]<-0
df$dvp<-as.numeric(df$dvp)
write.foreign(df,"dfupy.txt","dfupy.sas",
              package = "SAS")
write.foreign(df,"dfwpy","dfwpy.sas",
              package = "SAS")




###################
# select the Kasai region
dfkasai<-df[df$province=="Kasai Occidental"|
                    df$province=="Kasai Oriental"|
                    df$province=="Maniema",]
dfkasai<-mutate(dfkasai,winyear=paste(windex5,year,sep="_"))
write.foreign(dfkasai,"dfkasai","dfkasai.sas",
              package = "SAS")
table(dfkasai$urban,dfkasai$year)
table(dfkasai$dvp[dfkasai$year==2010&dfkasai$windex5=="wealthest"],
      dfkasai$ANCp[dfkasai$year==2010&dfkasai$windex5=="wealthest"])

table(dfkasai$dvp[dfkasai$year==2018&dfkasai$windex5=="wealthest"],
      dfkasai$ANCp[dfkasai$year==2018&dfkasai$windex5=="wealthest"])
