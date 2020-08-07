library(dplyr)
library(survey)
df<-read.csv("merge_final.csv")
table(df$dvp,df$year)
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

df$dvp[df$dvp=="No"]<-0
df$dvp[df$dvp=="Yes"]<-1
df$dvp<-as.numeric(df$dvp)
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
# check the report
svymean(~ANCp,design=df2)
svymean(~dvp,design=df2)

#############
dyta1<-svyby(~ANCp,~urban,design = df1,svymean)
dyta2<-svyby(~ANCp,~urban,design = df2,svymean)
dyta3<-svyby(~ANCp,~urban,design = df3,svymean)

dyta<-rbind(dyta1,dyta2,dyta3)
dyta


dcia1<-svyby(~ANCp,~urban,design = df1,svymean)%>% confint()
dcia2<-svyby(~ANCp,~urban,design = df2,svymean)%>% confint()
dcia3<-svyby(~ANCp,~urban,design = df3,svymean)%>% confint()
dcia<-rbind(dcia1,dcia2,dcia3)
dcia
dyta<-cbind(dyta,dcia)
dyta<-dyta[,c(1,2,4,5)]
dyta<-mutate(dyta,year=c(2001,2001,2010,2010,2018,2018),
             service="ANCp")
colnames(dyta)[3]<-"ymin"
colnames(dyta)[4]<-"ymax"

##########

dyts1<-svyby(~dvp,~urban,design = df1,svymean)

dyts2<-svyby(~dvp,~urban,design = df2,svymean)
dyts3<-svyby(~dvp,~urban,design = df3,svymean)

dyts<-rbind(dyts1,dyts2,dyts3)


dcis1<-svyby(~dvp,~urban,design = df1,svymean)%>% confint()
dcis2<-svyby(~dvp,~urban,design = df2,svymean)%>% confint()
dcis3<-svyby(~dvp,~urban,design = df3,svymean)%>% confint()
dcis<-rbind(dcis1,dcis2,dcis3)
dcis
dyts<-cbind(dyts,dcis)
dyts<-dyts[,c(1,2,4,5)]
dyts<-mutate(dyts,year=c(2001,2001,2010,2010,2018,2018),
             service="dvp")
dyts
dyta
colnames(dyts)[3]<-"ymin"
colnames(dyts)[4]<-"ymax"
colnames(dyta)[2]<-"prop"
colnames(dyts)[2]<-"prop"
dyt<-rbind(dyta,dyts)
dyt$us<-paste(dyt$urban,dyt$service,sep="_")
dyt$service<-factor(dyt$service,levels = c("ANCp","dvp"),
                    labels = c("Professional ANC",
                               "Professional delivery"))
ggplot(dyt, aes(x=as.factor(year),y=prop,group=as.factor(us),
                color=urban))+
        facet_grid(.~as.factor(service),
                   labeller = labeller(service = labels))+
        geom_point()+
        geom_line()+
        geom_errorbar(aes(ymin=ymin,ymax=ymax,group=urban),width=0.1)+
        theme_bw()+
        ylab(NULL)+
        xlab("year")
###########
write.csv(dyt,"cross_validation_ur.csv")
# cross-validation
d1<-svyby(~ANCp,~year,design=df,svymean)
d2<-svyby(~dvp,~year,design=df,svymean)
d1
d2
