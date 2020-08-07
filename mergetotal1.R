library(dplyr)
mics4<-read.csv("merge_mics4.csv")

str(mics4)
mics6<-read.csv("merge_mics6.csv")
str(mics4)
str(mics6)
# drop HH2 for mics6
mics6<-select(mics6,-HH2)
str(mics4)
# rename the windex
table(mics4$windex5)
table(mics6$windex5)
mics4$windex5<-factor(mics4$windex5,levels =c("poorest","poor","middle","wealth","wealthest"),
                      labels = c("poorest","poor","middle","wealth","wealthest"))
mics6$windex5<-factor(mics6$windex5,levels = c(1,2,3,4,5),
                      labels = c("poorest","poor","middle","wealth","wealthest"))
colnames(mics4)
colnames(mics6)
mics4<-select(mics4,hID,cluster,livebirths,deadchild,marital,
              welevel,MN1,MN2A,MN2B,MN2D,MN17A,MN17B,MN17D,MN18,MN3,wscore,
              wmage,windex5,province,urban,helevel,hhsex,year,wmweight)
colnames(mics4)
colnames(mics6)
mics6<-select(mics6,-X)
colnames(mics4)
colnames(mics6)
# birth place MN18 in mics4 but there is no information in mics2
mics4<-select(mics4,-MN18)
colnames(mics4)
colnames(mics6)
# ANC MN1 in mics4 Mn2 in mics6
table(mics4$MN1)
mics4$MN1[mics4$MN1=="Non"]<-0
mics4$MN1[mics4$MN1=="Oui"]<-1
mics4$MN1<-factor(mics4$MN1,levels = 0:1,
                  labels=c("No","Yes"))
colnames(mics4)[names(mics4)=="MN1"]<-"ANC"
table(mics6$MN2)
mics6$MN2[mics6$MN2==2]<-0
mics6$MN2<-factor(mics6$MN2,levels = 0:1,
                  labels=c("No","Yes"))
colnames(mics6)[names(mics6)=="MN2"]<-"ANC"
colnames(mics4)
colnames(mics6)

# professional anc
table(mics4$MN2A)
table(mics4$MN2B)
table(mics4$MN2D)
mics4$ANCp<-0
mics4$ANCp[mics4$MN2A=="Médecin"|mics4$MN2B=="Infirmière"|mics4$MN2D=="Accoucheuse"]<-1
table(mics4$ANCp,mics4$ANC)
mics4$ANCp<-factor(mics4$ANCp,levels = 0:1,
                   labels = c("No","Yes"))
colnames(mics4)[names(mics4)=='MN2A']<-"ANC_doc"
colnames(mics4)[names(mics4)=='MN2B']<-"ANC_nm"
colnames(mics4)[names(mics4)=='MN2D']<-"ANC_am"
table(mics4$ANC_doc)
mics4$ANC_doc[mics4$ANC_doc==""]<-0
mics4$ANC_doc[mics4$ANC_doc=="Médecin"]<-1
table(mics4$ANC_doc)
mics4$ANC_doc<-factor(mics4$ANC_doc,levels = 0:1,
                      labels = c("No","Yes"))
table(mics4$ANC_doc)
table(mics4$ANC_nm)
mics4$ANC_nm[mics4$ANC_nm==""]<-0
mics4$ANC_nm[mics4$ANC_nm=="Infirmière"]<-1
table(mics4$ANC_nm)
mics4$ANC_nm<-factor(mics4$ANC_nm,levels = 0:1,
                      labels = c("No","Yes"))
table(mics4$ANC_nm)

table(mics4$ANC_am)
mics4$ANC_am[mics4$ANC_am==""]<-0
mics4$ANC_am[mics4$ANC_am=="Accoucheuse"]<-1
table(mics4$ANC_am)
mics4$ANC_am<-factor(mics4$ANC_am,levels = 0:1,
                     labels = c("No","Yes"))
table(mics4$ANC_am)
# professional delivery care mics4

table(mics4$MN17A)
mics4<-mics4[mics4$MN17A!="Manquant",]
table(mics4$MN17B)
table(mics4$MN17D)
mics4$dvp<-0
mics4$dvp[mics4$MN17A=="Médecin"|mics4$MN17B=="Infirmier/Infirmière"|mics4$MN17D=="Accoucheuse"]<-1
table(mics4$dvp) %>% prop.table()
mics4$dvp<-factor(mics4$dvp,levels = 0:1,
                   labels = c("No","Yes"))
colnames(mics4)[names(mics4)=='MN17A']<-"dv_doc"
colnames(mics4)[names(mics4)=='MN17B']<-"dv_nm"
colnames(mics4)[names(mics4)=='MN17D']<-"dv_am"
table(mics4$dv_doc)
mics4$dv_doc[mics4$dv_doc==""]<-0
mics4$dv_doc[mics4$dv_doc=="Médecin"]<-1
table(mics4$dv_doc)
mics4$dv_doc<-factor(mics4$dv_doc,levels = 0:1,
                      labels = c("No","Yes"))
table(mics4$dv_doc)
table(mics4$dv_nm)
mics4$dv_nm[mics4$dv_nm==""]<-0
mics4$dv_nm[mics4$dv_nm=="Infirmier/Infirmière"]<-1
table(mics4$dv_nm)
mics4$dv_nm<-factor(mics4$dv_nm,levels = 0:1,
                     labels = c("No","Yes"))
table(mics4$dv_nm)

table(mics4$dv_am)
mics4$dv_am[mics4$dv_am==""]<-0
mics4$dv_am[mics4$dv_am=="Accoucheuse"]<-1
table(mics4$dv_am)
mics4$dv_am<-factor(mics4$dv_am,levels = 0:1,
                     labels = c("No","Yes"))
table(mics4$dv_am)



# mics 6
table(mics6$MN3A)
mics6$ANCp<-0
mics6$ANCp[mics6$MN3A=="A"|mics6$MN3C=="C"|mics6$MN3D=="D"]<-1
table(mics6$ANCp)
table(mics6$ANC,mics6$ANCp)
mics6$ANCp<-factor(mics6$ANCp,levels=0:1,
      labels=c("No","Yes"))

colnames(mics6)[names(mics6)=='MN3A']<-"ANC_doc"
colnames(mics6)[names(mics6)=='MN3C']<-"ANC_nm"
colnames(mics6)[names(mics6)=='MN3D']<-"ANC_am"
table(mics6$ANC_doc)
mics6$ANC_doc[mics6$ANC_doc==""]<-0
mics6$ANC_doc[mics6$ANC_doc=="A"]<-1
table(mics6$ANC_doc)
mics6$ANC_doc<-factor(mics6$ANC_doc,levels = 0:1,
                      labels = c("No","Yes"))
table(mics6$ANC_doc)
table(mics6$ANC_nm)
mics6$ANC_nm[mics6$ANC_nm==""]<-0
mics6$ANC_nm[mics6$ANC_nm=="C"]<-1
table(mics6$ANC_nm)
mics6$ANC_nm<-factor(mics6$ANC_nm,levels = 0:1,
                      labels = c("No","Yes"))
table(mics6$ANC_nm)

table(mics6$ANC_am)
mics6$ANC_am[mics6$ANC_am==""]<-0
mics6$ANC_am[mics6$ANC_am=="D"]<-1
table(mics6$ANC_am)
mics6$ANC_am<-factor(mics6$ANC_am,levels = 0:1,
                     labels = c("No","Yes"))
table(mics6$ANC_am)
#### ANC times
table(mics4$MN3)
table(mics6$MN5)
mics4$MN3<-as.numeric(mics4$MN3)
table(mics4$MN3)
table(mics6$MN5)
class(mics6$MN5)
mics6$MN5<-as.numeric(mics6$MN5)
colnames(mics4)[names(mics4)=="MN3"]<-"ANCtimes"
colnames(mics6)[names(mics6)=="MN5"]<-"ANCtimes"
str(mics4)
str(mics6)
mics4<-select(mics4,colnames(mics6))
colnames(mics4)==colnames(mics6)
str(mics6)
new<-rbind(mics4,mics6)
table(new$windex5,new$year)
str(new)
str(new)
dim(new)
dim(mics4)
dim(mics6)
new$cluster<-paste(new$year,new$cluster,sep="_")
length(unique(new$cluster))
length(unique(mics4$cluster))
length(unique(mics6$cluster))
table(new$ANC,new$year) %>% prop.table()
table(new$wmage,new$year) %>% prop.table()
table(new$welevel[new$year==2010]) %>% prop.table()
table(new$welevel[new$year==2018]) %>% prop.table()
############
# delivery care
table(mics6$MN19A)
table(mics6$MN19C)
mics6$dvp<-0
mics6$dvp[mics6$MN19A=="A"|mics6$MN19C=="C"|mics6$MN19D=="D"]<-1
table(mics6$dvp)

mics6$dvp<-factor(mics6$dvp,levels=0:1,
                   labels=c("No","Yes"))

colnames(mics6)[names(mics6)=='MN19A']<-"dv_doc"
colnames(mics6)[names(mics6)=='MN19C']<-"dv_nm"
colnames(mics6)[names(mics6)=='MN19D']<-"dv_am"
table(mics6$dv_doc)
mics6$dv_doc[mics6$dv_doc==""]<-0
mics6$dv_doc[mics6$dv_doc=="A"]<-1
table(mics6$dv_doc)
mics6$dv_doc<-factor(mics6$dv_doc,levels = 0:1,
                      labels = c("No","Yes"))
table(mics6$dv_doc)
table(mics6$dv_nm)
mics6$dv_nm[mics6$dv_nm==""]<-0
mics6$dv_nm[mics6$dv_nm=="C"]<-1
table(mics6$dv_nm)
mics6$dv_nm<-factor(mics6$dv_nm,levels = 0:1,
                     labels = c("No","Yes"))
table(mics6$dv_nm)

table(mics6$dv_am)
mics6$dv_am[mics6$dv_am==""]<-0
mics6$dv_am[mics6$dv_am=="D"]<-1
table(mics6$dv_am)
mics6$dv_am<-factor(mics6$dv_am,levels = 0:1,
                     labels = c("No","Yes"))
table(mics6$dv_am)


################
#
str(mics4)
str(mics6)
colnames(mics4)
colnames(mics6)
mics4<-select(mics4,colnames(mics6))
colnames(mics4)
new<-rbind(mics4,mics6)



##### let's the change the marital
table(new$marital)
new$marital[new$marital=="married"|new$marital==
                    "living with a partner"]<-1
new$marital[new$marital=="single"]<-0
table(new$marital)
new$marital<-factor(new$marital,levels = 0:1,
                    labels =  c("Single_now","living_married"))


# let's see mics 2
mics2<-read.csv("merge_mics2.csv")
str(mics2)
# rename the wealth index
colnames(mics2)[names(mics2)=="WLTHIND5"]<-"windex5"
table(mics2$windex5)
mics2$windex5<-factor(mics2$windex5,levels = c("Poorest",
                                               "Second","Middle",
                                               "Fourth","Richest"),
                      labels = c("poorest","poor","middle","wealth","wealthest"))
# rename the wmweight
colnames(mics2)[names(mics2)=="WMWEIGHT"]<-"wmweight"
# drop HI2
mics2<-select(mics2,-HI2)
table(mics2$HI3Y)
# Hi3Y is the year
colnames(mics2)[names(mics2)=="HI3Y"]<-"year"
# HI 6 is the rural or urban
table(mics2$HI6)
mics2$HI6[mics2$HI6=="Rural"]<-0
mics2$HI6[mics2$HI6=="Urbain"]<-1
table(mics2$HI6)
colnames(mics2)[names(mics2)=="HI6"]<-"urban"
table(mics2$urban)
mics2$urban<-factor(mics2$urban,levels = 0:1,
                    labels = c("rural","urban"))
str(mics2)
####HI7 is the province
table(mics2$HI7)
colnames(mics2)[names(mics2)=='HI7']<-"province"
table(mics2$province)
mics2$province[mics2$province=="Bandundu"]<-1
mics2$province[mics2$province=="Bas-congo"]<-2
mics2$province[mics2$province=="Equateur"]<-3
mics2$province[mics2$province=="Kasai Occidental"]<-4
mics2$province[mics2$province=="Kasai Oriental"]<-5
mics2$province[mics2$province=="Katanga"]<-6
mics2$province[mics2$province=="Kinshasa"]<-7
mics2$province[mics2$province=="Maniema"]<-8
mics2$province[mics2$province=="Nord-Kivu"]<-9
mics2$province[mics2$province=="Orientale"]<-10
mics2$province[mics2$province=="Sud-Kivu"]<-11
table(mics2$province)
mics2$province<-factor(mics2$province,levels = 1:11,
                     labels = c("Bandundu","Bas congo","Equateur",
                                "Kasai Occidental","Kasai Oriental",
                                "Katanga","Kinshasa","Maniema","Nord Kivu",
                                "Province Orientale","Sud Kivu"))
table(mics2$province)
#############
str(mics2)

# HI 11 number of women ekighile for review, drop
mics2<-select(mics2,-HI11)
# educ :helevel
table(mics2$EDUC)
colnames(mics2)[names(mics2)=="EDUC"]<-"helevel"
mics2$helevel[mics2$helevel=="Non-standard curriculum"|
                      mics2$helevel=="None"]<-1
mics2$helevel[mics2$helevel=="Primary"]<-2
mics2$helevel[mics2$helevel=="Secondary+"]<-3
mics2$helevel[mics2$helevel=="Missing/DK"]<-NA
table(mics2$helevel)
mics2$helevel<-factor(mics2$helevel,levels = 1:3,
                    labels =c("below primary","primary","secondary+" ))
table(mics2$helevel)  
str(mics2)
# hl33 is the sex of the household
colnames(mics2)[names(mics2)=='HL33']<-"hhsex"
table(mics2$hhsex)
mics2$hhsex[mics2$hhsex=="Feminin"]<-0
mics2$hhsex[mics2$hhsex=="Masculin"]<-1
table(mics2$hhsex)
mics2$hhsex<-factor(mics2$hhsex,levels = 0:1,labels = c("female","male"))
table(mics2$hhsex)
str(mics2)
# AGEREC is the household head's age 
mics2<-select(mics2,-AGEREC)
# WLTHSCOR is the wscore
colnames(mics2)[names(mics2)=='WLTHSCOR']<-"wscore"
# drop WI3AY,Cm2AY
mics2<-select(mics2,-WI3AY,-CM2AY)
str(mics2)
# CM7: have ever died child ,deadkids is the number
# we use CM7
table(mics2$CM7)
table(mics2$DEADKIDS)
colnames(mics2)[names(mics2)=='CM7']<-"deadchild"
mics2<-select(mics2,-DEADKIDS)
mics2$deadchild[mics2$deadchild=="Non"]<-0
mics2$deadchild[mics2$deadchild=="Oui"]<-1
mics2$deadchild<-factor(mics2$deadchild,levels = 0:1,
                        labels = c("No","Yes"))
table(mics2$deadchild)
str(mics2)
# we use MN2A
table(mics2$MN2A)
# drop the na in MN2A  and check the year
mics2<-mics2[!is.na(mics2$MN2A),]
table(mics2$MN2B)
 table(mics2$CM11)
table(mics2$year) 
mics2<-mics2[mics2$CM11Y!=1999,]
table(mics2$CM11Y)
mics2<-mics2[mics2$CM11Y!="DK",]
table(mics2$CM11Y)
mics2<-select(mics2,-CM11Y)
# MN2A
table(mics2$MN2A)
str(mics2)

colnames(mics2)[names(mics2)=='MN2A']<-"ANC_doc"
str(mics2)
colnames(mics2)[names(mics2)=='MN2B']<-"ANC_nm"
str(mics2)
colnames(mics2)[names(mics2)=="MN2C"]<-"ANC_am"
str(mics2)
table(mics2$ANC_doc)
mics2$ANC_doc[mics2$ANC_doc=="Docteur"]<-1
mics2$ANC_doc[mics2$ANC_doc=="Non"]<-0
table(mics2$ANC_doc)
mics2$ANC_doc<-factor(mics2$ANC_doc,levels = 0:1,
                      labels = c("No","Yes"))
table(mics2$ANC_doc)
table(mics2$ANC_nm)
mics2$ANC_nm[mics2$ANC_nm=="Non"]<-0
mics2$ANC_nm[mics2$ANC_nm=="Infirmiere / sage femme"]<-1
table(mics2$ANC_nm)
mics2$ANC_nm<-factor(mics2$ANC_nm,levels = 0:1,
                      labels = c("No","Yes"))
table(mics2$ANC_nm)
table(mics2$ANC_am)
mics2$ANC_am[mics2$ANC_am=="Non"]<-0
mics2$ANC_am[mics2$ANC_am=="Matrone"]<-1
table(mics2$ANC_am)
mics2$ANC_am<-factor(mics2$ANC_am,levels = 0:1,
                     labels = c("No","Yes"))
table(mics2$ANC_am)
### generate the ANCp
mics2$ANCp<-0
mics2$ANCp[mics2$ANC_doc=="Yes"|mics2$ANC_nm=="Yes"|
                   mics2$ANC_am=="Yes"]<-1
mics2$ANCp<-factor(mics2$ANCp,levels = 0:1,
                   labels = c("No","Yes"))
str(mics2)
table(mics2$ANCp)
table(mics4$ANCp)
table(mics6$ANCp)
str(mics2)
table(mics4$marital)
##MN3 delivery 
table(mics2$MN3A)
table(mics2$MN3B)
table(mics2$MN3C)

str(mics2)
colnames(mics2)[names(mics2)=='MN3A']<-"dv_doc"
str(mics2)
colnames(mics2)[names(mics2)=='MN3B']<-"dv_nm"
str(mics2)
colnames(mics2)[names(mics2)=="MN3C"]<-"dv_am"
str(mics2)
table(mics2$dv_doc)
mics2$dv_doc[mics2$dv_doc=="Docteur"]<-1
mics2$dv_doc[mics2$dv_doc=="Non"]<-0
table(mics2$dv_doc)
mics2$dv_doc<-factor(mics2$dv_doc,levels = 0:1,
                      labels = c("No","Yes"))
table(mics2$dv_doc)
table(mics2$dv_nm)
mics2$dv_nm[mics2$dv_nm=="Non"]<-0
mics2$dv_nm[mics2$dv_nm=="Infirmiere / sage femme"]<-1
table(mics2$dv_nm)
mics2$dv_nm<-factor(mics2$dv_nm,levels = 0:1,
                     labels = c("No","Yes"))
table(mics2$dv_nm)
table(mics2$dv_am)
mics2$dv_am[mics2$dv_am=="Non"]<-0
mics2$dv_am[mics2$dv_am=="Matrone"]<-1
table(mics2$dv_am)
mics2$dv_am<-factor(mics2$dv_am,levels = 0:1,
                     labels = c("No","Yes"))
table(mics2$dv_am)
# generate the professional delivery
mics2$dvp<-0
mics2$dvp[mics2$dv_doc=="Yes"|mics2$dv_nm=="Yes"|
                   mics2$dv_am=="Yes"]<-1
mics2$dvp<-factor(mics2$dvp,levels = 0:1,
                   labels = c("No","Yes"))
str(mics2)
table(mics2$dvp)
table(mics2$dv_doc)
table(mics2$dv_nm)
table(mics2$dv_am)

# MELEVEL maternal education level
table(mics2$MELEVEL)
colnames(mics2)[names(mics2)=="MELEVEL"]<-"welevel"
mics2$welevel[mics2$welevel=="Non-standard curriculum"|
                      mics2$welevel=="None"]<-1
mics2$welevel[mics2$welevel=="Primary"]<-2
mics2$welevel[mics2$welevel=="Secondary +"]<-3
mics2$welevel[mics2$welevel=="Missing/DK"]<-NA
table(mics2$welevel)
mics2$welevel<-factor(mics2$welevel,levels = 1:3,
                      labels =c("below primary","primary","secondary+" ))
table(mics2$welevel)  
str(mics2)
# MSTATUS marital status
table(mics2$MSTATUS)
colnames(mics2)[names(mics2)=="MSTATUS"]<-"marital"
mics2$marital[mics2$marital=="Currently married"]<-1
mics2$marital[mics2$marital=="Formerly married"|
                      mics2$marital=="Never married"]<-0
table(mics2$marital)
mics2$marital<-factor(mics2$marital,levels = 0:1,
                      labels = c("Single_now","living_married"))
table(mics2$marital)
# CEB  children evern born
table(mics2$CEB)
str(mics4)
colnames(mics2)[names(mics2)=="CEB"]<-"livebirths"
str(mics2)
# mics2 WAGE
table(mics2$WAGE)
colnames(mics2)[names(mics2)=='WAGE']<-"wmage"
table(mics2$wmage)
mics2$wmage[mics2$wmage=="15-19"]<-1
mics2$wmage[mics2$wmage=="20-24"]<-2
mics2$wmage[mics2$wmage=="25-29"]<-3
mics2$wmage[mics2$wmage=="30-34"]<-4
mics2$wmage[mics2$wmage=="35-39"]<-5
mics2$wmage[mics2$wmage=="40-44"]<-6
mics2$wmage[mics2$wmage=="45-49"]<-7
mics2$wmage<-factor(mics2$wmage,levels = 1:7,
                  labels = c("15-19","20-24",
                             "25-29","30-34",
                             "35-39","40-44",
                             "45-49"))
table(mics2$wmage)
str(mics2)
mics2$year<-2011
# merge the whole
colnames(new)
colnames(mics2)
# there is no ANC and ANCtimes in mics2
new<-select(new,-ANCtimes)
new<-select(new,-ANC)
mics2<-select(mics2,colnames(new))
str(mics2)
# bend the ANC_Nm and ANC_am
new$ANC_nursmid<-0
new$ANC_nursmid[new$ANC_am=="Yes"|new$ANC_nm=="Yes"]<-1
table(new$ANC_nursmid)
table(new$ANC_nm)
table(new$ANC_am)
sum(new$ANC_am=="Yes"|new$ANC_nm=="Yes")
new$ANC_nursmid<-factor(new$ANC_nursmid,levels = 0:1,
                        labels = c("No","Yes"))
new<-select(new,-ANC_am,-ANC_nm)
table(new$ANC_nursmid)

mics2$ANC_nursmid<-0
mics2$ANC_nursmid[mics2$ANC_am=="Yes"|mics2$ANC_nm=="Yes"]<-1
table(mics2$ANC_nursmid)
table(mics2$ANC_nm)
table(mics2$ANC_am)
sum(mics2$ANC_am=="Yes"|mics2$ANC_nm=="Yes")
mics2$ANC_nursmid<-factor(mics2$ANC_nursmid,levels = 0:1,
                        labels = c("No","Yes"))
mics2<-select(mics2,-ANC_am,-ANC_nm)
table(mics2$ANC_nursmid)
# mics2 the cluster
mics2$cluster<-paste(mics2$cluster,mics2$year,sep="_")
mics2$cluster
colnames(mics2)
colnames(new)
# no merge the data
new<-rbind(new,mics2)
table(new$province)
summary(new)
# now check the women age you can see it roughly have the same distribution
age<-tapply(new$wmage, new$year, table)
prop.table(age$`2010`)*100
prop.table(age$`2011`)*100
prop.table(age$`2018`)*100
sum(is.na(new$wmage))
str(new) 
# marital
table(new$marital)
marital<-tapply(new$marital, new$year, table)
prop.table(marital$`2010`)*100
prop.table(marital$`2011`)*100
prop.table(marital$`2018`)*100
str(new)
sum(is.na(new$marital))
# urban
table(new$urban)
urban<-tapply(new$urban, new$year, table)
prop.table(urban$`2010`)*100
prop.table(urban$`2011`)*100
prop.table(urban$`2018`)*100
sum(is.na(new$urban))
str(new)
# welevel
table(new$welevel)
we<-tapply(new$welevel, new$year, table)
prop.table(we$`2010`)*100
prop.table(we$`2011`)*100
prop.table(we$`2018`)*100
sum(is.na(new$welevel))
table(new$province)
# use the information here 
#https://en.wikipedia.org/wiki/Provinces_of_the_Democratic_Republic_of_the_Congo
new$province[new$province=="Tanganyika"|
                     new$province=="Haut Lomami"|
                     new$province=="Lualaba"|
                     new$province=="Haut Katanga"]<-"Katanga"
new$province[new$province=="Lomami"|
                     new$province=="Sankuru"|
                     new$province=="Kasai Oriental"]<-"Kasai Oriental"
new$province[new$province=="Kasai"|
                     new$province=="Kasai Central"]<-"Kasai Occidental"
new$province[new$province=="Kongo Central"|
                     new$province=="Bas congo"]<-"Bas Congo"
new$province[new$province=="Kwango"|
                     new$province=="Kwilu"|
                     new$province=="Maindombe"]<-"Bandundu"
new$province[new$province=="Equateur"|
                     new$province=="Tshuapa"|
                     new$province=="Mongala"|
                     new$province=="Nord Ubangi"|
                     new$province=="Sud Ubangi"]<-"Equateur"
new$province[new$province=="Bas Uele"|new$province=="Haut Uele"|
                     new$province=="Ituri"|
                     new$province=="Tshopo"|
                     new$province=="Province Orientale"]<-"Orientale"
new$province[new$province=="Maniema"|
                     new$province=="Nord Kivu"|
                     new$province=="Sud Kivu"]<-"Kivu"
table(new$province)
pro<-tapply(new$province, new$year, table)
prop.table(pro$`2010`)*100
prop.table(pro$`2011`)*100
prop.table(pro$`2018`)*100
#####
write.csv(new,"merge_final.csv")
