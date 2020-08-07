# In this file we will merge household data with women data in 2001 MICS2
# Load the HH data from previous path.
hh<-read.csv("hhmics2.csv")
wm<-read.csv("wmmics2.csv")
# this code we say how many duplicates in the household number in hh and wm file
sum(duplicated(hh$HI2))
str(hh)
hh<-hh[,-1] # we delete the first column here, because it is no meaning
str(hh)
# with function tell the R ,we do the following function in the hh data.frame
# paste0 creates a character to bind all the information together, it is little confusing ,just try it
hh$hID<-paste(hh$HI1A,hh$HI1B,hh$HI1C,hh$HI1D,hh$HI1E,hh$HI2,sep="_")
hh$cluster<-paste(hh$HI1A,hh$HI1B,hh$HI1C,hh$HI1D,hh$HI1E,sep="_")
hh$hID
length(unique(hh$cluster))
# unique tells us how many unique hID are. How many houesholds in the data
length(unique(hh$hID))
# same process to the wm
str(wm)
wm<-wm[,-1]
wm$hID<-with(wm,paste(HI1A,HI1B,HI1C,HI1D,HI1E,HI2,sep="_"))
wm$hID
length(unique(wm$hID))
summary(hh$HI11)
# HI2 is the only household number, we just use it to check our new hID
hh$HI2
length(hh$HI2)
summary(hh$HI2)
wm$HI2
length(wm$HI2)
summary(wm$HI2)
sum(duplicated(wm$HI2))
length(unique(wm$HI2))
length(unique(wm$hID))
# like before "-" means we don't want the columns here
hh<-select(hh,-c(HI1A,HI1B,HI1C,HI1D,HI1E))
wm<-select(wm,-c(HI1A,HI1B,HI1C,HI1D,HI1E))
length(wm$HI2)
# merge could combine two datasets togther by "hID" and "HI2". In theory every women shoud belong to a
# household
new<-merge(hh,wm,by=c("hID","HI2"),all=FALSE)
length(new$HI2)
summary(new)
unique(new$hID)
# we do some check above
# below are some complicated functions, you don't need to understant them
library(data.table)
wm<-as.data.table(wm)
new<-as.data.table(new)
g<-new[,.(.N),by=.(hID)]
g[order(N),]
# this tells us what the maximum number of households. In other words, how many women living in one 
#household at most
g<-new[,.(.N),by=.(HI2)]
g[order(N),]
new[,.(.N),by=.(HI2)]    
wm[,max(.N),by=.(HI2)] 
gw<-wm[,.(.N),by=.(hID)]
gw[order(N),]
dim(new)
dim(wm)
# like above just double check 
# Question : you need go back the mics 2 preprocess.R there is a question in hh file that asks 
#No. of women interviews completed. Check it with the maximum number of women living in one household 
# as we show above
write.csv(new,"merge_mics2.csv")
