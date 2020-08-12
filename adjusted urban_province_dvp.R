dfupydvp<-read.csv("dfupydvp.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
dfupydvp
dfupydvp<-mutate(dfupydvp,province=rep(c(
        "Bandundu",
        "Bas Congo",
        "Equateur",
        "Kasai Occidental",
        "Kasai Oriental",
        "Katanga",
        "Maniema",
        "Nord Kivu",
        "Orientale",
        "Sud Kivu",
        "Bandundu",
        "Bas Congo",
        "Equateur",
        "Kasai Occidental",
        "Kasai Oriental",
        "Katanga",
        "Kinshasa",
        "Maniema",
        "Nord Kivu",
        "Orientale",
        "Sud Kivu"),times=2))
dfupydvp
dfupydvp<-mutate(dfupydvp,urban=rep(
        c(rep("rural",times=10),rep("urban",times=11)),times=2
))
dfupydvp<-select(dfupydvp,upy,ADJMEAN,LOWCI, HIGHCI,province,urban)
dfupydvp
dfupydvp<-mutate(dfupydvp,year=rep(c(2010,2018),each=21))
dfupydvp
# draw the plot
tiff("dfupydvp_noanc.tiff",width = 8,height = 9,unit="in",res = 600)
ggplot(dfupydvp,aes(x=as.factor(year),y=ADJMEAN,fill=as.factor(urban)))+
        geom_bar(stat="identity",position = "dodge",color="black")+
        facet_wrap(.~province,nrow = 4)+
        geom_errorbar(aes(ymin=LOWCI,ymax=HIGHCI),width=0.2,
                      position = position_dodge(0.9))+
        theme_classic()+ 
        theme(strip.background = element_rect(colour="white", fill="white"))+
        theme(strip.text = element_text(size=13))+
        xlab("Year")+
        ylab("Skilled attendance at delivery")+
        scale_fill_manual(values = c("white","gray"),name="residency region")
dev.off()


############


dfupydvp<-read.csv("dfupydvp_anc.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
dfupydvp
dfupydvp<-mutate(dfupydvp,province=rep(c(
        "Bandundu",
        "Bas Congo",
        "Equateur",
        "Kasai Occidental",
        "Kasai Oriental",
        "Katanga",
        "Maniema",
        "Nord Kivu",
        "Orientale",
        "Sud Kivu",
        "Bandundu",
        "Bas Congo",
        "Equateur",
        "Kasai Occidental",
        "Kasai Oriental",
        "Katanga",
        "Kinshasa",
        "Maniema",
        "Nord Kivu",
        "Orientale",
        "Sud Kivu"),times=2))
dfupydvp
dfupydvp<-mutate(dfupydvp,urban=rep(
        c(rep("rural",times=10),rep("urban",times=11)),times=2
))
dfupydvp<-select(dfupydvp,upy,ADJMEAN,LOWCI, HIGHCI,province,urban)
dfupydvp
dfupydvp<-mutate(dfupydvp,year=rep(c(2010,2018),each=21))
dfupydvp
# draw the plot
tiff("dfupydvp_anc.tiff",width = 8,height = 9,unit="in",res = 600)
ggplot(dfupydvp,aes(x=as.factor(year),y=ADJMEAN,fill=as.factor(urban)))+
        geom_bar(stat="identity",position = "dodge",color="black")+
        facet_wrap(.~province,nrow = 4)+
        geom_errorbar(aes(ymin=LOWCI,ymax=HIGHCI),width=0.2,
                      position = position_dodge(0.9))+
        theme_classic()+ 
        theme(strip.background = element_rect(colour="white", fill="white"))+
        theme(strip.text = element_text(size=13))+
        xlab("Year")+
        ylab("Skilled attendance at delivery")+
        scale_fill_manual(values = c("white","gray"),name="residency region")
dev.off()




