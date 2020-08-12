dfupyanc<-read.csv("dfupyanc.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
dfupyanc
dfupyanc<-mutate(dfupyanc,province=rep(c(
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
dfupyanc
dfupyanc<-mutate(dfupyanc,urban=rep(
        c(rep("rural",times=10),rep("urban",times=11)),times=2
))
dfupyanc<-select(dfupyanc,upy,ADJMEAN,LOWCI, HIGHCI,province,urban)
dfupyanc
dfupyanc<-mutate(dfupyanc,year=rep(c(2010,2018),each=21))
dfupyanc
# draw the plot
tiff("dfupyanc.tiff",width = 8,height = 9,unit="in",res = 600)
ggplot(dfupyanc,aes(x=as.factor(year),y=ADJMEAN,fill=as.factor(urban)))+
        geom_bar(stat="identity",position = "dodge",color="black")+
        facet_wrap(.~province,nrow = 4)+
        geom_errorbar(aes(ymin=LOWCI,ymax=HIGHCI),width=0.2,
                      position = position_dodge(0.9))+
        theme_classic()+ 
        theme(strip.background = element_rect(colour="white", fill="white"))+
        theme(strip.text = element_text(size=13))+
        xlab("Year")+
        ylab("Professional ANC1")+
        scale_fill_manual(values = c("white","gray"),name="residency region")
dev.off()


############



