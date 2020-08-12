dfkasaianc<-read.csv("dfkasaianc.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)
dfkasaianc
dfkasaianc<-mutate(dfkasaianc,year=rep(c(2010,2018),times=5))
dfkasaianc<-mutate(dfkasaianc,wealth=rep(c("middle","poorer","poorest",
                                           "wealthier","wealthiest"),each=2))
dfkasaianc$wealth<-factor(dfkasaianc$wealth,
                          levels =c("poorest","poorer",
                                    "middle","wealthier",
                                    "wealthiest"))
dfkasaianc<-mutate(dfkasaianc,ser="Professional ANC1")
# kasai dvp _no anc
dfkadvpnanc<-read.csv("dfkasaidvp_noanc.csv")
dfkadvpnanc
dfkadvpnanc<-mutate(dfkadvpnanc,year=rep(c(2010,2018),times=5))
dfkadvpnanc<-mutate(dfkadvpnanc,wealth=rep(c("middle","poorer","poorest",
                                           "wealthier","wealthiest"),each=2))
dfkadvpnanc$wealth<-factor(dfkadvpnanc$wealth,
                          levels =c("poorest","poorer",
                                    "middle","wealthier",
                                    "wealthiest"))
dfkadvpnanc<-mutate(dfkadvpnanc,ser="Skilled attendance at delivery")
# kasai dvp anc adjusted
dfkadvpaanc<-read.csv("dfkasaidvp_anc.csv")
dfkadvpaanc
dfkadvpaanc<-mutate(dfkadvpaanc,year=rep(c(2010,2018),times=5))
dfkadvpaanc<-mutate(dfkadvpaanc,wealth=rep(c("middle","poorer","poorest",
                                             "wealthier","wealthiest"),each=2))
dfkadvpaanc$wealth<-factor(dfkadvpaanc$wealth,
                           levels =c("Poorest","Poorer",
                                     "Middle","Wealthier",
                                     "Wealthiest"))
dfkadvpaanc<-mutate(dfkadvpaanc,ser="Skilled attendance delivery*")
dfkadvpaanc
dfkadvpnanc
dfkasaianc
dfkasai<-rbind(dfkasaianc,dfkadvpaanc,dfkadvpnanc)
# draw the plot

ggplot(dfkasai,aes(x=as.factor(year),y=ADJMEAN,fill=as.factor(year)))+
        facet_grid(ser~wealth)+
        geom_bar(stat="identity",position = "dodge",color="black")+
        geom_errorbar(aes(ymin=LOWCI,ymax=HIGHCI),width=0.2,
                      position = position_dodge(0.9))+
        theme_classic()+ 
        theme(strip.background = element_rect(colour="white", fill="white"))+
        theme(strip.text = element_text(size=13))+
        xlab("Year")+
        ylab(NULL)+
        scale_fill_manual(values = c("white","gray"),name="Year")+
        theme(strip.text.y = element_text(size=10))



       
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




