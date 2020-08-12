df<-read.csv("merge4_6.csv")
df


library(survey)
df$ANCp[df$ANCp=="Yes"]<-1
df$ANCp[df$ANCp=="No"]<-0
df$ANCp<-as.numeric(df$ANCp)
df$dvp[df$dvp=="Yes"]<-1
df$dvp[df$dvp=="No"]<-0
df$dvp<-as.numeric(df$dvp)
df4<-svydesign(~1,weights = ~wmweight,data=df[df$year==2010,])
df4
svyciprop(~dvp,design = df4)
svyciprop(~ANCp,design = df4)
df6<-svydesign(~1,weights = ~wmweight,data=df[df$year==2018,])
svyciprop(~dvp,design = df6)
svyciprop(~ANCp,design = df6)
# let's read the data in a data.frame
trend<-data.frame(year=rep(c(2010,2018),each=2),
                  ser=rep(c("Professional ANC1","Skilled attendance at delivery"),
                          time=2))
trend<-mutate(trend, pre=c(0.873,0.742,0.824,0.852),
              ymin=c(0.861,0.725,0.811,0.841),
              ymax=c(0.88,0.76,0.84,0.86))
# make the plot
library(ggplot2)
library(ggthemes)
tiff("wmweighed_fig.tiff",width = 6.5,height = 6,unit="in",res = 600)
ggplot(trend,aes(x=as.factor(year),y=pre,fill=as.factor(year)))+
        geom_bar(stat="identity",position = "dodge",color="black")+
        geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2,
                      position = position_dodge(0.9))+
        facet_grid(.~ser)+
        theme_classic()+
        xlab("Year")+
        ylab(NULL)+
        theme(strip.background = element_rect(colour="white", fill="white"))+
        theme(strip.text = element_text(size=13))+
        scale_fill_manual(values = c("white","gray"),name="Year")+
        scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),
                           labels = c("0","0.25","0.50","0.75","1.00"),
                           limits = c(0,1.01))
dev.off()

