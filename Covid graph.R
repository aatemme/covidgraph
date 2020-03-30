### graphing script

library(tidyverse)
library(lubridate)
library(ggpubr)


  big.data <- read.csv("http://covidtracking.com/api/states/daily.csv")

###### target state  
  target.state<-"NY"
  data <- big.data %>% filter(state==target.state)
######
  

#### add today's data for state if not on covidtracking
  # add.today <- data.frame(date=c(20200328),positive=c(2366))
  # data <- merge(data,add.today,all=T)
#####


##### consider data after positive cases greater then 10
data <- data %>% filter(positive>=10) %>% select(date,positive)

data$time <- ymd(data$date)

data <- data %>% arrange(time)

###### calculate doubling rate over data window
  day.range <- 3  # number of days to fit line over
  
  doubling <- data.frame(time=data$time[day.range:dim(data)[1]],
                         slope=rep(NA,dim(data)[1]-(day.range-1)),
                         intercept=rep(NA,dim(data)[1]-(day.range-1)))
  
  
  for (i in 1:length(doubling$time)) {
    
    mod.data <- data[c(which(data$time==doubling$time[i]):(which(data$time==doubling$time[i])-(day.range-1))), ]
    
    mod1 <- lm(data=mod.data, log2(positive)~time)
  
    doubling$slope[i] <- coef(mod1)[2]
    doubling$intercept[i] <- coef(mod1)[1]
    
  }
  
  doubling$double.days <- 1/doubling$slope


###### plotting
lims <- c(10,ceiling(max(data$positive)/1000)*1000)

normal.scaled <- ggplot(data,aes(x=time,y=positive))+
  scale_y_continuous(limits=lims)+
  geom_line()+
  geom_point(colour="#009688",size=2.5)+
  labs(title="Cases",x="Date", y="Positive cases")+
  theme_light()

log.scaled <- ggplot(data,aes(x=time,y=positive))+
  scale_y_continuous(trans="log10",limits=lims,breaks=c(10,30,100,200,500,1000,2000,5000,10000,30000,60000))+
  stat_smooth(method="lm",se=F,col="black")+
  geom_point(colour="#009688",size=2.5)+
  labs(title="Cases (log scaled)",x="Date", y="Positive cases (log scaled)")+
  theme_light()

doubling.plot <- ggplot(doubling,aes(x=time,y=double.days))+
  scale_y_continuous(limits=c(0,NA))+
  geom_line()+
  geom_point(colour="#009688",size=2.5)+
  labs(title=paste("Days to double - fitted over ",day.range," days",sep=""),x="Date", y="Cases doubling rate (days)")+
  theme_light()


plot <- ggarrange(normal.scaled,log.scaled,doubling.plot,ncol=3)

figure <- annotate_figure(plot,
                          top = text_grob(paste(target.state,"state on",max(data$time)), color = "black", face = "bold", size = 14),
                          bottom = text_grob("Data source: covidtracking.com", color = "gray50",
                                             hjust = 1, x = 1, face = "italic", size = 10))

ggsave(paste(target.state," state on ",max(data$time),".pdf",sep=""),plot=figure,width=10,height=4)
