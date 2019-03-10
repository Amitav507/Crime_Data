library(plyr)
library(dplyr)
library(plyr)
library(ggplot2)
setwd('G:/Ana_pro/R_script/crim')
crime<-read.csv(file='crime.csv',sep=',',header=T)
View(crime)
str(crime)
typeofcrime<-count(crime,Category)
View(typeofcrime)
#kpi1
ggplot(typeofcrime, aes(fill=typeofcrime$Category, y=typeofcrime$n , x=typeofcrime$Category)) +
  geom_bar(stat="identity")+ xlab('Types of crime') + ylab("Number of incidents")+
  labs(title  ="Crime Data 2003-2016", subtitle = "(Count based on types)", fill="Category") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#kpi2
crime_per<-typeofcrime
x<-sum(typeofcrime$n)
crime_per$n=round(((crime_per$n)/x)*100,digits = 4)
View(crime_per)
colnames(crime_per)<-c("Type of crime","Percentage")
write_xlsx(crime_per, path = "KPI.xlsx")
#kpi3
dayofcrime<-count(crime,DayOfWeek)
View(dayofcrime)
pie3D(dayofcrime$n,labels =paste(dayofcrime$DayOfWeek,'(',dayofcrime$n,')',sep=' '),
      main="Crime on each day of Week" ,col =rainbow(length(dayofcrime$DayOfWeek)))
#kpi4

crime$date<-as.Date(crime$Date,'%m/%d/%Y')
#View(crime)
is.Date(crime$date)
#crime$date1<-parse_date_time(x =crime$Date,orders = c("m d y","%m/%d/%Y"))
crime$year<-format(crime$date,'%Y')
View(crime)
crimeyear<-count(crime,year)
View(crimeyear)

#kpi5
districtofcrime<-group_by(crime,PdDistrict,DayOfWeek)
View(districtofcrime)
disofcrime<-summarise(districtofcrime,count=n())
View(disofcrime)
ggplot(disofcrime, aes(fill=disofcrime$DayOfWeek , y=disofcrime$count, x=disofcrime$PdDistrict)) +
  geom_bar(stat="identity") + xlab('Location of crime') + ylab("Number of incidents")+
  labs(title  ="Crime Data 2003-2016", subtitle = "(Count based on Location)", 
       fill="Day of week")

#kpi6

#kpi7
crime$time<-strptime(crime$Time,format='%H:%M')
crime$time<-format(crime$time,'%H%M')
View(crime)
is.timespan(crime$time)
str(crime)
crime$time<-as.integer(crime$time)
#crime$timespan<-ifelse(crime$time<=600,0,ifelse(crime$time<=1200,1,ifelse(crime$time<=1800,2,3)))
crime$timespan<-ifelse(crime$time<600,'12AM to 6AM',ifelse(crime$time<1200,'6AM to 12PM',ifelse(crime$time<1800,'12PM to 6PM','6PM to 12AM')))
View(crime)
dftimespan<-count(crime,timespan)
View(dftimespan)
#pie(dftimespan$freq,labels = paste(dftimespan$timespan,"(",dftimespan$freq,')','[',round(100*(dftimespan$freq/sum(dftimespan$freq))),'%',']'),sep='',col=rainbow(length(dftimespan$timespan)))
par(mai = c(0,0,0,0))
layout(c(1,2),heights=c(0.3,1))
plot.new()
plot.new()
pie(dftimespan$n,labels = paste('[',round(100*(dftimespan$n/sum(dftimespan$n)),2),'%',']',sep=''),
                          col=rainbow(length(dftimespan$timespan)),
    )
legend('right',as.vector(paste(dftimespan$timespan,"(",dftimespan$n,")"))
       ,cex=0.8,fill=rainbow(length(dftimespan$n)))

