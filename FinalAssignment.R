##Reproducible data, final assignment.  https://www.coursera.org/learn/reproducible-research/peer/OMZ37/course-project-2
##our data analysis must address the following questions:

    #Across the United States, which types of events (as indicated in the EVTYPE
    #variable) are most harmful with respect to population health?
    #Across the United States, which types of events have the greatest economic 
    #consequences?

setwd("C:/Users/JKK/Assignments/reproducible data")
library(RCurl)
library(R.utils)
library(dplyr)
StormData<- bunzip2("StormData.csv.bz2", "StormData.csv", remove = FALSE, skip = TRUE)
StormData<-read.csv(StormData, stringsAsFactors = FALSE)
str(StormData)
head(StormData)
summary(StormData)
names(StormData)

StormData$BGN_DATE<-as.Date(StormData$BGN_DATE, format = "%m/%d/%Y")
TrimStormData<-StormData[StormData$FATALITIES > 0 & StormData$INJURIES > 0 &
                         StormData$BGN_DATE>"1991-11-28",
                         c('EVTYPE','FATALITIES','INJURIES')]
head(TrimStormData)

EventType<-aggregate(.~EVTYPE, TrimStormData, sum)
EventTypeFatalities<-EventType[order(EventType$FATALITIES, decreasing = TRUE),]
head(EventTypeFatalities)
EventTypeInjuries<-EventType[order(EventType$INJURIES, decreasing = TRUE),]
head(EventTypeInjuries)

StormDataEcon <- StormData[StormData$PROPDMG > 0 & StormData$CROPDMG > 0, c('EVTYPE','PROPDMG','PROPDMGEXP','CROPDMG','CROPDMGEXP')]
StormDataEcon <- na.omit(StormDataEcon)
head(StormDataEcon)

StormDataEcon1 <- subset(StormDataEcon,as.numeric(StormDataEcon$PROPDMG)!=0) 
y<- length(StormDataEcon1$PROPDMG)
x<- 1

while(x<=y){
    StormDataEcon1$PropertyDamage[[x]] <- as.numeric(StormDataEcon1$PROPDMG[[x]])*
        if (toupper(StormDataEcon1$PROPDMGEXP[[x]])=="B") 100000000 else 
        if (toupper(StormDataEcon1$PROPDMGEXP[[x]])=="M") 1000000 else 
        if (toupper(StormDataEcon1$PROPDMGEXP[[x]])=="K") 1000 else 
        if (toupper(StormDataEcon1$PROPDMGEXP[[x]])=="H") 100 else 
        if (StormDataEcon1$PROPDMGEXP[[x]]=="-" || StormDataEcon1$PROPDMGEXP[[x]]=="+") 0 else 
        if (StormDataEcon1$PROPDMGEXP[[x]]=="2") 100 else 
        if (StormDataEcon1$PROPDMGEXP[[x]]=="3") 1000 else   
        if (StormDataEcon1$PROPDMGEXP[[x]]=="4") 10000 else  
        if (StormDataEcon1$PROPDMGEXP[[x]]=="5") 100000 else
        if (StormDataEcon1$PROPDMGEXP[[x]]=="6") 1000000 else 
        if (StormDataEcon1$PROPDMGEXP[[x]]=="7") 10000000 else 1
        x<-x+1}


StormDataEcon1 <- data.frame(Event = StormDataEcon1$EVTYPE,PropertyDamage = StormDataEcon1$PropertyDamage)
StormDataEcon2 <- aggregate(PropertyDamage ~ Event,StormDataEcon1,sum)
StormDataEcon3 <- arrange(StormDataEcon2,desc(PropertyDamage))
StormDataEcon4<-head(StormDataEcon3)

library(dplyr)
str(StormDataEcon1)
head(StormDataEcon3)
tail(StormDataEcon3)
head(StormDataEcon1)
names(StormDataEcon3)
summarise(StormDataEcon3)
summary(StormDataEcon3)
library(ggplot2)
head(StormDataEcon2)

StormDataCrop <- subset(StormDataEcon,as.numeric(StormDataEcon$CROPDMG)!=0) 
y<- length(StormDataCrop$CROPDMG)
x<- 1

while(x<=y){
    StormDataCrop$CropDamage[[x]] <- as.numeric(StormDataCrop$CROPDMG[[x]])*
        if (toupper(StormDataCrop$CROPDMGEXP[[x]])=="B") 100000000 else 
        if (toupper(StormDataCrop$CROPDMGEXP[[x]])=="M") 1000000 else 
        if (toupper(StormDataCrop$CROPDMGEXP[[x]])=="K") 1000 else 
        if (toupper(StormDataCrop$CROPDMGEXP[[x]])=="H") 100 else 
        if (StormDataCrop$CROPDMGEXP[[x]]=="-" || StormDataCrop$CROPDMGEXP[[x]]=="+") 0 else 
        if (StormDataCrop$CROPDMGEXP[[x]]=="2") 100 else 
        if (StormDataCrop$CROPDMGEXP[[x]]=="3") 1000 else   
        if (StormDataCrop$CROPDMGEXP[[x]]=="4") 10000 else  
        if (StormDataCrop$CROPDMGEXP[[x]]=="5") 100000 else
        if (StormDataCrop$CROPDMGEXP[[x]]=="6") 1000000 else 
        if (StormDataCrop$CROPDMGEXP[[x]]=="7") 10000000 else 1
    x<-x+1}


StormDataCrop <- data.frame(Event = StormDataCrop$EVTYPE,CropDamage = StormDataCrop$CropDamage)
StormDataCrop1 <- aggregate(CropDamage~ Event,StormDataCrop,sum)
StormDataCrop2 <- arrange(StormDataCrop1,desc(CropDamage))
StormDataCrop3<-head(StormDataCrop2)

#Plot StormDataEcon4 and StormDataCrop3
PlotEcon<-ggplot(StormDataEcon4, aes(x = Event, y = PropertyDamage/1000000000)) + 
    geom_bar(stat = "identity", fill ="navy")+ylab("Propery Damage $ (billions)")+
    ggtitle(expression(atop("Weather Events that cause Most Property Damage", atop(italic("1991 - 2011")))))+
    theme(axis.text.x =
              element_text(size  = 10,
                           angle = 45,
                           hjust = 1,
                           vjust = 1))+
    theme(panel.background = element_blank(),
          legend.key=element_blank())
print(PlotEcon)

PlotCrop<-ggplot(StormDataCrop3, aes(x = Event, y = CropDamage/100000000)) + 
    geom_bar(stat = "identity", fill ="dark green")+ylab("Crop Damage $ (billions)")+
    ggtitle(expression(atop("Weather Events that cause Most Crop Damage", atop(italic("1991 - 2011")))))+
    theme(axis.text.x =
              element_text(size  = 10,
                           angle = 45,
                           hjust = 1,
                           vjust = 1))+
    theme(panel.background = element_blank(),
          legend.key=element_blank())
print(PlotCrop)
