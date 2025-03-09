getwd()
data<-read.csv("data.csv")
data<-subset(data,Year==2024)
head(data)

happinesshightraffic<-subset(data, Traffic_Density== "High")$Happiness_Score
happinesshightraffic
happinesstl
hist(happinessmediumtraffic)
happinessmediumtraffic<-subset(data, Traffic_Density=="Medium")$Happiness_Score
hist(happinesshightraffic)
length(happinesshightraffic)
length(happinessmediumtraffic)

median(happinesshightraffic)
median(happinessmediumtraffic)

library(dplyr)
wilcox.test(happinesshightraffic,happinessmediumtraffic)

filtered<-subset(data,Traffic_Density=="Medium"|Traffic_Density=="High")

library("ggpubr")
ggplot(filtered, aes(x = Traffic_Density, y = Happiness_Score)) +
  geom_boxplot() +
  labs(title = "Boxplot of Happiness Scores by Traffic Density",
       x = "Traffic Density",
       y = "Happiness Score")

hist(data$Healthcare_Index)
plot(data$Happiness_Score,data$Healthcare_Index)
ggplot(groupeddata2,aes(group=Healthcare,x=Healthcare,y=Happiness_Score))+
  geom_boxplot()
groupeddata2$Healthcare <- factor(groupeddata2$Healthcare, levels = unique(groupeddata2$Healthcare), ordered = TRUE)
groupeddata2$Healthcare<-as.character(groupeddata2$Healthcare)
mean(data$Happiness_Score)
mean(data$Healthcare_Index)
groupeddata2<- data %>% 
  arrange(Healthcare_Index) %>% 
  mutate(Healthcare=ntile(Healthcare_Index,3))

groupeddata2[,11]<-as.factor(groupeddata2[,11])

groupeddata<- data %>% 
  arrange(Cost_of_Living_Index) %>% 
  mutate(LivingCost=ntile(Cost_of_Living_Index,3))
groupeddata %>% 
  count(LivingCost)
groupeddata[,9]<-as.factor(groupeddata[,9])
aov<-aov(Happiness_Score~Healthcare,data=groupeddata2)
summary(aov)
cost3<-subset(groupeddata,LivingCost=="3")

var.test(Happiness_Score~LivingCost,data=cost1)
bartlett.test(Happiness_Score ~ interaction(LivingCost=="1",LivingCost=="2",LivingCost=="3"), data=groupeddata)
hist(cost3$Happiness_Score)
bartlett.test(Happiness_Score ~ interaction(Healthcare=="1",Healthcare=="2",Healthcare=="3"), data=groupeddata2)

library(multcomp)
install.packages("multcomp")
mc<-glht(aov,linfct=mcp(Healthcare="Tukey"))
summary(mc)
