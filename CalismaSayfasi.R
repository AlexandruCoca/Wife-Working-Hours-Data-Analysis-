library(tidyverse)

getwd()
veriseti <- read.csv("wife_working_dataset.csv",header = TRUE,sep = ',')
veriseti <- veriseti[-1]
veriseti <- veriseti[-11]

Children=select(veriseti,child5:child17)
veriseti <- data.frame(veriseti,TotalChild=rowSums(Children))






veriseti <- veriseti %>% filter(veriseti$income>0)

ortalama_calisma_suresi <- mean(veriseti$hours)


calismayan_kadinlar <- veriseti %>% filter(veriseti$hours==0)
calisan_kadinlar <- veriseti %>% filter(veriseti$hours!=0)
clsn_clsmn_oran <- nrow(calisan_kadinlar)/nrow(calismayan_kadinlar)

ggplot(veriseti,aes(x=child5,y=hours)) + geom_bar(stat="identity",aes(color=child5))+xlab("Calisma Saati")+ylab("0-5 Yas arasi Cocuk Sayisi")
ggplot(veriseti,aes(x=child5,y=hours)) + geom_bar(stat="identity",aes(fill=as.factor(child5)))+ggtitle("0-5 Yaş arası çoçuk sayısı ve çalışma saatleri")+xlab("0-5 Yas arasi Cocuk Sayisi")+ylab("Calisma Saati")+theme(legend.title=element_blank())


eduHours <- veriseti %>% group_by(education) %>% 
  summarise_at(vars(hours),funs(mean))

eduChildHour <- select(veriseti,c(education,hours,child5,child13,child17))           
eduChildHour %>% summarise_at(vars(child5:child17),funs(rowSums))

#eduChildHour <- eduChildHour %>% group_by(education) %>% 
#  summarise_at(vars(hours),funs(mean))

ggplot(eduChild,aes(x=education,y=hours))+geom_line(aes(color=3,size=0.4))



eduChild <-veriseti %>% group_by(education) %>% 
  summarise_at(vars(hours),funs(mean))


ggplot(eduChild,aes(x=education,y=TotalChild))+geom_line(aes(color=3,size=0.4))
install.packages("quantreg")
library(quantreg)

ggplot(veriseti,aes(x=TotalChild,y=hours))+geom_point(aes(color='red'))


max(veriseti$TotalChild)


library(rpart)
library(rpart.plot)


#plot(veriseti)
#kumeleme = kmeans(veriseti,3,nstart = 10)
#plot(veriseti,col=kumeleme$cluster)

calisan_kadinlar <- veriseti %>% filter(veriseti$hours!=0)
agacmodeli = rpart(hours~.,calisan_kadinlar)
rpart.plot(agacmodeli,main="Çalışma Saati Tahminine Yönelik Karar Ağacı Modeli",shadow.col = "yellow",col="blue")

agactahmini = predict(agacmodeli,calisan_kadinlar)
plot(calisan_kadinlar$hours,agactahmini)



require(data.table)
lineer_model = lm(hours~-1+.,calisan_kadinlar)
plot(calisan_kadinlar$hours, lineer_model$fitted.values, xlab = "Gercek Deger", ylab = "Tahmini Deger")
plot(lineer_model)
summary(lineer_model)

dogruluk <- lineer_model$fitted.values==calisan_kadinlar
sum(dogruluk)
data.table(tahmin=lineer_model$fitted.values,gercek=calisan_kadinlar[,1])
predict(lineer_model,veriseti[8,])
summary(agacmodeli)

