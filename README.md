# Wife-Working-Hours-Data-Analysis-

 This study was prepared on the last day of the Data Analysis training with a 36-hour R from the 20th academic conference in 2018.
 The information I have learned from this training is a simple work I have prepared.
 
 You can find the dataset and html output of the work in the files. Your expertise in data science may come as a very bad work. 
 
 Because This is my first data study :)
 
 
 This is my work ;
 
 ---
title: "ABD 'deki Kadın Çalısma Saatleri Veri Analizi"
author: "Muhammed PEKTAS - Cevher SOYLEMEZ"
---

#####Berk Orbay ve Mustafa Aydoğan'a teşekkür ederiz.
<br>

*Veriler 1995 yılında ABD'den toplanmistir.*


#HAZIRLIK

Gerekli paketlerin yuklenmesi;

```{r,warning=FALSE,message=FALSE}
#install.packages("tidyverse")
library(tidyverse)
library(rpart)
library(rpart.plot)
```
yazildigi sekildedir.



Veri setimizi kullanmak istediğimiz bir format'a cevirmek icin;
```{r,warning=FALSE}
veriseti <- read.csv("wife_working_dataset.csv",header = TRUE,sep = ',')
veriseti <- veriseti[-1]
veriseti <- veriseti[-11]
veriseti <- veriseti %>% filter(veriseti$income>0)
Children=select(veriseti,child5:child17)
veriseti <- data.frame(veriseti,TotalChild=rowSums(Children))
```

kod satırlarını kullanıyoruz..


#ANALIZ

### Bazı İstatistiksel Cikarimlar
 
Kadinların ortalama calisma saatleri asagidaki kod yardimiyla yillik *1135.47* oldugunu goruyoruz.

```{r}
ortalama_calisma_suresi <- mean(veriseti$hours)
```


Calisan kadinlarin calismayan kadinlara orani; aşagidaki kod satirlari kullanılarak *2.7575* bulunmustur.
```{r}
calismayan_kadinlar <- veriseti %>% filter(veriseti$hours==0)
calisan_kadinlar <- veriseti %>% filter(veriseti$hours!=0)
clsn_clsmn_oran <- nrow(calisan_kadinlar)/nrow(calismayan_kadinlar)
```
>Bu durumda 1995 yilinda ABD'de calisan kadinlarin calismayan kadinlardan *yaklasik 3 kat fazla* oldugunu goruyoruz.


###Grafiksel Yaklasimlar


İlk olarak kadınların yıllık calisma sureleri ile 0-5 yaş aralığındaki çocuk sayıları arasındaki ilişkiyi inceleceyelim.


*0-5 Yaş Aralığı
```{r}
ggplot(veriseti,aes(x=child5,y=hours)) + geom_bar(stat="identity",aes(fill=as.factor(child5)))+ggtitle("0-5 Yaş arası çoçuk sayısı ve çalışma saatleri")+xlab("0-5 Yas arasi Cocuk Sayisi")+ylab("Calisma Saati")+theme(legend.title=element_blank())

```

>Bu grafikten anlaşıldığı üzere kadınların 0-5 yaş arası çocuk sayıları arttıkça çalışma süreleri azalmaktadır.


Aşağıdaki kod bloğunda ise kadınların eğitim durumlarına göre ortalama çalışma saatleri gösterilmiştir.

*Eğitim - Çalışma Saati
```{r}
eduHour <-veriseti %>% group_by(education) %>% 
  summarise_at(vars(hours),funs(mean))

ggplot(eduHour,aes(x=education,y=hours))+geom_line(aes(colour="green"))
```

>Bu grafikten kadınların 0-5 yıl eğitim alanlarının ortalama çalışma saatleri değişkenlik gösterse de bu eşikten sonra eğitim süresi arttıkça ortalama çalışma saatlerinin de arttığını görüyoruz.

```{r}
ggplot(veriseti,aes(x=TotalChild,y=hours))+geom_point(aes(color='red'))
```

>Toplam çoçuk sayıları arttıkça çalışan kadınların sayısının azalmasıyla beraber ortalama çalışma saatlerinin de azaldığı görülmektedir.


###Tahminsel Yaklaşımlar
<br>

```{r}

calisan_kadinlar <- veriseti %>% filter(veriseti$hours!=0)
agacmodeli = rpart(hours~.,calisan_kadinlar)
rpart.plot(agacmodeli,main="Çalışma Saati Tahminine Yönelik Karar Ağacı Modeli",shadow.col = "yellow",col="blue")

```

Yukarıda çalışma saatinin tahminine yönelik karar ağacı modeli çizilmiştir.
<br>
>Bu modele göre kadınların 0-5 yaş arasında 1 den fazla çocuğu olan kadınların diğerlerine göre daha az sürelerde çalıştığını görebiliyoruz. Ayrıca 52 yaşından küçük olanların büyük olanlara göre nispeten daha çok çalıştıklarını söyleyebiliriz.


####Lineer Yaklaşım
```{r message=FALSE,warning=FALSE}

require(data.table)
lineer_model = lm(hours~.,calisan_kadinlar)
#plot(calisan_kadinlar$hours, lineer_model$fitted.values, xlab = "Gercek Deger", ylab = "Tahmini Deger")
plot(lineer_model$fitted.values,lineer_model$residuals)
```

>Grafiğin tahmin değerlerimizin Residuals değerinde 0'a yakın olduğu yerlerde toplanması grafiğimizin başarısını göstermektedir. 













