install.packages("ggplot2")
library(plyr)
library(ggplot2)
library(reshape2)

vehicles<-read.csv("D:/car_data_ana/vehicles.csv",stringsAsFactors=FALSE)
x<-readLines("D:/car_data_ana/varlabels.txt")
y<-strsplit(x," - ")
labels<-do.call(rbind,y)  #y里所有元素执行rbind
length(unique(vehicles$year))    #包含35年的数据
first_year<-min(vehicles[,"year"])   #1984
last_year<-max(vehicles[,"year"])    #2018(model year)

#燃料类型
table(vehicles$fuelType1)
# Diesel（柴油）       Electricity（电动）  Midgrade Gasoline（中等汽油）    
#1016                  137                  77                                   
#Natural Gas（天然气）  Premium Gasoline（优质汽油）
#60                     10311
#Regular Gasoline (普通汽油)
#26539

#传动方式
vehicles$trany[vehicles$trany==" "]<-NA
vehicles$trany2<-ifelse(substr(vehicles$trany,1,4)=="Auto","Auto","Manual")
table(vehicles$trany2)
#Auto Manual 
#25701  12439 

attach(vehicles)

#探索平均MPG在时间序列上的变化
mpgByYr<-ddply(vehicles,~year,summarise,avgMPG=     #按年份整合，对每个组计算highway,city,combine燃油效率
                 mean(comb08),avgHghy=mean(highway08),avgCity=
                 mean(city08))
#绘图
ggplot(mpgByYr,aes(year,avgMPG))+geom_point()+    #添加散点、平滑均值、阴影
  geom_smooth()+xlab("YEAR")+ylab("Average MPG")+
  labs(title="All Cars")+
  theme(plot.title = element_text(hjust = 0.5))   #使标题居中

#只观察燃油汽车
gasCars<-subset(vehicles,fuelType1 %in% c("Regular Gasoline",
                                          "Premium Gasoline","Midgrade Gasoline")&
                  fuelType2==""&atvType!="Hybrid")
mpgByYr_Gas<-ddply(gasCars,~year,summarise,avgMPG=mean(comb08))
ggplot(mpgByYr_Gas,aes(year,avgMPG))+geom_point()+
  geom_point()++xlab("YEAR")+ylab("Average MPG")+
  labs(title="Gasoline Cars")+
  theme(plot.title = element_text(hjust = 0.5))

#验证是否大马力的车效率低，displ表示引擎排量
typeof(vehicles$displ)  #double
ggplot(gasCars,aes(displ,comb08))+geom_point()+geom_smooth()

#平均引擎排量与年份的关系
avgCarSize<-ddply(gasCars,~year,summarise,avgDispl=mean(displ))
ggplot(avgCarSize,aes(year,avgDispl))+geom_point()+    
  geom_smooth()+xlab("YEAR")+ylab("Average engine displacement")

#每年的平均燃油效率和平均引擎排量
byYear<-ddply(gasCars,~year,summarise,avgMPG=mean(comb08),avgDispl=mean(displ))
byYear2<-melt(byYear,id="year")
levels(byYear2$variable)<-c("Average MPG","Avg engine displacement")
ggplot(byYear2,aes(year,value))+geom_point()+
  geom_smooth()+facet_wrap(~variable,ncol = 1,scales = "free_y")+  #分面
  xlab("Year")+ylab("")

#4缸发动机的车在不同传动方式下的MPG
gasCars4<-subset(gasCars,cylinders=="4")
ggplot(gasCars4,aes(factor(year),comb08))+geom_boxplot()+
  facet_wrap(~trany2,ncol = 1)+theme(axis.text.x = element_text(angle = 45))+
  labs(x="Year",y="MPG")

#每年手动挡占比
ggplot(gasCars4,aes(factor(year),fill=factor(trany2)))+
  geom_bar(position = "fill")+labs(x="Year",y="Proportion of cars",
                                   fill="Transmission")+theme(axis.text.x = element_text(angle = 45))+
  geom_hline(yintercept = 0.5,linetype=2)  #when x=0,y=0.5

#四缸车的车型与厂商频次
carsMake<-ddply(gasCars4,~year,summarise,numberOfMakes=length(unique(make)))
ggplot(carsMake,aes(year,numberOfMakes))+geom_point()+
  labs(x="Year",y="Number of available Makes",title="Four cylinder cars")+
  theme(plot.title = element_text(hjust = 0.5))

#每年的厂商
uniqMakes<-dlply(gasCars4,~year,function(x)unique(x$make))
commonMakes<-Reduce(intersect,uniqMakes)
commonMakes  #chevrolet
uniqMakes

#剔除2018
uniqMakes$`2018`<-NA
uniqMakes2<-dlply(gasCars4,~year,function(x)unique(x$make))
commonMakes2<-Reduce(intersect,uniqMakes2)
commonMakes2
