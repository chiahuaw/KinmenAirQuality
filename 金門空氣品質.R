library(dplyr)
library(ggplot2)
library(data.table)

#### 1 AQI hours data #### 空氣品質指標(AQI)(歷史資料) - 資料集 - 行政院環保署EPA

f.list = list.files("~/GoogleDrive/Data/AQI/csv")

# if (Sys.info()[[1]]=="Windows") {
#   #Windows下才需要執行以下二行
#   Sys.setlocale()
#   Sys.setlocale(category = "LC_ALL",locale="C")
# }

df = data.frame()
for (i in 1:length(f.list)) {
  
  Temp = fread(paste0("~/GoogleDrive/Data/AQI/csv/",f.list[i]),encoding = "UTF-8")
  Temp = filter(Temp,County=="金門縣")
  
  
  df = rbind(df,Temp)
  
  rm(Temp)
}

# if (Sys.info()[[1]]=="Windows") {
#   #Windows下才需要執行以下一行
#   Sys.setlocale(category = "LC_ALL",locale="")
# }

df = mutate(df,SO2=as.numeric(SO2),CO=as.numeric(CO),O3=as.numeric(O3),O3_8hr=as.numeric(O3_8hr),PM10=as.numeric(PM10),
            PM2.5=as.numeric(PM2.5),NO2=as.numeric(NO2),NOx=as.numeric(NOx),NO=as.numeric(NO),WindSpeed=as.numeric(WindSpeed),
            WindDirec=as.numeric(WindDirec),DataCreationDate=as.POSIXct(DataCreationDate),
            year=as.numeric(format(DataCreationDate,format="%Y")),month=as.numeric(format(DataCreationDate,format="%m")),date=as.Date(DataCreationDate),hour=as.numeric(format(DataCreationDate,format="%H")),week=as.numeric(format(DataCreationDate,format="%w")))

df.work = filter(df,date>=as.Date("2020-09-01")&date<=as.Date("2022-08-13"))

#### 2 環境空氣品質檢驗結果（月） #### 行政院環境保護署公務統計「環境空氣品質檢驗結果」

df2 = fread("~/Git/KinmenAirQuality/環境空氣品質檢驗結果_all.csv",encoding = "UTF-8")

#### 3 組合1和2 （月） ####


df3 = mutate(df,date=paste0(format(DataCreationDate,format="%Y-%m"),"-15"))
df3 = filter(df3,!is.na(PM10),!is.na(PM2.5),!is.na(WindDirec))
df3 = summarise(group_by(df3,date),PM10_m=round(mean(PM10),1),PM25_m=round(mean(PM2.5),1),WindDirection=round(mean(WindDirec),0)) %>% 
  as.data.frame()

df3$TSP = 0
df3$dustfall = 0

for (i in 1:nrow(df3)) {
  if (df3$date[i] %in% as.character(df2$DataCreationDate)) {
    df3$TSP[i] = df2$TSP[df2$city=="金門縣"&df2$DataCreationDate==df3$date[i]]
    df3$dustfall[i] = df2$dustfall[df2$city=="金門縣"&df2$DataCreationDate==df3$date[i]]
  }
}

df3 = filter(df3,(TSP!=0)&(dustfall!=0))
df3 = mutate(df3,date=as.Date(date),year=format(date,format="%Y"))

#### plot ####

ggplot()+
  stat_smooth(data=df,aes(x=DataCreationDate,y=PM10))+
  stat_smooth(data=df,aes(x=DataCreationDate,y=PM2.5),col="red")+
  ylab("PM10(blue)/PM2.5(red)")+
  ggtitle("2017至2021年6月金門縣PM10及PM2.5監測數值迴歸圖")

ggplot()+
  stat_smooth(data=df,aes(x=hour,y=PM10))+
  stat_smooth(data=df,aes(x=hour,y=PM2.5),col="red")+
  ylab("PM10(blue)/PM2.5(red)")+
  ggtitle("2017至2021年6月金門縣PM10及PM2.5監測數值迴歸圖（小時）")

ggplot()+
  stat_smooth(data=df,aes(x=month,y=PM10))+
  stat_smooth(data=df,aes(x=month,y=PM2.5),col="red")+
  ylab("PM10(blue)/PM2.5(red)")+
  scale_x_continuous(breaks =c(1:12))+
  ggtitle("2017至2021年6月金門縣PM10及PM2.5監測數值迴歸圖（月）")

ggplot()+
  stat_smooth(data=df,aes(x=WindDirec,y=PM10))+
  stat_smooth(data=df,aes(x=WindDirec,y=PM2.5),col="red")+
  coord_polar(start=pi/2)+
  ylab("PM10(blue)/PM2.5(red)")+
  ggtitle("2017至2021年6月金門縣PM10及PM2.5監測數值迴歸圖（風向）")+
  facet_wrap(~year)

ggplot()+
  stat_smooth(data=df2[df2$city!="金門縣",],aes(x=DataCreationDate,y=dustfall,col=city),se=F)+
  stat_smooth(data=df2[df2$city=="金門縣",],aes(x=DataCreationDate,y=dustfall,col="kinmen",size=1.2))+
  ggtitle("全國每月落塵量線性迴歸圖")

ggplot()+
  stat_smooth(data=df3,aes(x=date,y=PM10_m,col="PM10"))+
  stat_smooth(data=df3,aes(x=date,y=PM25_m,col="PM2.5"))+
  stat_smooth(data=df3,aes(x=date,y=TSP,col="TSP"))+
  stat_smooth(data=df3,aes(x=date,y=dustfall,col="Dustfall"))+
  ylab("number")+
  ggtitle("2017年至2021年5月金門縣PM10、PM2.5、TSP、Dustfall監測數值迴歸圖（月）")

ggplot()+
  stat_smooth(data=df3,aes(x=WindDirection,y=TSP,col="TSP"))+
  stat_smooth(data=df3,aes(x=WindDirection,y=dustfall,col="Dustfall"))+
  coord_polar(start=pi/2)+
  ylab("PM10(blue)/PM2.5(red)")+
  ggtitle("2017年至2021年5月金門縣TSP及Dustfall監測數值迴歸圖（風向）")+
  facet_wrap(~year)
  
