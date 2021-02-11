#Манцева Тамара – для региона 63 Самарская область
#для региона 63 рассчитайте урожайность пшеницы в 2005 году,
#взяв для рассчета средние суммы активных температур за предыдущие 15 лет, с 19 ближайших метеостанций

#Активация библиотек и ввод исходных данных
library (tidyverse);library (rnoaa);library(lubridate)
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y1=1.0;Kf=300;Qj=1600;Lj=2.2;Ej=25 
#Скачивание данных и создание из них датафрейма с информацией по 19 станциям 
station_data = read.csv("station_data.csv")
samara = data.frame(id = "Samara", latitude = 53.25,  longitude = 50.2167)
sam_around = meteo_nearby_stations(lat_lon_df = samara, station_data = station_data, limit = 19, var = c("TAVG"), year_min = 1990, year_max = 2004)
sam_id = sam_around[["Samara"]][["id"]][1]
sam_table = data.frame (sam_around)
all_pre_sam = data.frame()
all_sam_meteodata = data.frame()
for(i in 1:19)
{
  all_pre_sam=meteo_tidy_ghcnd(stationid =sam_id, var="TAVG", date_min = "1990-01-01", date_max = "2004-12-31") 
  all_sam_meteodata=rbind(all_sam_meteodata, all_pre_sam)
}
write.csv (all_sam_meteodata,"all_sam_meteodata.csv")
#Создание датафрейма с данными за нужный период времени 
all_sam_meteodata=read.csv("all_sam_meteodata.csv") 
all_sam_meteodata [,"year"]= year(all_sam_meteodata$date)
all_sam_meteodata [,"month"]= month(all_sam_meteodata$date) 
all_sam_meteodata [,"day_of_the_year"]= yday(all_vor_meteodata$date) 
y_sam_met= filter(all_sam_meteodata,year>1989 & year<2005)
#подсчет средних активных температур по месяцам за нужный период времени
y_sam_met[,"tavg"]= y_sam_met$tavg/10
y_sam_met [is.na(y_sam_met$tavg), "tavg"] = 0
y_sam_met [y_sam_met$tavg<5, "tavg"] = 0
alldays=group_by(y_sam_met,id,year,month)
sum_t=summarize(alldays, tsum = sum(tavg))
group_months=group_by(sum_t,month)
sum_t_months=summarize(group_months , St = mean(tsum))
#Рассчет урожая по формуле
sum_t_months = mutate(sum_t_months, Fi = afi+bfi*y1*St)
sum_t_months = mutate(sum_t_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
Yield = sum(sum_t_months$Yi)
Yield