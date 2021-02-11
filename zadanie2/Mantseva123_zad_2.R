#Манцева Т.Э. ПАЭ 123 
#создайте модель множественной линейной регрессии ночных потоков углекислого газа за период 2013 года 
#по данным измерений методом турбулентной пульсации
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")  
library("readr")

#Загрузка и редакция исходных данных
eddypro = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
eddypro = eddypro[-1, ]
eddypro = select(eddypro, -(roll))
eddypro = eddypro %>% mutate_if(is.character, factor)

#Переименовка столбцов для корректных названий для переменных
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_exclam_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

#Подгон данных под условия задания
eddypro = drop_na(eddypro)
eddypro = filter(eddypro, daytime==F)
eddy_num = eddypro[,sapply(eddypro,is.numeric)]
eddy_nonum = eddypro[,!sapply(eddypro,is.numeric)]

#Создание выборок
row_numbers = 1:length(eddy_num$co2_flux)
teach = sample(row_numbers, floor(length(eddy_num$co2_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddy_num[teach,]
testing_tbl = eddy_num[test,]

#Модель 1 по обучающей выборке
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)
summary(mod1)
coef(mod1)
anova(mod1)
plot(mod1)

#Модель 2
mod2 = lm ( co2_flux~  DOY + file_records + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux 
            + rand_err_co2_flux + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew
            + u_unrot + w_rot + max_speed + wind_dir + yaw + pitch + u. + TKE + L + X.z.d..L + bowen_ratio + T. + x_peak + x_offset 
            + x_10. + x_30. + x_50. +x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf +un_co2_flux + un_h2o_flux + v_spikes 
            + w_spikes + mean_value + ts_var + co2_var + w.co2_cov + w.h2o_cov + co2 + h2o.1 + h2o_signal_strength_7200 + flowrate , data = teaching_tbl)
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2) 

#Модель 3
mod3 = lm ( co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux 
            + rand_err_co2_flux + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew
            + u_unrot + w_rot + wind_dir + yaw + pitch + u. + TKE + X.z.d..L + bowen_ratio + T. + x_peak + x_offset 
            + x_10. + x_30. + x_50. +x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf +un_co2_flux + un_h2o_flux
            + w_spikes + mean_value + co2_var + w.co2_cov + w.h2o_cov + co2 ,data = teaching_tbl)
summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3) 

#Построение точек по значениями обучающей и тестирующей выборки и наложение предсказанных значений по 3 модели
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))