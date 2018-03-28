setwd("D:/Joulia/YandexDisk/J_R-Projects/MathMod/MathMod")


library("tidyverse") 
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
tbl=read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl=tbl[-1,] 
tbl 
tbl=tbl[tbl$DOY > 62 & tbl$DOY < 156,] 
tbl=tbl[tbl$daytime == TRUE,] 
glimpse(tbl) 
tbl = select(tbl, -(roll)) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl [,sapply (tbl,is.numeric) ] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux) 
cor_td 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula
mod=lm(formula, data = tbl)
anova(mod)
summary(mod)
formula1 = as.formula(paste("h2o_flux ~ Tau + rand_err_Tau + H + LE + rand_err_LE + h2o_flux + 
                            rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
                            co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + 
                            air_density + air_molar_volume + es + RH + VPD + u_rot + 
                            wind_speed + max_speed + u. + TKE + un_Tau + un_H + un_LE + 
                            un_h2o_flux + u_var + v_var + w_var + w.ts_cov + w.h2o_cov + 
                            co2 + co2.1 + flowrate"))
formula1
mod2=lm(formula1, data = tbl)
anova(mod2)
summary(mod2)
formula2 = as.formula(paste("h2o_flux ~ Tau + rand_err_Tau + H + LE + rand_err_LE + h2o_flux + 
                            rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
                            co2_mixing_ratio + h2o_time_lag + air_temperature + 
                            air_density + air_molar_volume + es + RH + VPD + u_rot + 
                            wind_speed + u. + TKE + un_Tau + un_H + un_h2o_flux + 
                            co2"))
mod3=lm(formula2, data = tbl)
anova(mod3)
summary(mod3)
formula3 = as.formula(paste("h2o_flux ~  H + LE + un_LE + 
                            un_h2o_flux"))
mod4=lm(formula3, data = tbl)
anova(mod4)
summary(mod4)
#взаимодействия переменных
mod5 = lm(h2o_flux ~ (H + LE) ^2 - H:un_h2o_flux - LE:un_LE - LE:un_h2o_flux - un_LE, data = tbl)
mod5
anova(mod5)
summary(mod5)
mod6 = lm(h2o_flux ~ (H + LE) - H:un_h2o_flux - LE:un_LE - LE:un_h2o_flux - H:LE - un_LE, data = tbl)
mod6
anova(mod6)
summary(mod6)
