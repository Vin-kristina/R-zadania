library("tidyverse") 
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
tb1=read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tb1=tb1[-1,] 
tb1 
tb1=tb1[tb1$DOY > 62 & tb1$DOY < 156,] 
tb1=tb1[tb1$daytime == FALSE,] 
glimpse(tb1) 
tb1 = select(tb1, -(roll)) 
tb1 = tb1 %>% mutate_if(is.character, factor) 
names(tb1) = str_replace_all(names(tb1), "[!]","_emph_") 
names(tb1) = names(tb1) %>% 
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
glimpse(tb1) 
#Оставляем только численные данные 
sapply(tb1,is,numeric) 
tb1_numeric = tb1 [,sapply (tb1,is.numeric) ] 
tb1_non_numeric = tb1[,!sapply(tb1,is.numeric) ] 
cor_tb1 = cor(drop_na(tb1_numeric)) 
cor_tb1 
cor_tb1 = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(h2o_flux) 
cor_tb1 
vars = row.names(cor_tb1)[cor_tb1$h2o_flux^2 > .2] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
teaching_tb1 = sample_n(tb1, floor(length(tb1$date)*.7)) 
testing_tb1 = sample_n(tb1, floor(length(tb1$date)*.3)) 
formula