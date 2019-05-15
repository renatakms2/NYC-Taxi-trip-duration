library(feather)
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(gamlss)
library(MASS)
library(glm2)
library(readr)
library(leaflet)
library(stringr)

setwd("~/Arquivos_Analise2")

test <- read_delim("test.txt", "\t", escape_double = FALSE, 
                   col_types = cols(pickup_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                   trim_ws = TRUE)
glimpse(test)

# NAO TEM dropoff_datetime 
#Tratando as variaveis pickup_datetime  e dropoff_datetime 
# pickup_datetime
test <- test %>% mutate(hr_up_int = hour(test$pickup_datetime)) %>% 
  mutate(minuto_up_int = (minute(test$pickup_datetime))/60) %>% 
  mutate(horas_up = hr_up_int + minuto_up_int) %>% 
  mutate(dia_up = wday(test$pickup_datetime))%>% 
  mutate(mes_up = month(test$pickup_datetime))


# dropoff_datetime
# NAO TEM ESSA VARIÁVEL NA BASE DE TESTE (??????)
test <- test %>% mutate(hr_off_int = hour(test$dropoff_datetime)) %>% 
  mutate(minuto_off_int = (minute(test$dropoff_datetime))/60) %>% 
  mutate(horas_off = hr_off_int + minuto_off_int) %>% 
  mutate(dia_off = wday(test$dropoff_datetime))%>% 
  mutate(mes_off = month(test$dropoff_datetime))


#### criando variavel a partir da distancia euclidiana de lat long

distancia <- ((test$pickup_latitude - test$dropoff_latitude)^2 + 
                (test$pickup_longitude - test$dropoff_longitude)^2)

summary(distancia)

test <- test %>% 
  mutate(distancia = distancia)

#---------------- store_and_fwd_flag
# N = 1
# Y = 2
test = test %>% mutate(store_and_fwd_flag = ifelse(.$store_and_fwd_flag == "N", 1,2))


####----------------- selecionando as variaveis que vou utilizar

test <- test %>% dplyr::select(vendor_id, passenger_count,
                                 pickup_longitude,pickup_latitude,
                                 dropoff_longitude, dropoff_latitude,
                                 store_and_fwd_flag,
                                 horas_up, dia_up, mes_up, distancia)
                                 

