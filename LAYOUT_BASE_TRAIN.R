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

train <- read_delim("train.txt", "\t", escape_double = FALSE, 
                    col_types = cols(dropoff_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     pickup_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                    trim_ws = TRUE)
 

#Tratando as variaveis pickup_datetime  e dropoff_datetime 
# pickup_datetime
train <- train %>% mutate(hr_up_int = hour(train$pickup_datetime)) %>% 
  mutate(minuto_up_int = (minute(train$pickup_datetime))/60) %>% 
  mutate(horas_up = hr_up_int + minuto_up_int) %>% 
  mutate(dia_up = wday(train$pickup_datetime))%>% 
  mutate(mes_up = month(train$pickup_datetime))


# dropoff_datetime
train <- train %>% mutate(hr_off_int = hour(train$dropoff_datetime)) %>% 
  mutate(minuto_off_int = (minute(train$dropoff_datetime))/60) %>% 
  mutate(horas_off = hr_off_int + minuto_off_int) %>% 
  mutate(dia_off = wday(train$dropoff_datetime))%>% 
  mutate(mes_off = month(train$dropoff_datetime))


## Levels: Sun1 < Mon2 < Tues3 < Wed4 < Thurs5 < Fri6 < Sat7

#### criando variavel a partir da distancia euclidiana de lat long

distancia <- ((train$pickup_latitude - train$dropoff_latitude)^2 + 
              (train$pickup_longitude - train$dropoff_longitude)^2)

summary(distancia)

train <- train %>% 
  mutate(distancia = distancia)


#-------------- store_and_fwd_flag
# N = 1
# Y = 2
train = train %>% mutate(store_and_fwd_flag = ifelse(.$store_and_fwd_flag == "N", 1,2))


#------------ trip duration

train <- train %>%
  
  mutate(trip_duration = log(trip_duration + 1))

####----------------- selecionando as variaveis que vou utilizar

train <- train %>% dplyr::select(vendor_id, passenger_count,
                                 pickup_longitude,pickup_latitude,
                                 dropoff_longitude, dropoff_latitude,
                                 store_and_fwd_flag, trip_duration,
                                 horas_up, dia_up, mes_up, 
                                 horas_off, dia_off, mes_off, distancia)
                                 


glimpse(train)


####------- divide-se a base de train em trn = 80% e validacao = 20%

train <- train %>% 
  mutate(flag = sample(0:1, n(), replace = T, prob=c(0.8, 0.2)))


train_80 <- train %>%  filter(flag == 0)  #Base de treinamento com 80% da base original

val_20 <- train %>%  filter(flag==1) #Base de Validação com 20% da base original

