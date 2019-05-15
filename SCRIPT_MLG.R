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


#### NO RUN ############################################################

train_full <- read.csv(file="C:\\Users\\rkms\\Documents\\analise\\Em_att\\Dados_originais\\train.csv", header=TRUE, sep=",")
#amostra de 10%
train <- train_full %>% sample_frac(0.1)

caminho = "train.txt"
write.table(train, file = caminho, na = "", sep = "\t", 
            row.names = FALSE, quote = FALSE)


test_full <- read.csv(file="C:\\Users\\rkms\\Documents\\analise\\Em_att\\Dados_originais\\test.csv", header=TRUE, sep=",")
#amostra de 10%
test <- test_full %>% sample_frac(0.1)

caminho = "test.txt"
write.table(test, file = caminho, na = "", sep = "\t", 
            row.names = FALSE, quote = FALSE)

#### NO RUN ############################################################



############# MODELAGEM
# VER https://lamfo-unb.github.io/2018/09/29/MLG/

# Gama
# fit1 <- gamlss(formula = trip_duration ~ factor(vendor_id) + 
#                  factor(passenger_count) + horas_up + dia_up +
#                  mes_up + horas_off + dia_off + mes_off,
#                family = GG(mu.link = "log"), data = train_80)
# predicao1 = predict(fit1, type="response", newdata = val_20)
# 
# fit2 <- gamlss(formula = trip_duration ~ factor(vendor_id) + 
#                  factor(passenger_count) + horas_up + dia_up +
#                  mes_up + horas_off + dia_off + mes_off,
#                family = GG(mu.link = "inverse"), data = train_80)
# predicao2 = predict(fit2, type="response", newdata = val_20)
# 
# 
# fit3 <- gamlss(formula = trip_duration ~ factor(vendor_id) + 
#                  factor(passenger_count) + horas_up + dia_up +
#                  mes_up + horas_off + dia_off + mes_off,
#                family = GG(mu.link = "identity"), data = train_80)
# predicao3 = predict(fit3, type="response", newdata = val_20)


# normal inversa
fit4 <- gamlss(formula = trip_duration ~ factor(vendor_id) + 
                 factor(passenger_count) + horas_up + dia_up +
                 mes_up + horas_off + dia_off + mes_off,
               family = IG(mu.link = "log", sigma.link = "log"), 
               data = train_80)
predicao4 = predict(fit4, type="response", newdata = val_20)


# poisson
fit5 <- gamlss(formula = trip_duration ~ factor(vendor_id) + 
                 factor(passenger_count) + horas_up + dia_up +
                 mes_up + horas_off + dia_off + mes_off,
               family = PO(mu.link = "log"), data = train_80)
predicao5 = predict(fit5, type="response", newdata = val_20)


### AJUSTE LINEAR SIMPLES
fit6 <- lm(formula = trip_duration ~ factor(vendor_id) + 
             factor(passenger_count) + horas_up + dia_up +
             mes_up + horas_off + dia_off + mes_off, data = train_80)
summary(ajuste1)
predicao6 = predict(ajuste6, type="response", newdata = val_20)


# melhor modelo eh aquele com menor erro quadratico medio

# eqm1 = mean((val_20$trip_duration - predicao1)^2)
# 
# eqm2 = mean((val_20$trip_duration - predicao2)^2)
# 
# eqm3 = mean((val_20$trip_duration - predicao3)^2)

eqm4 = mean((val_20$trip_duration - predicao4)^2)

eqm5 = mean((val_20$trip_duration - predicao5)^2)

eqm6 = mean((val_20$trip_duration - predicao6)^2)





