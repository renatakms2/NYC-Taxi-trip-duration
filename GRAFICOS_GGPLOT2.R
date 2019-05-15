library(ggplot2)
library(grid)
library(dplyr)

# VER EM
# https://www.kaggleusercontent.com/kf/10126769/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..W03iNRBUdZythUeU_ZFlVQ.CFtq2ws-lsCeDVhyKnH6s7Py4iWWN2IO5r4vy7casg07MgPY7EFlBTlFP0IgEasObx2ut-i2eZYfN0isylW9PVIG_wJYfo-X3Owrsh5Y-soVP2hGL-R7hnfSG3fzhCD_k67cAPj_EQfi0LdIe03nv5DIbmzNcVsoYw3ImipaeVIPBAhLvsa0YLOn2dD23Fky.B9OdrCZfPT4HxIzD-fPLzw/__results__.html#
# FAZER UM CORRPLOT

#################------------ FUNCTION MULTIPLOT ------------#################

# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





#################------------ MESES ------------#################


# pickup
dados1 <- train_80 %>% 
  
  dplyr::select(mes_up) %>%
  
  transmute(Mes = mes_up) %>% 
  
  group_by(Mes) %>%
  
  summarise(n = n()) %>%
  
  mutate(Frequencia = n/sum(n)) %>% 
  
  arrange(desc(n)) %>%
  
  mutate(Mes = factor (Mes, levels = Mes))

# grafico pickup
dados1 %>%
  
  ggplot(aes(x = reorder(Mes, n), y= n, fill= Mes)) +
  
  geom_bar(stat = "identity") +
  
  guides(fill = "none") + #tirou a legenda
  
  coord_flip() +  #muda a direcao do grafico de barras
  
  labs(x = "Mês", y = "Frequência", title = "Gráfico 1: Frequência dos meses de ativação do medidor", subtitle = "Fonte: kaggle.com ") +
  
  geom_label(aes(label = paste(round(100*Frequencia), "%", sep = ""))) + # colocou o rotulo
  
  scale_fill_brewer(palette = "Reds", direction = -1) 


# dropoff
dados2 <- train %>% 
  
  dplyr::select(mes_off) %>%
  
  transmute(Mes = mes_off) %>% 
  
  group_by(Mes) %>%
  
  summarise(n = n()) %>%
  
  mutate(Frequencia = n/sum(n)) %>% 
  
  arrange(desc(n)) %>%
  
  mutate(Mes = factor (Mes, levels = Mes))

# grafico dropoff
dados2 %>%
  
  ggplot(aes(x = reorder(Mes, n), y= n, fill= Mes)) +
  
  geom_bar(stat = "identity") +
  
  guides(fill = "none") + #tirou a legenda
  
  coord_flip() +  #muda a direcao do grafico de barras
  
  labs(x = "Mês", y = "Frequência", title = "Gráfico 2: Frequência dos meses de desativação do medidor", subtitle = "Fonte: kaggle.com ") +
  
  geom_label(aes(label = paste(round(100*Frequencia), "%", sep = ""))) + # colocou o rotulo
  
  scale_fill_brewer(palette = "Reds", direction = -1) 





#################------------ DIAS DA SEMANA ------------#################

# Dia 

ggplot(train, aes(dia_up)) + 
  geom_bar (color = "gray",fill = "#CC3300") +
  coord_flip() +
  xlab("Dia de ativação do medidor") +
  ylab("Frequência") + ggtitle("Gráfico 3: Dias de ativação do medidor")

ggplot(train, aes(dia_off)) + 
  geom_bar (color = "gray",fill = "#CC3300") +
  coord_flip() +
  xlab("Dia de ativação do medidor") +
  ylab("Frequência") + ggtitle("Gráfico 4: Dias de desativação do medidor")




#################------------ HORA ------------#################

ggplot(train, aes(horas_up)) + 
  geom_histogram (color = "gray",fill = "#CC3300", bins = 30) +
  xlab("Hora de ativação do medidor") +
  ylab("Frequência") + ggtitle("Gráfico 5: Histograma dos dos horários de ativação do medidor")

ggplot(train, aes(horas_off)) + 
  geom_histogram (color = "gray",fill = "#CC3300", bins = 30) +
  xlab("Hora de Desativação do medidor") +
  ylab("Frequência") + ggtitle("Gráfico 6: Histograma dos dos horários de desativação do medidor")






#################------------ PASSENGER COUNT ------------#################

p1 <- train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() +
  theme(legend.position = "none")




#################------------ VENDOR ID ------------#################

p2 <- train %>%
  ggplot(aes(vendor_id, fill = vendor_id)) +
  geom_bar() +
  theme(legend.position = "none")





#################------------ STORE ------------#################

p3 <- train %>%
  ggplot(aes(store_and_fwd_flag)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_log10()




#################------------ #TRIPS x DAYS OF WEEK ------------#################

p4 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "none")




#################------------ #TRIPS x HOURS OF DAY ------------#################

p5 <- train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "none")


layout <- matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)

















# do provedor

ggplot(train, aes(as.factor(vendor_id))) + 
  geom_bar (color = "gray",fill = "#CC3300") + 
  xlab("Provedor associado ao registro de viagem") +
  ylab("Número de passageiros no veículo") + 
  ggtitle("Gráfico 7: Número de passageiros no carro por provedor")



ggplot(train, aes(x = as.factor(vendor_id), y = passenger_count)) + 
  geom_boxplot (color = "gray",fill = "#CC3300") + 
  xlab("Provedor associado ao registro de viagem") +
  ylab("Número de passageiros no veículo") + 
  ggtitle("Gráfico 8: Número de passageiros no carro por provedor")

