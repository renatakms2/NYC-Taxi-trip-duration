

# day up
train = train %>% mutate(dia_up = ifelse(.$dia_up == 1, "Domingo",
                                         ifelse(.$dia_up == 2, "Segunda",
                                                ifelse(.$dia_up == 3, "Terca",
                                                       ifelse(.$dia_up == 4, "Quarta",
                                                              ifelse(.$dia_up == 5, "Quinta",
                                                                     ifelse(.$dia_up == 6, "Sexta",
                                                                          ifelse(.$dia_up == 7, "Sabado", .$dia_up))))))))

table(train$dia_up)

# day off
train = train %>% mutate(dia_off = ifelse(.$dia_off == 1, "Domingo",
                                         ifelse(.$dia_off == 2, "Segunda",
                                                ifelse(.$dia_off == 3, "Terca",
                                                       ifelse(.$dia_off == 4, "Quarta",
                                                              ifelse(.$dia_off == 5, "Quinta",
                                                                     ifelse(.$dia_off == 6, "Sexta",
                                                                            ifelse(.$dia_off == 7, "Sabado", .$dia_off))))))))


table(train$dia_off)

# mes up
train = train %>% mutate(mes_up = ifelse(.$mes_up == 1, "Jan",
                                         ifelse(.$mes_up == 2, "Fev",
                                                ifelse(.$mes_up == 3, "Mar",
                                                       ifelse(.$mes_up == 4, "Abr",
                                                              ifelse(.$mes_up == 5, "Mai",
                                                                     ifelse(.$mes_up == 6, "Jun",
                                                                            ifelse(.$mes_up == 7, "Jul", .$mes_up))))))))

table(train$mes_up)

# mes off
train = train %>% mutate(mes_off = ifelse(.$mes_off == 1, "Jan",
                                         ifelse(.$mes_off == 2, "Fev",
                                                ifelse(.$mes_off == 3, "Mar",
                                                       ifelse(.$mes_off == 4, "Abr",
                                                              ifelse(.$mes_off == 5, "Mai",
                                                                     ifelse(.$mes_off == 6, "Jun",
                                                                            ifelse(.$mes_off == 7, "Jul", .$mes_off))))))))

table(train$mes_off)


# visualizacao geral
glimpse(train)




