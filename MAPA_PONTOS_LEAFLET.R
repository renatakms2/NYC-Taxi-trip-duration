

# https://castlab.org/tutorial/2019-04-10-spatial-data-in-r/


library(leaflet)

train_80_1percen <- train %>% sample_frac(0.01)


leaflet() %>% addTiles() %>% addCircles(data = train_80_5percen, 
                                        lng = train_80_5percen$pickup_longitude, 
                                        lat = train_80_5percen$pickup_latitude, 
                                        radius = 0.1)


leaflet() %>% addTiles() %>% addCircles(data = train_80_5percen, 
                                        lng = train_80_5percen$pickup_longitude, 
                                        lat = train_80_5percen$pickup_latitude, 
                                        radius = 0.1)
