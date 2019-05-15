
# REVER EM: https://www.kaggle.com/pularat/nyc-taxi-trip-duration-die-hard-three#using-xgboost-to-model

library(xgboost)

#convert to XGB matrix

t1 <- train_80 %>% dplyr::select(-trip_duration)

v1 <- val_20 %>% dplyr::select(-trip_duration)


glimpse(t1)
glimpse(v1)


dtrain <- xgb.DMatrix(as.matrix(t1),label = train_80$trip_duration)

dvalid <- xgb.DMatrix(as.matrix(v1),label = val_20$trip_duration)

dtest <- xgb.DMatrix(as.matrix(test))



##----------------- DEFININDO OS PARAMETROS DO XGBOOST

xgb_params <- list(colsample_bytree = 0.7, #variables per tree 
                   
                   subsample = 0.7, #data subset per tree 
                   
                   booster = "gbtree",
                   
                   max_depth = 5, #tree levels
                   
                   eta = 0.3, #shrinkage
                   
                   eval_metric = "rmse", 
                   
                   objective = "reg:linear",
                   
                   seed = 4321
                   
)



watchlist <- list(train=dtrain, valid=dvalid)


## ------------------- TREINAMENTO DO XGBOOST

set.seed(4321)

gb_dt <- xgb.train(params = xgb_params,
                   
                   data = dtrain,
                   
                   print_every_n = 5,
                   
                   watchlist = watchlist,
                   
                   nrounds = 60)

# [1]	train-rmse:4.243015	valid-rmse:4.244735 
# [6]	train-rmse:0.948383	valid-rmse:0.957047 
# [11]	train-rmse:0.592950	valid-rmse:0.602963 
# [16]	train-rmse:0.553851	valid-rmse:0.563628 
# [21]	train-rmse:0.533557	valid-rmse:0.542377 
# [26]	train-rmse:0.510886	valid-rmse:0.521047 
# [31]	train-rmse:0.494924	valid-rmse:0.506520 
# [36]	train-rmse:0.477075	valid-rmse:0.490587 
# [41]	train-rmse:0.466431	valid-rmse:0.481044 
# [46]	train-rmse:0.454349	valid-rmse:0.469444 
# [51]	train-rmse:0.450345	valid-rmse:0.466535 
# [56]	train-rmse:0.446085	valid-rmse:0.463280 
# [60]	train-rmse:0.441604	valid-rmse:0.459662


xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 5, nrounds=15)


# [1]	train-rmse:4.242499+0.000764	test-rmse:4.242580+0.003457 
# Multiple eval metrics are present. Will use test_rmse for early stopping.
# Will train until test_rmse hasn't improved in 10 rounds.
# 
# [2]	train-rmse:3.009365+0.000837	test-rmse:3.009196+0.003825 
# [3]	train-rmse:2.160074+0.000980	test-rmse:2.160235+0.003266 
# [4]	train-rmse:1.583671+0.001665	test-rmse:1.584429+0.002729 
# [5]	train-rmse:1.201949+0.001963	test-rmse:1.203315+0.003212 
# [6]	train-rmse:0.957984+0.003710	test-rmse:0.959992+0.003081 
# [7]	train-rmse:0.810551+0.006961	test-rmse:0.813385+0.004998 
# [8]	train-rmse:0.720368+0.007302	test-rmse:0.724287+0.008796 
# [9]	train-rmse:0.672311+0.007315	test-rmse:0.676785+0.009445 
# [10]	train-rmse:0.639514+0.007596	test-rmse:0.644477+0.007406 
# [11]	train-rmse:0.619609+0.004389	test-rmse:0.624994+0.003821 
# [12]	train-rmse:0.609663+0.004977	test-rmse:0.615470+0.006006 
# [13]	train-rmse:0.602295+0.004828	test-rmse:0.608262+0.007298 
# [14]	train-rmse:0.590196+0.003718	test-rmse:0.596835+0.006194 
# [15]	train-rmse:0.585549+0.003928	test-rmse:0.592328+0.005460 


## -------------------- IMPORTANCIA DAS VARIAVEIS

imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(train %>% dplyr::select(-trip_duration)), model = gb_dt))

imp_matrix %>%
  
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  
  geom_col() +
  
  coord_flip() +
  
  theme(legend.position = "none") +
  
  labs(x = "Features", y = "Importance")

## ------------------- PREDICAO (?????????)

test_preds <- predict(gb_dt,dtest) ### ta dando erro aqui

pred <- test_id %>%
  
  mutate(trip_duration = exp(test_preds) - 1)


pred %>% write_csv('submit.csv')