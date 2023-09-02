mydata <- mydata %>%
  select(-Rk, -Player)
head(mydata)
mydata$Pos <- as.factor(mydata$Pos)

set.seed(123)
nba_split <- initial_split(mydata, prop = .7)
nba_train <- training(nba_split)
nba_test  <- testing(nba_split)



m1 <- randomForest(
  formula = as.factor(Pos) ~ .,
  data = nba_train
)

m1

m1_pred=as.factor(predict(m1,nba_test,type="class"))
nba_factor=as.factor(nba_test$Pos)
confusionMatrix(m1_pred,nba_factor)
#Accuracy 39% - we really need to improve this model
plot(m1)

# Our random forest model does not perform well when trained with default hyperparameters. 
#$We need to improve the model by tuning the hyperparameters

# further split training data into training and validation
set.seed(2000)
valid_split <- initial_split(nba_train, .8)

# training data
nba_train_v2 <- analysis(valid_split)

#####
#Initial tuning

# names of features
features <- setdiff(names(nba_train), "Pos")

m2 <- tuneRF(
  x          = nba_train[features],
  y          = nba_train$Pos,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

m2

#OOB RMSE is still very high which indicates accuracy will be low. I will continue with improving my model.
#I would like to expand the grid search to make the search faster, I am going to use the ranger that implements C++

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(2, 27, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = Pos ~ ., 
    data            = nba_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

#I still could not reach the efficiency I am looking for in my model.
#I am going to run a full grid search using h20 package that is a java based package. 

#Conenct to the server
h2o.no_progress()
#allocate the max memory used
h2o.init(max_mem_size = "5g")


# create feature names
y <- "Pos"
x <- setdiff(names(nba_train), y)

# turn training set into h2o object
train.h2o <- as.h2o(nba_train)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(17, 27, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set
nba_test.h2o <- as.h2o(nba_test)
best_model_perf <- h2o.performance(model = best_model, newdata = nba_test.h2o)
best_model_perf

#I was able to improve my model to an error rate of 9% on test data!!!



























