### Demo ###

#We will be using tidymodels for this demo so we need to install the package
#Any other machine learning package in R will work as well
library(tidyverse)
library(tidymodels)
library(vip)
library(patchwork)

#Load the functions in the global environment
source("./TransformFunctions.R")

#Load in the data 
#load("SimulatedData1.RData")
load("SimulatedData2.RData")

#Plotting the data
ggplot(data=trainData, aes(x=trainLocs[,1], y=trainLocs[,2], color=y)) +
  geom_point() +
  scale_color_distiller(palette="Spectral") +
  theme_minimal() +
  ggtitle("Training Data")

cores <- max(1,parallel::detectCores()-5)

### Tranform the data to independent data
transformedData <- transform_to_ind(formula=y~.,
                                  trainData=trainData,
                                  trainLocs=trainLocs,
                                  testData=testData[,-1],
                                  testLocs=testLocs,
                                  smoothness=1/2,
                                  M=30,
                                  ncores=cores)




## Creating a recipe for both a spatial and non-spatial random forest

#Spatial Recipe 
spatial_rf_rec <- recipe(y~., data=transformedData$trainData) 

#Non-Spatial Recipe
rf_rec <- recipe(y~., data=trainData) 

#Initialize the random forest model
#These values were trained externally
rf_model <- rand_forest(mtry=5, min_n = 10, trees=500) %>%
  set_engine("ranger", importance = "permutation", num.threads = 1) %>%
  set_mode("regression")

#Spatial Workflow 
spatial_rf_wf <- workflow() %>%
  add_recipe(spatial_rf_rec) %>%
  add_model(rf_model) %>%
  fit(data=transformedData$trainData)

#None-Spatial Workflow
rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_model) %>%
  fit(data=trainData)

## Make Predictions
spatial_RF_preds <- predict(spatial_rf_wf, new_data=transformedData$testData)$.pred %>%
  back_transform_to_spatial(preds=., transformedData) #back transform to spatial

RF_preds <- predict(rf_wf, new_data=testData)$.pred


## VIP - only X1 and X2 are important
p1 <- vip(spatial_rf_wf$fit$fit, num_features = 10)
p2 <- vip(rf_wf$fit$fit, num_features = 10)
ggpubr::ggarrange(p1, p2, ncol=2)


#Predictions 
testPreds <- data.frame(truth=testData$y,
                        SpatialRF=spatial_RF_preds,
                        NonSpatialRF= RF_preds)

results <- data.frame(t(apply(testPreds,2,FUN = function(o){
  sqrt(mean((testPreds$truth-o)^2))
})))



#Comparing the predictions to the truth with graph

t1 <- ggplot(data=testData, aes(x=testLocs[,1], y=testLocs[,2], color=y)) +
  geom_point() +
  scale_color_distiller(palette="Spectral") +
  theme_minimal() +
  ggtitle("Test Data")

t2 <- ggplot(data=testPreds, aes(x=testLocs[,1], y=testLocs[,2], 
                                color=SpatialRF)) +
  geom_point() +
  scale_color_distiller(palette="Spectral") +
  theme_minimal() +
  ggtitle("Spatial Random Forest Predictions")

t3 <- ggplot(data=testPreds, aes(x=testLocs[,1], y=testLocs[,2], 
                                color=NonSpatialRF)) +
  geom_point() +
  scale_color_distiller(palette="Spectral") +
  theme_minimal() +
  ggtitle("Non-Spatial Random Forest Predictions")

t1 + (t2/t3)






