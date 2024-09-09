Sys.setLanguage("en")
rm(list = ls())

library(pacman)
p_load(tidyverse, randomForest, caret, caTools, raster, sf)


#######################################################################################
# Loading and Cleaning the data
#######################################################################################



WC_data <- read.csv("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Tables/Final Telemetry Data1.csv", header=TRUE, sep=",")

head(WC_data)

WC_data <- as.data.frame(WC_data) %>% 
  mutate(
    pca = as.factor(Percent_cover_Agric),
    pcu = as.factor(Percent_cover_Urban),
    pcw = as.factor(Percent_cover_Wetland),
    pcr = as.factor(Percent_cover_Road),
    be = as.factor(bearing),
    ec = as.factor(ecotone_code),
    sd = as.factor(ifelse(Obs_Type == "A", "Absent", "Present")) #sd is specie distribution
  ) %>% 
  dplyr::select(-(1:10)) 


# Replace NA values
# Convert factor columns to numeric and replace NA values with 0
WC_data <- WC_data %>%
  mutate(
    pca = as.numeric(as.character(pca)),
    pcu = as.numeric(as.character(pcu)),
    pcw = as.numeric(as.character(pcw)),
    pcr = as.numeric(as.character(pcr)),
    be = as.numeric(as.character(be))
  ) %>%
  mutate(
    pca = replace_na(pca, 0),
    pcu = replace_na(pcu, 0),
    pcw = replace_na(pcw, 0),
    pcr = replace_na(pcr, 0),
    be = replace_na(be, 0)
  )

str(WC_data)


#######################################################################################
# Setting up the Random Forest model
#######################################################################################



# Set up the formula for random forest
formula <- sd ~ pcu + pcw + pca + pcr + ec + be

# Split the data into training and testing sets
set.seed(150)
trainIndex <- createDataPartition(WC_data$sd, p = 0.75, list = FALSE)
trainData <- WC_data[trainIndex, ]
testData <- WC_data[-trainIndex, ]


#######################################################################################
# Building the Random Forest model
#######################################################################################

# Check for NA Values
colSums(is.na(WC_data))

summary(WC_data)


# Fit the Random Forest model
# Method 1
SDmodel <- randomForest(formula, 
                        data = trainData, 
                        importance = TRUE,
                        Proximity = TRUE,
                        ntree = 500)
SDmodel

# Method 2
SDmodel1 <- randomForest(sd ~ ., 
                         data = trainData, 
                         importance = TRUE,
                         Proximity = TRUE)
SDmodel1 



#######################################################################################
# Making Predictions
#######################################################################################


# Predict probabilities on the test set
testData$Predicted <- predict(SDmodel, testData, type = "prob")[, "Present"]

# Convert the predicted probabilities to "Absent" or "Present" based on a threshold
testData$PredictedClass <- ifelse(testData$Predicted > 0.5, "Present", "Absent")
testData$PredictedClass <- factor(testData$PredictedClass, levels = c("Absent", "Present"))

# Evaluate model performance
confusionMatrix(data = testData$PredictedClass, 
                reference = testData$sd,
                positive = "Present")



#######################################################################################
# Optimizing the Model
#######################################################################################

# Optimum nTrees

# Plotting the error rates for each tree based on a matrix within the model called err.rate
head(SDmodel$err.rate, 5)

oob.error.data <- data.frame(
  Trees = rep(1:nrow(SDmodel$err.rate), times = 3),
  Type = rep(c("OOB", "Present", "Absent"), each = nrow(SDmodel$err.rate)),
  Error = c(SDmodel$err.rate[ , "OOB"],
            SDmodel$err.rate[ , "Present"],
            SDmodel$err.rate[ , "Absent"])
)

oob.error.data %>% 
  ggplot(aes(Trees, Error)) +
  geom_line(aes(color = Type))


#making model with 1000 trees

SDmodel_1000 <- randomForest(sd ~ ., 
                             data = trainData, 
                             ntree = 1000, 
                             Proximity = TRUE)
SDmodel_1000 

#to see if plotting with 1000 is better, plot error rate

oob.error.data1 <- data.frame(
  Trees = rep(1:nrow(SDmodel_1000$err.rate), times = 3),
  Type = rep(c("OOB", "Present", "Absent"), each = nrow(SDmodel_1000$err.rate)),
  Error = c(SDmodel_1000$err.rate[ , "OOB"],
            SDmodel_1000$err.rate[ , "Present"],
            SDmodel_1000$err.rate[ , "Absent"])
)

oob.error.data1 %>% 
  ggplot(aes(Trees, Error)) +
  geom_line(aes(color = Type))



#Plot variable Importance

importance(SDmodel)
varImpPlot(SDmodel)



# Optimum Variables per node


#################### Method 1 #######################################
oob.values <- vector(length = 10) 

for (i in 1:10) { 
  temp.model <- temp.model <- randomForest(sd ~ ., data = trainData, mtry=i, ntree=1000) 
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

oob.values #print out the different oob error values and the one with the lowest value is the number of variables that works best.





##################### Method 2 #############################################

# Get the number of predictor variables in the dataset
num_predictors <- ncol(trainData) - 1  # Subtract 1 to exclude the response variable

# Initialize a vector to store out-of-bag (OOB) error rates
oob.values <- vector(length = num_predictors)

# Loop over different values of mtry within the valid range
for (i in 1:num_predictors) {
  temp.model <- randomForest(sd ~ ., 
                             data = trainData, 
                             mtry = i, 
                             ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1]
}

# Display the OOB error rates for each value of mtry
oob.values




#######################################################################################
# Final Random Forest Model.
#######################################################################################


## find the minimum error
min(oob.values)
## find the optimal value for mtry...
mtryV <- which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
FinalSD_model <- randomForest(sd ~ ., 
                              data = trainData,
                              ntree = 1000, 
                              proximity = TRUE, 
                              mtryV = mtryV)
FinalSD_model



#######################################################################################
# Making Predictions
#######################################################################################

# Predict probabilities on the test set
testData$Predicted <- predict(FinalSD_model, testData, type = "prob")[, "Present"]

# Convert the predicted probabilities to "Absent" or "Present" based on a threshold
testData$PredictedClass <- ifelse(testData$Predicted > 0.5, "Present", "Absent")
testData$PredictedClass <- factor(testData$PredictedClass, levels = c("Absent", "Present"))

# Evaluate model performance
confusionMatrix(data = testData$PredictedClass, 
                reference = testData$sd,
                positive = "Present")


#######################################################################################
# Habitat selection
#######################################################################################

# Load the raster layers
urban_raster <- raster("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Percent_Cover_Urban_Raster.tif")
wetland_raster <- raster("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Percent_Cover_Wetlands_Raster.tif")
agric_raster <- raster("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Percent_Cover_Agric_Raster.tif")
road_raster <- raster("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Percent_Cover_Road_Raster.tif")
ecotone_raster <- raster("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Ecotone_Raster.tif")
bearing_raster <- raster("D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Bearing_Raster.tif")




# Stack the rasters
env_var <- stack(urban_raster, wetland_raster, agric_raster, road_raster, ecotone_raster, bearing_raster)
names(env_var) <- c("pcu", "pcw", "pca", "pcr", "ec", "be")


# Predict the suitability/probability of occurrence across the landscape
suitability_map <- predict(env_var, FinalSD_model, type = "prob", index = 2)






# Plot the suitability map
plot(suitability_map, main = "Whooping Crane Habitat Suitability Map")


# Save the suitability map
writeRaster(suitability_map, "D:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/suitability_map_sample.tif", format = "GTiff", overwrite = TRUE)


