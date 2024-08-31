










##################### TEMPLATE ###################################

# Load necessary libraries
library(randomForest)
library(sp)
library(raster)
library(caret)

# Assuming your dataset is stored in a data frame called 'crane_data'
# and the variables are named as follows:
# - 'Presence': presence (1) or absence (0) points
# - 'UrbanCover': percent urban cover
# - 'WetlandCover': percent wetland cover
# - 'AgriCover': percent agriculture cover
# - 'RoadCover': percent road cover
# - 'Ecotone': categorical variable for distance to wetlands and agriculture
# - 'Bearing': bearing to the wintering ground from the breeding ground

# Convert Ecotone to factor if it's not already
crane_data$Ecotone <- as.factor(crane_data$Ecotone)

# Set up the formula for random forest
formula <- Presence ~ UrbanCover + WetlandCover + AgriCover + RoadCover + Ecotone + Bearing

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(crane_data$Presence, p = 0.7, list = FALSE)
trainData <- crane_data[trainIndex, ]
testData <- crane_data[-trainIndex, ]

# Train the random forest model
rf_model <- randomForest(formula, data = trainData, importance = TRUE, ntree = 500)

# Print the model summary
print(rf_model)

# Predict probabilities on the test set
testData$Predicted <- predict(rf_model, testData, type = "prob")[,2]

# Evaluate model performance
confusionMatrix(data = as.factor(ifelse(testData$Predicted > 0.5, 1, 0)), 
                reference = as.factor(testData$Presence))

# Importance of variables
importance(rf_model)
varImpPlot(rf_model)

# Assuming you have raster layers for the environmental variables
# Load the raster layers
urban_raster <- raster("path_to/urban_cover.tif")
wetland_raster <- raster("path_to/wetland_cover.tif")
agri_raster <- raster("path_to/agriculture_cover.tif")
road_raster <- raster("path_to/road_cover.tif")
ecotone_raster <- raster("path_to/ecotone.tif")
bearing_raster <- raster("path_to/bearing.tif")

# Stack the rasters
env_stack <- stack(urban_raster, wetland_raster, agri_raster, road_raster, ecotone_raster, bearing_raster)
names(env_stack) <- c("UrbanCover", "WetlandCover", "AgriCover", "RoadCover", "Ecotone", "Bearing")

# Predict the suitability/probability of occurrence across the landscape
suitability_map <- predict(env_stack, rf_model, type = "prob", index = 2)

# Save the suitability map
writeRaster(suitability_map, "path_to/suitability_map.tif", format = "GTiff", overwrite = TRUE)

# Plot the suitability map
plot(suitability_map, main = "Whooping Crane Habitat Suitability Map")
