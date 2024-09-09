###############################################################################################################################################################################
####                                                                                                                                                                       ####
####                   USING A RANDOM FOREST MODEL TO PREDICT RELATIVE PROBABILITY DISTRIBUTION OF WHOOPING CRANE IN KANSAS: RANDOM Forests Species Distribution Model     ####
####                                                            Ifeoma F. Okonye                                                                                           ####
####                                                                                                                                                                       ####
###############################################################################################################################################################################

Sys.setLanguage("en")
rm(list = ls())

library(pacman)
p_load(tidyverse, randomForest, caret, caTools, raster, sf, pROC, verification, rfUtilities, tools, ROCR)


#######################################
#######################################
########## Import Data
#######################################
#######################################

# WC_data <- read.csv("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Tables/Final RF Variables.csv", header=TRUE, sep=",")
WC_data <- read.csv("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Tables/Final RF Variables.csv", header=TRUE, sep=",")
head(WC_data)


#######################################
#######################################
########## Create Time Variable
#######################################
#######################################


# Define the date range and calculate the number of days
start_date <- as.Date("2010-03-23")
end_date <- as.Date("2016-11-25")
num_days <- as.numeric(difftime(end_date, start_date, units = "days"))


# Generate random numbers from 1 to 2435
set.seed(123)
random_days <- sample(1:num_days, size = 2000, replace = TRUE)


#Convert the random numbers to actual dates
random_dates <- start_date + random_days


# Create data frame for pseudo-absence points with their assigned random dates
pseudo_absence_date <- data.frame(
  OID_ = 1:2000,  
  random_date = random_dates
)
head(pseudo_absence_date)


#convert time column to date format
WC_data$TIME_OM <- as.Date(WC_data$TIME_OM, format = "%m/%d/%Y")
str(WC_data)


# Merge the pseudo-absence dates into WC_data based on OID
WC_data <- WC_data %>%
  left_join(pseudo_absence_date, by = "OID_") %>%
  mutate(
    obs_date = coalesce(as.Date(random_date), as.Date(TIME_OM))  # Ensure Date format & prioritize random_date
  ) %>%
  dplyr::select(-OID_, -TIME_OM, -random_date)  # Remove the original columns
head(WC_data)


#######################################
#######################################
########## Tidy Data
######################################
#######################################

# Transform data types
WC_data <- as.data.frame(WC_data) %>% 
  mutate(
    sd = as.factor(ifelse(Obs_Type %in% c("A", "Absent", "Absence"), "Absent", "Present")), #sd is specie distribution
    
    ec = as.factor(ecotone_code),
    pca = as.factor(Percent_cover_Agric),
    pcu = as.factor(Percent_cover_Urban),
    pcw = as.factor(Percent_cover_Wetland),
    pcr = as.factor(Percent_cover_Road),
    be = as.factor(bearing),
    Date = as.Date(obs_date),
    Long = as.factor(Longitude),
    Lat = as.factor(Latitude)
    ) %>% 
  dplyr::select(-(1:10)) 
str(WC_data)


# Convert ecotone categories to numbers
WC_data$ec <- as.factor(match(WC_data$ec, LETTERS[1:16]))
head(WC_data)
str(WC_data)


# Adding Season variable
wcDataframe_season <- WC_data %>%
  mutate(
    Date = as.Date(Date),  # Keep as Date type
    season = case_when(
      month(Date) %in% 3:5 ~ "Spring",
      month(Date) %in% 9:11 ~ "Fall",
      month(Date) %in% 6:8 ~ "Summer",
      TRUE ~ "Winter"))


# Replace NA values
# Convert factor columns to numeric and replace NA values with 0
wcDataframe_season <- wcDataframe_season %>%
  mutate(
    DateNum = as.numeric(as.Date(Date)),
    season = as.factor(as.character(season)),
    pca = as.numeric(as.character(pca)),
    pcu = as.numeric(as.character(pcu)),
    pcw = as.numeric(as.character(pcw)),
    pcr = as.numeric(as.character(pcr)),
    be = as.numeric(as.character(be)),
    Long = as.numeric(as.character(Long)),
    Lat = as.numeric(as.character(Lat))
  ) %>%
  mutate(
    DateNum = replace_na(DateNum, 0),
    season = replace_na(season, 0),
    pca = replace_na(pca, 0),
    pcu = replace_na(pcu, 0),
    pcw = replace_na(pcw, 0),
    pcr = replace_na(pcr, 0),
    be = replace_na(be, 0),
    Long = replace_na(Long, 0),
    Lat = replace_na(Lat, 0)
  ) %>% 
  dplyr::select(-Date)


str(wcDataframe_season)

# Remove Season Variable
wcDataframe <- wcDataframe_season %>%
  dplyr::select(-season)

str(wcDataframe)

# Set Seed
set.seed(1234)

###############################################################################################################
####                                                                                                       ####
####                            Random Forest Model: Belaire Model for Kansas                              ####
####                              Variables: Be, PCA, PCW, PCU, PCR, Ec                                    ####
####                                                                                                       ####
###############################################################################################################

#######################################
#######################################
##### Select Required Variables
#######################################
#######################################

WcD_Belaire <- wcDataframe %>% 
  dplyr::select(-(8:10))
str(WcD_Belaire)

# Set up the formula for random forest
formula <- sd ~ pcu + pcw + pca + pcr + ec + be

# Split the data into training and testing sets
trainIndex <- createDataPartition(WcD_Belaire$sd, p = 0.75, list = FALSE)
trainData <- WcD_Belaire[trainIndex, ]
testData <- WcD_Belaire[-trainIndex, ]


#######################################
#######################################
##### Building the Random Forest model
#######################################
#######################################

# Check for NA Values and view summary of data
colSums(is.na(WcD_Belaire))
summary(WcD_Belaire)


# Fit the Random Forest model
(SDmodel <- randomForest(formula, 
                        data = trainData, 
                        importance = TRUE,
                        Proximity = TRUE,
                        Positive = "Present",
                        ntree = 500))



#Plot variable Importance
importance(SDmodel)
varImpPlot(SDmodel, main = "Species Distribution Model", scale = TRUE)


#######################################
#######################################
####### Optimizing the Model
#######################################
#######################################


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



# making model with nTrees
(SDmodelnT <- randomForest(sd ~ ., 
                          data = trainData, 
                          ntree = 1000,
                          importance = TRUE,
                          Proximity = TRUE))


# to see if plotting with nT is better, plot error rate
oob.error.data1 <- data.frame(
  Trees = rep(1:nrow(SDmodelnT$err.rate), times = 3),
  Type = rep(c("OOB", "Present", "Absent"), each = nrow(SDmodelnT$err.rate)),
  Error = c(SDmodelnT$err.rate[ , "OOB"],
            SDmodelnT$err.rate[ , "Present"],
            SDmodelnT$err.rate[ , "Absent"])
)

oob.error.data1 %>% 
  ggplot(aes(Trees, Error)) +
  geom_line(aes(color = Type))


# Optimum Variables per node

oob.values <- vector(length = 10) 

for (i in 1:10) { 
  temp.model <- temp.model <- randomForest(sd ~ ., data = trainData, mtry=i, ntree=1500) 
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

oob.values #print out the different oob error values and the one with the lowest value is the number of variables that works best.


#######################################
#######################################
####### Final Random Forest Model.
#######################################
#######################################

## find the minimum error
min(oob.values)
## find the optimal value for mtry...
mtryV <- which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
(SDmodel_Belaire <- randomForest(sd ~ ., 
                                data = trainData,
                                ntree = 1001, 
                                proximity = TRUE,
                                importance = TRUE,
                                mtryV = mtryV))


importance(SDmodel_Belaire)
varImpPlot(SDmodel_Belaire, main = "Species Distribution Model", scale = TRUE)


#######################################
#######################################
##### Making Predictions
#######################################
#######################################

# Predict probabilities on the test set
testData$Predicted <- predict(SDmodel_Belaire, testData, type = "prob")[, "Present"]

# Convert the predicted probabilities to "Absent" or "Present" based on a threshold
testData$PredictedClass <- ifelse(testData$Predicted > 0.5, "Present", "Absent")
testData$PredictedClass <- factor(testData$PredictedClass, levels = c("Absent", "Present"))


# Evaluate model performance
(cm <- confusionMatrix(data = testData$PredictedClass, 
                       reference = testData$sd,
                       positive = "Present"))


# Extract the confusion matrix table
cm_table <- as.table(cm$table)


# Convert the table to a data frame
cm_df <- as.data.frame(cm_table)

# Plot confusion matrix
ggplot(data = cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "brown", high = "darkslateblue") +
  labs(x = "Actual Class", y = "Predicted Class") +
  theme_minimal()



#######################################
#######################################
####### Plotting AUC Curve
#######################################
#######################################


par(pty = "s")

# Plot the ROC curve using the test set response and predicted probabilities
ROC <- roc(testData$sd, testData$Predicted, percent = TRUE, 
           levels = c("Absent", "Present"), legacy.axes=TRUE)
ROC

str(ROC)

##############################################################

# # To find out the optimal threshold
# roc.df <- data.frame(
#   tpp=ROC$sensitivities*100, ## tpp = true positive percentage
#   fpp=(1 - ROC$specificities)*100, ## fpp = false positive precentage
#   thresholds=ROC$thresholds)
# 
# head(roc.df) ## head() will show us the values for the upper right-hand corner
# ## of the ROC graph, when the threshold is so low
# ## (negative infinity) that every single sample is called "obese".
# ## Thus TPP = 100% and FPP = 100%
# 
# tail(roc.df) ## tail() will show us the values for the lower left-hand corner
# ## of the ROC graph, when the threshold is so high (infinity)
# ## that every single sample is called "not obese".
# ## Thus, TPP = 0% and FPP = 0%
# 
# ## now let's look at the thresholds between TPP 60% and 80%...
# roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

#################################################################


AUC_Plot <- plot.roc(ROC, col = "#00008B", lwd = 4, print.auc = TRUE,
                     xlab="False Positive Percentage", ylab="True Postive Percentage")

par(pty = "m")


#######################################
#######################################
##### Plotting Partial Dependence Plots
#######################################
#######################################
# rm(imp,impvar,op)

# # Looping over variables ranked by importance:
# imp <- importance(SDmodel)
# impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
# 
# 
# # setting the graph number of rows and columns
# op <- par(mfrow=c(2, 3))
# 
# for (i in seq_along(impvar)) {
#   partialPlot(SDmodel, WcD_Belaire, impvar[i], xlab=impvar[i],
#               main=paste("Partial Dependence on", impvar[i]),
#               ylim=c(-1, 2),
#               which.class = "Present")
# }
# par(op)
# 

#######################################
#######################################
####### Normalize Importance
#######################################
#######################################

# Function to normalize a vector to a 0-1 scale
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Get importance of variables and sort them
imp <- importance(SDmodel)
impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]

# Define labels for ecotone category (A-P)
ecotone_labels <- LETTERS[1:16]  # Creates A-P

# Set up the plot layout
op <- par(mfrow = c(2, 3))

# Loop through each variable and plot
for (i in seq_along(impvar)) {
  if (impvar[i] == "ec") {
    # Handle categorical variable (ecotone category) as a bar plot
    mean_values <- tapply(predict(SDmodel, trainData, type = "prob")[, "Present"], 
                          trainData[[impvar[i]]], mean)
    
    # Normalize the values
    mean_values <- normalize(mean_values)
    
    # Barplot with custom ecotone labels (A-P)
    barplot(mean_values, 
            names.arg = ecotone_labels,  # Replace numbers with A-P
            main = "Partial Dependence on Ecotone Category", 
            xlab = "Ecotope Category", 
            ylab = "Normalized Predicted Suitability", 
            col = "blue")
  } else {
    # Generate partial dependence data
    pd_data <- partialPlot(SDmodel, trainData, impvar[i], which.class = "Present", smooth = "loess", plot = FALSE)
    
    # Normalize y-values
    pd_data$y <- normalize(pd_data$y)
    
    # Plot the normalized partial dependence plot
    plot(pd_data$x, pd_data$y, type = "l", col = "#4daf4a", lwd = 3, 
         main = paste("Partial Dependence on", impvar[i]), 
         xlab = impvar[i], 
         ylab = "Normalized Predicted Suitability")
  }
}

# Reset plot layout
par(op)

# 
# ################### From Dan's Class
# par(mfrow=c(2,3))  # Set up multi-panel layout
# 
# # Get importance of variables and sort them
# imp <- importance(SDmodel)
# impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]
# 
# for(i in seq_along(impvar)) {
#   var_name <- impvar[i]  # Get variable name
#   {
#     rf.partial.prob(SDmodel, trainData, var_name, "Present",
#                     main = paste("Partial Dependence on", impvar[i]),
#                     smooth = "loess", raw = TRUE, rug = FALSE)
#   }
# }
# 
# # Reset plot layout
# par(op)

#######################################
#######################################
####### Habitat Suitability Map
#######################################
#######################################

# Load the raster layers
# urban_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Pcr.tif")
# wetland_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcw.tif")
# agric_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pca.tif")
# road_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcr.tif")
# ecotone_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/ec.tif")
# bearing_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/be.tif")
# Longitude_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Long.tif")
# Latitude_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Lat.tif")

urban_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Pcr.tif")
wetland_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcw.tif")
agric_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pca.tif")
road_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcr.tif")
ecotone_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/ec.tif")
bearing_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/be.tif")
Longitude_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Long.tif")
Latitude_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Lat.tif")

# Stack the rasters
env_var <- stack(urban_raster, wetland_raster, agric_raster, road_raster, ecotone_raster, bearing_raster)
names(env_var) <- c("pcu", "pcw", "pca", "pcr", "ec", "be")


# Predict the suitability/probability of occurrence across the landscape
suitability_map <- predict(env_var, SDmodel_Belaire, type = "prob", index = 2, progress = "window")

# Plot the suitability map
plot(suitability_map, main = "Whooping Crane Habitat Suitability Map")

# Save the suitability map
# writeRaster(suitability_map, "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/SuitabilityMap_Belaire.tif", format = "GTiff", overwrite = TRUE)
writeRaster(suitability_map, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - Belaire/RelativeProbabilityFunction_Belaire_2011_1km2.tif", format = "GTiff", overwrite = TRUE)


##############################################
##############################################
######### Identify Habitats as Binary Variable
##############################################
##############################################
rm(env_var) #Remove stack to free up some memory

# Occurrence thresholds
occurrence.threshold(SDmodel_Belaire, trainData[,sel.vars], class="Present", p = seq(0.1, 0.9, 0.02), type = "delta.ss")

occurrence.threshold(SDmodel_Belaire, trainData[,sel.vars], class="Present", p = seq(0.1, 0.9, 0.02), type = "kappa")

occurrence.threshold(SDmodel_Belaire, trainData[,sel.vars], class="Present", p = seq(0.1, 0.7, 0.02), type = "sum.ss")

# change raster to binary layer
rbinary <- r

# set based on occurrence threshold from summed sensitivity and specificity above to create binary(1 or 0) habitat map
rbinary[rbinary >= 0.4] <- 1
rbinary[rbinary < 0.4] <- 0

# double check
head(rbinary)
plot(rbinary)
rbinary[rbinary == 0] <- NA
writeRaster(rbinary, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - Belaire/RelativeProbabilityFunction_Binary.tif", format = "GTiff", overwrite = TRUE)





###############################################################################################################
####                                                                                                       ####
####                   Improved Model: Inclusion of Spatial and Temporal Covariates                        ####
####                                 New Variables: Lat, Long, Date                                        ####
####                                                                                                       ####
###############################################################################################################

##############################################
##############################################
########## Multicolinearity test
##############################################
##############################################
cl <- multi.collinear(wcDataframe[,3:ncol(wcDataframe)], p=0.05) 
cl

# perform a "leave one out" test to evaluate if any of the variables are forcing other variables to appear multicollinear. 
# for(l in cl) {
#   cl.test <- wcDataframe[,-which(names(wcDataframe)==l)]
#   print(paste("Remove variable", l, sep=": "))
#   multi.collinear(cl.test, p=0.05) 
# }

# remove multicolinear variable
# wcDataframe <- wcDataframe[,-which(names(wcDataframe) %in% cl )]
# str(wcDataframe)


##############################################
##############################################
# Calculate the percent of the positive
# (Present) class to check for sample
# balance (zero inflation issues) in the model
##############################################
##############################################
(nrow(wcDataframe[wcDataframe$sd == "Present", ]) / nrow(wcDataframe)) * 100


##############################################
##############################################
########## Model Selection
##############################################
##############################################

# This approach is used to find the most parsimonious model that is not fitting noise. 
# This allows us to test the model parameters and select the model with the best error component.
# set.seed(123)
(rf.model <- rf.modelSel(x=wcDataframe[,2:ncol(wcDataframe)], 
                         y=wcDataframe$sd, 
                         imp.scale="mir", 
                         ntree=1001))


######################## Method 1 ###################

# Get variables for each parameter set
params <- rf.model$parameters
param_sets <- lapply(params, function(x) as.formula(paste("sd ~", paste(x, collapse = " + "))))


# Print formulas for verification
lapply(param_sets, print)


ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model_list <- list()
for (i in seq_along(params)) {
  model_list[[i]] <- train(
    x = wcDataframe[, params[[i]]],
    y = wcDataframe$sd,
    method = "rf",
    trControl = ctrl,
    metric = "ROC"
  )
}

# Compare results
resamps <- resamples(model_list)
summary(resamps)
dotplot(resamps, metric = "ROC")

# Aggregate results
results <- resamples(model_list)

# Print AUC comparison
summary(results, metric = "ROC")


# Pairwise t-tests with Bonferroni correction
diff <- diff(results, adjust = "bonferroni")
summary(diff, metric = "ROC")



######################## Method 2 ###################
# First we select the default parameters
# sel.vars <- rf.model$selvars

# Alternatively, choose a specific model (in this case, parameters associated with model 2).
# sel.vars <- rf.model$parameters[[2]]

# # Alternatively, choose a specific model (in this case, parameters associated with model 1).
sel.vars <- rf.model$parameters[[1]]


# Set up the formula for random forest
SelVar <- as.formula(paste("sd ~", paste(sel.vars, collapse = " + ")))


##############################################
##############################################
########## Selecting required vraiables
##############################################
##############################################

# Split the data into training and testing sets
# set.seed(123)

# Create a combined stratification variable
wcDataframe <- wcDataframe %>%
  mutate(strata = interaction(sd, wcDataframe_season$season, drop = TRUE))

# Verify the distribution
table(wcDataframe$strata)


trainIndex <- createDataPartition(wcDataframe$strata, 
                                  p = 0.60, 
                                  list = FALSE,
                                  times = 1)
trainData <- wcDataframe[trainIndex, ]
testData <- wcDataframe[-trainIndex, ]

##############################################
##############################################
########## Building the Random Forest model
##############################################
##############################################

# Fit the Random Forest model
# Method 1
# formula <- param_sets[[2]]
# 
# (SDmodelI <- randomForest(formula, 
#                           data = trainData, 
#                           importance = TRUE,
#                           Proximity = TRUE,
#                           norm.votes=TRUE,
#                           positive = "Present",
#                           ntree = 1001))

# Method 2
(SDmodelI <- randomForest(SelVar,
                          data = trainData,
                          importance = TRUE,
                          Proximity = TRUE,
                          norm.votes=TRUE,
                          positive = "Present",
                          ntree = 1001))


importance(SDmodelI)
varImpPlot(SDmodelI, main = "Species Distribution Model", scale = TRUE)

##############################################
##############################################
########## Making Predictions
##############################################
##############################################

# Predict probabilities on the test set
testData$Predicted <- predict(SDmodelI, testData, type = "prob")[, "Present"]

# Convert the predicted probabilities to "Absent" or "Present" based on a threshold
testData$PredictedClass <- ifelse(testData$Predicted > 0.5, "Present", "Absent")
testData$PredictedClass <- factor(testData$PredictedClass, levels = c("Absent", "Present"))

# Evaluate model performance
(cm <- confusionMatrix(data = testData$PredictedClass, 
                       reference = testData$sd,
                       positive = "Present"))


##############################################
##############################################
########## Plot Confusion Matrix
##############################################
##############################################

# Extract the confusion matrix table
cm_table <- as.table(cm$table)

# Convert the table to a data frame
cm_df <- as.data.frame(cm_table)

# Plot confusion matrix
ggplot(data = cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "brown", high = "darkslateblue") +
  labs(x = "Actual Class", y = "Predicted Class") +
  theme_minimal()


##############################################
##############################################
########## Plotting AUC Curve
##############################################
##############################################
par(pty = "s")

# Plot the ROC curve using the test set response and predicted probabilities
ROC <- roc(testData$sd, testData$Predicted, percent = TRUE, 
           levels = c("Absent", "Present"), legacy.axes=TRUE)

# Plot the AUC using the ROC Curve
AUC_Plot <- plot.roc(ROC, col = "#00008B", lwd = 4, print.auc = TRUE,
                     xlab="False Positive Percentage", ylab="True Postive Percentage")

par(pty = "m")


#######################################################################################
#######################################################################################
#######################################################################################
# Making Partial Dependence Plots
#######################################################################################
#######################################################################################
#######################################################################################

##############################################
##############################################
########## Regular Plots - Not Normalized
##############################################
##############################################
# 
# # Looping over variables ranked by importance:
# imp <- importance(SDmodelI)
# impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
# 
# 
# # setting the graph number of rows and columns
# op <- par(mfrow=c(2, 3))
# 
# for (i in seq_along(impvar)) {
#   partialPlot(SDmodelI, testData, impvar[i], which.class = "Present", xlab=impvar[i],
#               main=paste("Partial Dependence on", impvar[i]),
#               ylim=c(-1, 2))
# }
# par(op)


##############################################
##############################################
########## Normalized Plots - Smoothening
##############################################
##############################################

# Set up the plot layout
op <- par(mfrow = c(3, 3))

# Function to normalize a vector to a 0-1 scale
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Define labels for ecotone category (A-P)
ecotone_labels <- LETTERS[1:16]  # Creates A-P

# Get importance of variables and sort them
imp <- importance(SDmodelI)
impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]

for (i in seq_along(impvar)) {
  var_name <- impvar[i]  # Get variable name
  
  if (impvar[i] == "ec") {
    # Handle categorical variable (ecotone category) as a bar plot
    mean_values <- tapply(predict(SDmodelI, testData, type = "prob")[, "Present"], 
                          testData[[impvar[i]]], mean)
    
    # Normalize the values
    mean_values <- normalize(mean_values)
    
    # Barplot with custom ecotone labels (A-P)
    barplot(mean_values, 
            names.arg = ecotone_labels,  # Replace numbers with A-P
            main = "Partial Dependence on Ecotone Category", 
            xlab = "Ecotone Category", 
            ylab = "Normalized Predicted Suitability", 
            col = "blue")
  } else {
    # Process numerical variables normally
    rf.partial.prob(SDmodelI, testData, var_name, "Present",
                    main = paste("Partial Dependence on", var_name),
                    smooth = "loess", raw = TRUE, rug = FALSE,
                    ylab = "Normalized Probability of Use")
  }
}

# Restore previous plotting settings
par(op)


#######################################################################################
#######################################################################################
#######################################################################################
# Creating Resource Selection Function for a specific Day
#######################################################################################
#######################################################################################
#######################################################################################

##############################################
##############################################
########## create raster for Day
##############################################
##############################################

# Define the raster folder
# raster_folder <- "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data"
raster_folder <- "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data"


#########################
# 
# # Load the raster layers
# # urban_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Pcr.tif")
# # wetland_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcw.tif")
# # agric_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pca.tif")
# # road_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcr.tif")
# # ecotone_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/ec.tif")
# # bearing_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/be.tif")
# # Longitude_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Long.tif")
# # Latitude_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Lat.tif")
# 
# urban_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Pcr.tif")
# wetland_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcw.tif")
# agric_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pca.tif")
# road_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcr.tif")
# ecotone_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/ec.tif")
# bearing_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/be.tif")
# Longitude_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Long.tif")
# Latitude_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Lat.tif")
# 
# # Input date and create day raster
# InputDate <- "2015-04-15"
# DatePredict <- as.numeric(as.Date(InputDate)) 
# # date <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcw.tif")  # Create a template raster
# date <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcw.tif")  # Create a template raster
# 
# values(date) <- DatePredict  # Assign the future date value to all cells
# 
# # Get all variables that are included in the random forest model (including "Date")
# raster_vars <- rownames(SDmodelI$importance)
# 
# # # Stack the rasters
# # env_var <- stack(urban_raster, wetland_raster, agric_raster, road_raster, ecotone_raster, bearing_raster, Longitude_raster, Latitude_raster, date)
# # names(env_var) <- c("pcu", "pcw", "pca", "pcr", "ec", "be", "Long", "Lat", "DateNum")
# 
# 
# # Stack the rasters
# env_var <- stack(wetland_raster, agric_raster, ecotone_raster, bearing_raster, Longitude_raster, Latitude_raster, date)
# names(env_var) <- c("pcw", "pca", "ec", "be", "Long", "Lat", "DateNum")
# 
# 
# # Ensure variable names match the model
# (names(env_var) <- raster_vars)
# 
# # Predict the suitability/probability of occurrence across the landscape
# suitability_map <- predict(env_var, SDmodelI, type = "prob", index = 2, progress = "window")
# 
# 
# # Plot the suitability map
# plot(suitability_map, main = "Whooping Crane Habitat Suitability Map")
# 
# # Save the suitability map
# # writeRaster(r, "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Suitability Map - All Variables/Suitability.model_AllVariables15Apr15.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(suitability_map, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - All Variables/Suitability.model_AllVariables15Apr2015_New.tif", format = "GTiff", overwrite = TRUE)

#############################

# Input date and create day raster
InputDate <- "2015-11-15"
DatePredict <- as.numeric(as.Date(InputDate))
# date <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcw.tif")  # Create a template raster
date <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcw.tif")  # Create a template raster

values(date) <- DatePredict  # Assign the future date value to all cells


# Get all variables that are included in the random forest model (including "Date")
raster_vars <- rownames(SDmodelI$importance)

# Construct file paths for all raster variables
raster_files <- file.path(raster_folder, paste0(raster_vars, ".tif"))

# Check if the files exist before stacking
raster_files <- raster_files[file.exists(raster_files)]

# Stack the raster layers
env_var <- stack(raster_files, date)

# Ensure variable names match the model
(names(env_var) <- raster_vars)


##############################################
##############################################
########## Predict RSF for that Day
##############################################
##############################################
# Remove comment for type of variables to use in model out of "All Variables", "Default Parameters", and "Selected parameters"

# # All Variables ##############################
# predict(env_var, SDmodelI, "SuitabilityModel_Parameter2_15Oct2015.tif", type="prob", index=2, na.rm=TRUE, overwrite=TRUE, progress="window")
# 
# # Load predicted probability raster
# r <- raster("SuitabilityModel_Parameter2_15Oct2015.tif")
# # Plot the probability raster
# plot(r)
# # Save the suitability map
# # writeRaster(r, "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Suitability Map - All Variables/Suitability.model_AllVariables15Apr15.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(r, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - Selected Parameters/SuitabilityModel_Parameter2_15Oct2015.tif", format = "GTiff", overwrite = TRUE)


# # Default Parameters ##############################
# predict(env_var, SDmodelI, "Suitability.model_DefaultParameters.tif", type="prob", index=2, na.rm=TRUE, overwrite=TRUE, progress="window")
# 
# # Load predicted probability raster
# r <- raster("Suitability.model_DefaultParameters.tif")
# # Plot the probability raster
# plot(r)
# # Save the suitability map
# writeRaster(r, "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Suitability Map - Default Parameters/Suitability.model_DefaultParameters.tif", format = "GTiff", overwrite = TRUE)


# Selected Parameters ##############################
predict(env_var, SDmodelI, "suitabilityModel_AllVariables_2015-11-15.tif", type="prob", index=2, na.rm=TRUE, overwrite=TRUE, progress="window")

# Load predicted probability raster
r <- raster("suitabilityModel_AllVariables_2015-11-15.tif")
# Plot the probability raster
plot(r)
# Save the suitability map
# writeRaster(r, "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Suitability Map - Selected Parameters/Suitability.model_Parameter(2)15Oct15.tif", format = "GTiff", overwrite = TRUE)
writeRaster(r, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - All Variables/RelativeProbabilityFunction_AllVariables_2015-11-15.tif", format = "GTiff", overwrite = TRUE)


##############################################
##############################################
######### Identify Habitats as Binary Variable
##############################################
##############################################
rm(env_var) #Remove stack to free up some memory

# Occurrence thresholds
occurrence.threshold(SDmodelI, trainData[,sel.vars], class="Present", p = seq(0.1, 0.9, 0.02), type = "delta.ss")

occurrence.threshold(SDmodelI, trainData[,sel.vars], class="Present", p = seq(0.1, 0.9, 0.02), type = "kappa")

occurrence.threshold(SDmodelI, trainData[,sel.vars], class="Present", p = seq(0.1, 0.7, 0.02), type = "sum.ss")

# change raster to binary layer
rbinary <- r

# set based on occurrence threshold from summed sensitivity and specificity above to create binary(1 or 0) habitat map
rbinary[rbinary >= 0.4] <- 1
rbinary[rbinary < 0.4] <- 0

# double check
head(rbinary)
plot(rbinary)
rbinary[rbinary == 0] <- NA
writeRaster(rbinary, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - All Variables/RelativeProbabilityFunction_Binary_2015-11-15.tif", format = "GTiff", overwrite = TRUE)



###############################################################################################################
####                                                                                                       ####
####                                 Improved Model: Temporal Analysis                                     ####
####                     1. Variations between migration seasons (Spring and Fall)                         ####
####                                    2. Variations between years                                        ####
####                                      3. Time series Analysis                                          ####
###############################################################################################################

#######################################################################################
#######################################################################################
#######################################################################################
# Variations between migration seasons (Spring and Fall)
#######################################################################################
#######################################################################################
#######################################################################################

# Make sure wcDataframe has Date column in Date format
wcDataframe$Date <- as.Date(WC_data$Date)

# Define function to classify season
season <- function(date) {
  month <- month(date)
  if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Winter")
  }
}

# Add Year and Season columns
wcDataframe <- wcDataframe %>%
  mutate(Year = year(Date),
         Season = sapply(Date, season))

# ✅ Specify year and season of interest
target_year <- 2015
target_season <- "Spring"  # or "Spring", "Summer", "Winter"

# Filter for specific season within a year
subset_data <- wcDataframe %>%
  filter(Year == target_year, Season == target_season)

# Check data size
cat("Subset size:", nrow(subset_data), "rows\n")

# Partition data
set.seed(123)
train_idx <- createDataPartition(subset_data$sd, p = 0.75, list = FALSE)
train_data <- subset_data[train_idx, ]
test_data <- subset_data[-train_idx, ]


formula <- sd ~ pcu + pcw + pca + pcr + ec + be + Long + Lat #+ DateNum
# Train model
(rf_season <- randomForest(formula,
                           data = train_data,
                           importance = TRUE,
                           Proximity = TRUE,
                           norm.votes=TRUE,
                           positive = "Present",
                           ntree = 500))

importance(rf_season)
varImpPlot(rf_season, main = "Species Distribution Model", scale = TRUE)


##############################################
##############################################
########## Making Predictions
##############################################
##############################################

# Predict probabilities on the test set
testData$Predicted <- predict(rf_season, testData, type = "prob")[, "Present"]

# Convert the predicted probabilities to "Absent" or "Present" based on a threshold
testData$PredictedClass <- ifelse(testData$Predicted > 0.5, "Present", "Absent")
testData$PredictedClass <- factor(testData$PredictedClass, levels = c("Absent", "Present"))

# Evaluate model performance
(cm <- confusionMatrix(data = testData$PredictedClass, 
                       reference = testData$sd,
                       positive = "Present"))



##############################################
##############################################
########## Plotting AUC Curve - look at examples in help and improve plot
##############################################
##############################################
par(pty = "s")

# Plot the ROC curve using the test set response and predicted probabilities
ROC <- roc(testData$sd, testData$Predicted, percent = TRUE,
           levels = c("Absent", "Present"), legacy.axes=TRUE)

# Plot the AUC using the ROC Curve
AUC_Plot <- plot.roc(ROC, col = "#00008B", lwd = 4, print.auc = TRUE,
                     xlab="False Positive Percentage", ylab="True Postive Percentage")

par(pty = "m")


##############################################
##############################################
########## Normalized Plots - Smoothening
##############################################
##############################################

# Set up the plot layout
op <- par(mfrow = c(3, 3))

# Function to normalize a vector to a 0-1 scale
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Define labels for ecotone category (A-P)
ecotone_labels <- LETTERS[1:16]  # Creates A-P

# Get importance of variables and sort them
imp <- importance(rf_season)
impvar <- rownames(imp)[order(imp[, 1], decreasing = TRUE)]

for (i in seq_along(impvar)) {
  var_name <- impvar[i]  # Get variable name
  
  if (impvar[i] == "ec") {
    # Handle categorical variable (ecotone category) as a bar plot
    mean_values <- tapply(predict(rf_season, testData, type = "prob")[, "Present"], 
                          testData[[impvar[i]]], mean)
    
    # Normalize the values
    mean_values <- normalize(mean_values)
    
    # Barplot with custom ecotone labels (A-P)
    barplot(mean_values, 
            names.arg = ecotone_labels,  # Replace numbers with A-P
            main = "Partial Dependence on Ecotone Category", 
            xlab = "Ecotone Category", 
            ylab = "Normalized Predicted Suitability", 
            col = "blue")
  } else {
    # Process numerical variables normally
    rf.partial.prob(rf_season, testData, var_name, "Present",
                    main = paste("Partial Dependence on", var_name),
                    smooth = "loess", raw = TRUE, rug = FALSE,
                    ylab = "Normalized Probability of Use")
  }
}

# Restore previous plotting settings
par(op)

#######################################
#######################################
####### Habitat Suitability Map
#######################################
#######################################

# Load the raster layers
# urban_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Pcr.tif")
# wetland_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcw.tif")
# agric_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pca.tif")
# road_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/pcr.tif")
# ecotone_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/ec.tif")
# bearing_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/be.tif")
# Longitude_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Long.tif")
# Latitude_raster <- raster("G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data/Lat.tif")

urban_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Pcr.tif")
wetland_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcw.tif")
agric_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pca.tif")
road_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/pcr.tif")
ecotone_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/ec.tif")
bearing_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/be.tif")
Longitude_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Long.tif")
Latitude_raster <- raster("C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Lat.tif")

# Stack the rasters
env_var <- stack(urban_raster, wetland_raster, agric_raster, road_raster, ecotone_raster, bearing_raster, Longitude_raster, Latitude_raster)
names(env_var) <- c("pcu", "pcw", "pca", "pcr", "ec", "be", "Long", "Lat")


# Predict the suitability/probability of occurrence across the landscape
suitability_map <- predict(env_var, rf_season, type = "prob", index = 2, progress = "window")

# Plot the suitability map
plot(suitability_map, main = "Whooping Crane Habitat Suitability Map")

##############################################
##############################################
######### Identify Habitats as Binary Variable
##############################################
##############################################
rm(env_var) #Remove stack to free up some memory

# Occurrence thresholds
occurrence.threshold(SDmodelI, trainData[,sel.vars], class="Present", p = seq(0.1, 0.9, 0.02), type = "delta.ss")

occurrence.threshold(SDmodelI, trainData[,sel.vars], class="Present", p = seq(0.1, 0.9, 0.02), type = "kappa")

occurrence.threshold(SDmodelI, trainData[,sel.vars], class="Present", p = seq(0.1, 0.7, 0.02), type = "sum.ss")

# change raster to binary layer
rbinary <- r

# set based on occurrence threshold from summed sensitivity and specificity above to create binary(1 or 0) habitat map
rbinary[rbinary >= 0.4] <- 1
rbinary[rbinary < 0.4] <- 0

# double check
head(rbinary)
plot(rbinary)
rbinary[rbinary == 0] <- NA
writeRaster(rbinary, "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data/Suitability Map - All Variables/RelativeProbabilityFunction_Binary_2015-11-15.tif", format = "GTiff", overwrite = TRUE)


#######################################################################################
#######################################################################################
#######################################################################################
# Variations between years
#######################################################################################
#######################################################################################
#######################################################################################

# ✅ Set target year
target_year <- 2010

# Filter data for that year
subset_data <- wcDataframe %>%
  filter(Year == target_year)

# Check sample size
cat("Subset size:", nrow(subset_data), "rows\n")

# Split into training and testing
set.seed(123)
train_idx <- createDataPartition(subset_data$sd, p = 0.75, list = FALSE)
train_data <- subset_data[train_idx, ]
test_data <- subset_data[-train_idx, ]

# Fit Random Forest model
formula <- sd ~ pcu + pcw + pca + pcr + ec + be + Long + Lat #+ DateNum

# Train model
(rf_year <- randomForest(formula,
                         data = train_data,
                         importance = TRUE,
                         Proximity = TRUE,
                         norm.votes=TRUE,
                         positive = "Present",
                         ntree = 500))

importance(rf_year)
varImpPlot(rf_year, main = "Species Distribution Model", scale = TRUE)








#######################################################################################
#######################################################################################
#######################################################################################
# Time series Analysis
#######################################################################################
#######################################################################################
#######################################################################################

# Define the raster folder
# raster_folder <- "G:/KDWP_WhoopingCrane/HabitatAssessment_Belaire_New/HabitatAssessment_Belaire/Raster Data"
raster_folder <- "C:/Users/ifeom/OneDrive - Kansas State University/Desktop/New Research Stuff to Merge with SSD Drive/HabitatAssessment_Belaire/Raster Data"

# Define years and migration seasons
years <- seq(2010, 2016, by = 3)  # 5-years with a 5-year interval
spring_dates <- c("-03-23", "-04-15", "-05-10")  # Spring migration dates
fall_dates <- c("-09-15", "-10-10", "-11-05")  # Fall migration dates

# List to store suitability rasters
suitability_rasters <- list()

# Loop through years and seasons
for (year in years) {
  for (season_name in names(list(Spring = spring_dates, Fall = fall_dates))) {
    dates <- if (season_name == "Spring") spring_dates else fall_dates  # Select correct dates
    
    for (date_suffix in dates) {
      InputDate <- paste0(year, date_suffix)  # Construct date
      DatePredict <- as.numeric(as.Date(InputDate))  # Convert to numeric format for model
      
      # Load a reference raster to match extent
      date_raster <- raster(file.path(raster_folder, "pcw.tif"))
      values(date_raster) <- DatePredict  # Assign date value
      
      # Get environmental variables used in the model
      raster_vars <- rownames(SDmodelI$importance)  # Ensure this matches model variables
      raster_files <- file.path(raster_folder, paste0(raster_vars, ".tif"))
      raster_files <- raster_files[file.exists(raster_files)]  # Keep only existing files
      
      # Stack raster layers
      env_var <- stack(raster_files, date_raster)
      names(env_var) <- raster_vars  # Ensure names match the model
      
      # Generate output file name
      output_file <- file.path(raster_folder, paste0("Suitability_", year, "_", season_name, "_", gsub("-", "", date_suffix), ".tif"))
      
      # Remove existing file if needed
      if (file.exists(output_file)) {
        file.remove(output_file)
      }
      
      # Predict habitat suitability and save as GeoTIFF
      suitability_raster <- predict(env_var, SDmodelI, filename = output_file, type = "prob",
                                    index = 2, na.rm = TRUE, overwrite = TRUE, progress = "window")
      
      # Optional: Plot suitability map
      plot(suitability_raster, main = paste("Habitat Suitability -", season_name, year, date_suffix))
      
      # Store raster for animation
      suitability_rasters <- append(suitability_rasters, list(suitability_raster))
    }
  }
}


