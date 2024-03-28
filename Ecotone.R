rm(list = ls())

# Load required libraries
library(pacman)
p_load(tidyverse, readr)

# Read CSV file
WHCRMCTelemetry_dist <- read.csv("C:/Users/ifeom/OneDrive - Kansas State University/Courses/3. HSI Research/KDWP_WhoopingCrane/HabitatAssessment_Belaire/Tables/WHCRMCTelemetryG.csv")

# Display structure of the data frame
str(WHCRMCTelemetry_dist)

# Convert columns to numeric
WHCRMCTelemetry_dist <- WHCRMCTelemetry_dist %>%
  mutate(DistanceWetlands = as.numeric(DistanceWetlands),
         DistanceAgric = as.numeric(DistanceAgric))

# Define a function to categorize distances
categorize_distance <- function(DistanceWetlands, DistanceAgric) {
  ifelse(DistanceWetlands >= 0 & DistanceWetlands <= 100,
         ifelse(DistanceAgric >= 0 & DistanceAgric <= 100, "A",
                ifelse(DistanceAgric > 100 & DistanceAgric <= 500, "B",
                       ifelse(DistanceAgric > 500 & DistanceAgric <= 1000, "E", "J"))),
         ifelse(DistanceWetlands > 100 & DistanceWetlands <= 500,
                ifelse(DistanceAgric >= 0 & DistanceAgric <= 100, "C",
                       ifelse(DistanceAgric > 100 & DistanceAgric <= 500, "D",
                              ifelse(DistanceAgric > 500 & DistanceAgric <= 1000, "F", "K"))),
                ifelse(DistanceWetlands > 500 & DistanceWetlands <= 1000,
                       ifelse(DistanceAgric >= 0 & DistanceAgric <= 100, "G",
                              ifelse(DistanceAgric > 100 & DistanceAgric <= 500, "H",
                                     ifelse(DistanceAgric > 500 & DistanceAgric <= 1000, "I", "L"))),
                       ifelse(DistanceAgric >= 0 & DistanceAgric <= 100, "M",
                              ifelse(DistanceAgric > 100 & DistanceAgric <= 500, "N",
                                     ifelse(DistanceAgric > 500 & DistanceAgric <= 1000, "O", "P"))))))
}

# Create a new column 'ecotone_code'
ecotone <- WHCRMCTelemetry_dist %>%
  select(-(27:48)) %>%
  mutate(ecotone_code = categorize_distance(DistanceWetlands, DistanceAgric))


write_csv(ecotone, file = "ecotone.csv")
