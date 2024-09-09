# Load required libraries
library(pacman)
p_load(tidyverse, lubridate, geosphere)


# Load the KS Telemetry file
WHCR_KS_Telemetry <- read.csv("C:/Users/ifokonye/OneDrive - Kansas State University/Courses/3. HSI Research/KDWP_WhoopingCrane/HabitatAssessment_Belaire/Tables/WHCR_KS_Telemetr_ExportTable.csv")

# Convert time columns to proper datetime format
WHCR_KS_Telemetry$TIME_OM <- as.POSIXct(WHCR_KS_Telemetry$TIME_OM, format = "%m/%d/%Y %H:%M")
WHCR_KS_Telemetry$TIME_CTZ <- as.POSIXct(WHCR_KS_Telemetry$TIME_CTZ, format = "%m/%d/%Y %H:%M")
WHCR_KS_Telemetry$TIME_CTZ_R <- as.POSIXct(WHCR_KS_Telemetry$TIME_CTZ_R, format = "%m/%d/%Y %H:%M")

# Task 1: Average observations per bird per day
avg_obs_per_day <- WHCR_KS_Telemetry %>%
  group_by(PTT_ID, DATE = as.Date(TIME_CTZ)) %>%
  summarise(observations = n()) %>%
  summarise(avg_observations = mean(observations))
avg_obs_per_day

#Calculating overall average observations
overall_avg <- mean(avg_obs_per_day$avg_observations, na.rm = TRUE)
overall_avg

# Task 2: Telemetry transmitting cycle (converted to hours)
data <- arrange(WHCR_KS_Telemetry, PTT_ID, TIME_CTZ_R)

transmitting_cycle_minutes <- data %>%
  group_by(PTT_ID) %>%
  mutate(transmitting_cycle = difftime(TIME_CTZ_R, lag(TIME_CTZ_R), units = "mins")) %>%
  summarise(avg_transmitting_cycle = mean(as.numeric(transmitting_cycle, units = "mins"), na.rm = TRUE))

# Calculate overall average per bird
overall_avg_per_bird <- mean(transmitting_cycle_minutes$avg_transmitting_cycle, na.rm = TRUE)

# Convert overall average to hours
overall_avg_hours <- overall_avg_per_bird / 60


# Task 3: First and last time of transmitting per day
first_last_transmit <- WHCR_KS_Telemetry %>%
  group_by(PTT_ID, DATE = as.Date(TIME_OM)) %>%
  summarise(first_transmit = min(TIME_CTZ), last_transmit = max(TIME_CTZ))

# Task 4: Clustering of bird points
# Assuming clustering by latitude and longitude within a certain radius
clustered_points <- WHCR_KS_Telemetry %>%
  group_by(PTT_ID, DATE = as.Date(TIME_OM)) %>%
  summarise(cluster_count = n_distinct(paste0(round(LAT, 3), round(LONG, 3))))



# Task 5: Flight distance between consecutive transmissions
flight_distance <- WHCR_KS_Telemetry %>%
  mutate(previous_time = lag(TIME_OM),
         previous_lat = lag(LAT),
         previous_long = lag(LONG)) %>%
  filter(!is.na(previous_time)) %>%
  mutate(distance = distm(cbind(previous_long, previous_lat), cbind(LONG, LAT), fun = distVincentyEllipsoid))

# Print results
print("Task 1: Average observations per bird per day")
print(avg_obs_per_day)

print("Task 2: Telemetry transmitting cycle")
print(mean(as.numeric(transmitting_cycle), na.rm = TRUE))

print("Task 3: First and last time of transmitting per day")
print(first_last_transmit)

print("Task 4: Clustering of bird points")
print(clustered_points)

print("Task 5: Flight distance between consecutive transmissions")
print(mean(flight_distance$distance))

mean(clustered_points[clustered_points])
