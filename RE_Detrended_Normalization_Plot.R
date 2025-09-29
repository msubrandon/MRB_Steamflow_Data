# i need to detrend and then normailze the streamflow data first

#Step one detrend all the streamflow data

#Step two normalize all the streamflow data

'/work/bmcdaniel/R_stuff/merged_data_with_station_names.csv'

#THIS IS THE FINAL OUTPUT
Final_out <- read.csv("/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")

View(Final_out)



merged_with_station_names <- '/work/bmcdaniel/R_stuff/merged_data_with_station_names.csv'
View(merged_with_station_names)
# Read the dataset

mds <- read.csv('/work/bmcdaniel/R_stuff/merged_data_with_station_names.csv')

# Fit a linear model: Q as a function of year
model <- lm(Q ~ year, data = mds)

# Detrend the data by subtracting the fitted values from the original Q values
mds$Q_detrended <- residuals(model)

# Save the new dataset with the detrended Q values
write.csv(mds, "/work/bmcdaniel/R_stuff/detrended_data_with_station_names.csv", row.names = FALSE)

# View the first few rows to confirm
head(mds)





# Read the dataset with detrended Q values
mds <- read.csv('/work/bmcdaniel/R_stuff/detrended_data_with_station_names.csv')

# Calculate the mean and standard deviation of the detrended Q values
mean_Q_detrended <- mean(mds$Q_detrended)
sd_Q_detrended <- sd(mds$Q_detrended)

# Apply Z-Score Standardization
mds$Q_normalized <- (mds$Q_detrended - mean_Q_detrended) / sd_Q_detrended

# Save the normalized dataset to a new CSV file
write.csv(mds, "/work/bmcdaniel/R_stuff/normalized_detrended_data_with_station_names.csv", row.names = FALSE)

# View the first few rows to confirm
head(mds)





# Read the dataset with detrended Q values
mds <- read.csv('/work/bmcdaniel/R_stuff/detrended_data_with_station_names.csv')

# Calculate the minimum and maximum for Min-Max Normalization
min_Q_detrended <- min(mds$Q_detrended)
max_Q_detrended <- max(mds$Q_detrended)

# Apply Min-Max Normalization
mds$Q_normalized <- (mds$Q_detrended - min_Q_detrended) / (max_Q_detrended - min_Q_detrended)

# Calculate the mean and standard deviation for Z-Score Standardization
mean_Q_detrended <- mean(mds$Q_detrended)
sd_Q_detrended <- sd(mds$Q_detrended)

# Apply Z-Score Standardization
mds$Q_standardized <- (mds$Q_detrended - mean_Q_detrended) / sd_Q_detrended

# Save the dataset with both normalized and standardized Q values
write.csv(mds, "/work/bmcdaniel/R_stuff/normalized_standardized_detrended_data_with_station_names.csv", row.names = FALSE)

# View the first few rows to confirm
head(mds)





# Read the dataset with detrended Q values
mds <- read.csv('/work/bmcdaniel/R_stuff/detrended_data_with_station_names.csv')

# Calculate the minimum and maximum of the detrended Q values
min_Q_detrended <- min(mds$Q_detrended)
max_Q_detrended <- max(mds$Q_detrended)

# Apply Min-Max Normalization to the detrended Q values
mds$Q_detrended_normalized <- (mds$Q_detrended - min_Q_detrended) / (max_Q_detrended - min_Q_detrended)

# Save the dataset with normalized detrended Q values
write.csv(mds, "/work/bmcdaniel/R_stuff/minmax_normalized_detrended_data.csv", row.names = FALSE)

# View the first few rows to confirm
head(mds)





# Read the dataset with detrended Q values and existing normalized/standardized columns
mds <- read.csv('/work/bmcdaniel/R_stuff/normalized_standardized_detrended_data_with_station_names.csv')

# Calculate the minimum and maximum of the detrended Q values for Min-Max Normalization
min_Q_detrended <- min(mds$Q_detrended)
max_Q_detrended <- max(mds$Q_detrended)

# Apply Min-Max Normalization to the detrended Q values
mds$Q_detrended_normalized <- (mds$Q_detrended - min_Q_detrended) / (max_Q_detrended - min_Q_detrended)

# Save the updated dataset with all columns (standardized, normalized, and Min-Max normalized)
write.csv(mds, "/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv", row.names = FALSE)

# View the first few rows to confirm
head(mds)




Final_out <- read.csv("/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")