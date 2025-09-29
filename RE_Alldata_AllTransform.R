mds <- fread("/work/bmcdaniel/R_stuff/merged_data_with_station_names.csv")

# Detrend 
detrend_model <- lm(Q ~ year, data = mds)
mds[, Q_detrended := residuals(detrend_model)]

# Z-Score
mean_q <- mean(mds$Q_detrended, na.rm = TRUE)
sd_q   <- sd(mds$Q_detrended, na.rm = TRUE)
mds[, Q_standardized := (Q_detrended - mean_q) / sd_q]

#Min-Max Normalization 
min_q <- min(mds$Q_detrended, na.rm = TRUE)
max_q <- max(mds$Q_detrended, na.rm = TRUE)
mds[, Q_minmax := (Q_detrended - min_q) / (max_q - min_q)]


fwrite(mds, "/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")


head(mds)U