#The data that I used 
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# ---- Format Station IDs ----
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# This Merges the Reconstructed and Observed 
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Calibration and Validation Years
calibration_years <- 1940:1989
validation_years  <- 1990:2005

# Validation Metrics
calc_metrics <- function(dt) {
  cal <- dt[year %in% calibration_years]
  val <- dt[year %in% validation_years]
  
  if (nrow(cal) < 1 || nrow(val) < 1) {
    return(data.table(CRSQ = NA_real_, VRSQ = NA_real_, CVRE = NA_real_, VCE = NA_real_))
  }
  
  Qa_cal <- cal$Qa; Q_cal_raw <- cal$Q
  Qa_val <- val$Qa; Q_val_raw <- val$Q
  
  
  Q_cal <- mean(Qa_cal) + ((Q_cal_raw - mean(Q_cal_raw)) * sd(Qa_cal) / sd(Q_cal_raw))
  Q_val <- mean(Qa_val) + ((Q_val_raw - mean(Q_val_raw)) * sd(Qa_val) / sd(Q_val_raw))
  
  CRSQ <- 1 - sum((Qa_cal - Q_cal)^2, na.rm = TRUE) / sum((Qa_cal - mean(Qa_cal))^2, na.rm = TRUE)
  VRSQ <- cor(Q_val, Qa_val, use = "complete.obs")^2
  CVRE <- 1 - sum((Qa_val - Q_val)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_cal))^2, na.rm = TRUE)
  VCE  <- 1 - sum((Qa_val - Q_val)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_val))^2, na.rm = TRUE)
  
  return(data.table(CRSQ = CRSQ, VRSQ = VRSQ, CVRE = CVRE, VCE = VCE))
}
metrics_by_station <- merged[, calc_metrics(.SD), by = ID]

# RMSE
rmse_by_station <- merged[, .(
  RMSE = sqrt(mean((Qa - Q)^2, na.rm = TRUE)),
  nRMSE = sqrt(mean((Qa - Q)^2, na.rm = TRUE)) / mean(Qa, na.rm = TRUE)
), by = ID]

# Using the CE scores from other script
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge all stats into one 
scoreDT <- merge(scoreDT, rmse_by_station, by = "ID", all.x = TRUE)
scoreDT <- merge(scoreDT, metrics_by_station, by = "ID", all.x = TRUE)

if ("nRMSE.y" %in% names(scoreDT)) {
  scoreDT[, nRMSE := nRMSE.y]
} else if ("nRMSE.x" %in% names(scoreDT)) {
  scoreDT[, nRMSE := nRMSE.x]
}
scoreDT[, c("nRMSE.x", "nRMSE.y") := NULL]

if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# plot
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)), size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = metric, limits = scale_limits,
                          breaks = scales::pretty_breaks(3), na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

p_ce    <- create_metric_plot(scoreDT[!is.na(CE)],    'CE',    c(-1, 1)) + labs(title = "CE")
p_nrmse <- create_metric_plot(scoreDT[!is.na(nRMSE)], 'nRMSE', c(0, 2)) + labs(title = "RMSE")
p_crsq  <- create_metric_plot(scoreDT[!is.na(CRSQ)],  'CRSQ',  c(0, 1)) + labs(title = "CRSQ")
p_cvre  <- create_metric_plot(scoreDT[!is.na(CVRE)],  'CVRE',  c(0, 1)) + labs(title = "CVRE")
p_vrsq  <- create_metric_plot(scoreDT[!is.na(VRSQ)],  'VRSQ',  c(0, 1)) + labs(title = "VRSQ")
p_vce   <- create_metric_plot(scoreDT[!is.na(VCE)],   'VCE',   c(0, 1)) + labs(title = "VCE")

final_panel <- (p_ce | p_nrmse | p_crsq) / (p_cvre | p_vrsq | p_vce)
print(final_panel)
ggsave("combined_metrics_map_magma.png", final_panel, width = 16, height = 10, dpi = 300)

# Table to look at data
metrics_table <- scoreDT[, .(ID, CE, nRMSE, CRSQ, CVRE, VRSQ, VCE)]
print(metrics_table)
View(metrics_table)




####

#test

###
library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)

# Load datasets
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format Station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge Reconstructed and Observed
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Calibration and Validation Years
calibration_years <- 1940:1989
validation_years  <- 1990:2005

# Validation Metrics Function (including VRE)
calc_metrics <- function(dt) {
  cal <- dt[year %in% calibration_years]
  val <- dt[year %in% validation_years]
  
  if (nrow(cal) < 1 || nrow(val) < 1) {
    return(data.table(CRSQ = NA_real_, VRSQ = NA_real_, CVRE = NA_real_, VCE = NA_real_, VRE = NA_real_))
  }
  
  Qa_cal <- cal$Qa; Q_cal_raw <- cal$Q
  Qa_val <- val$Qa; Q_val_raw <- val$Q
  
  # Scale reconstructed Q to observed stats
  Q_cal <- mean(Qa_cal) + ((Q_cal_raw - mean(Q_cal_raw)) * sd(Qa_cal) / sd(Q_cal_raw))
  Q_val <- mean(Qa_val) + ((Q_val_raw - mean(Q_val_raw)) * sd(Qa_val) / sd(Q_val_raw))
  
  # Metrics
  CRSQ <- 1 - sum((Qa_cal - Q_cal)^2, na.rm = TRUE) / sum((Qa_cal - mean(Qa_cal))^2, na.rm = TRUE)
  VRSQ <- cor(Q_val, Qa_val, use = "complete.obs")^2
  CVRE <- 1 - sum((Qa_val - Q_val)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_cal))^2, na.rm = TRUE)
  VCE  <- 1 - sum((Qa_val - Q_val)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_val))^2, na.rm = TRUE)
  VRE  <- 1 - sum((Qa_val - Q_val)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_cal))^2, na.rm = TRUE)
  
  return(data.table(CRSQ = CRSQ, VRSQ = VRSQ, CVRE = CVRE, VCE = VCE, VRE = VRE))
}

# Calculate metrics by station (includes VRE now)
metrics_by_station <- merged[, calc_metrics(.SD), by = ID]

# RMSE calculations
rmse_by_station <- merged[, .(
  RMSE = sqrt(mean((Qa - Q)^2, na.rm = TRUE)),
  nRMSE = sqrt(mean((Qa - Q)^2, na.rm = TRUE)) / mean(Qa, na.rm = TRUE)
), by = ID]

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge all metrics
scoreDT <- merge(scoreDT, rmse_by_station, by = "ID", all.x = TRUE)
scoreDT <- merge(scoreDT, metrics_by_station, by = "ID", all.x = TRUE)

# Handle multiple nRMSE columns if present
if ("nRMSE.y" %in% names(scoreDT)) {
  scoreDT[, nRMSE := nRMSE.y]
} else if ("nRMSE.x" %in% names(scoreDT)) {
  scoreDT[, nRMSE := nRMSE.x]
}
scoreDT[, c("nRMSE.x", "nRMSE.y") := NULL]

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# US map boundaries
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Create plotting function
create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)), size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = metric, limits = scale_limits,
                          breaks = scales::pretty_breaks(3), na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

# Generate all plots
p_ce    <- create_metric_plot(scoreDT[!is.na(CE)],    'CE',    c(-1, 1)) + labs(title = "CE")
p_nrmse <- create_metric_plot(scoreDT[!is.na(nRMSE)], 'nRMSE', c(0, 2)) + labs(title = "RMSE")
p_crsq  <- create_metric_plot(scoreDT[!is.na(CRSQ)],  'CRSQ',  c(0, 1)) + labs(title = "CRSQ")
p_cvre  <- create_metric_plot(scoreDT[!is.na(CVRE)],  'CVRE',  c(0, 1)) + labs(title = "CVRE")
p_vrsq  <- create_metric_plot(scoreDT[!is.na(VRSQ)],  'VRSQ',  c(0, 1)) + labs(title = "VRSQ")
p_vce   <- create_metric_plot(scoreDT[!is.na(VCE)],   'VCE',   c(0, 1)) + labs(title = "VCE")
p_vre   <- create_metric_plot(scoreDT[!is.na(VRE)],   'VRE',   c(0, 1)) + labs(title = "VRE")

# Combine plots
final_panel <- (p_ce | p_nrmse | p_crsq) / (p_cvre | p_vrsq | p_vce) / p_vre
print(final_panel)
ggsave("combined_metrics_map_with_vre.png", final_panel, width = 16, height = 12, dpi = 300)

# View metrics table
metrics_table <- scoreDT[, .(ID, CE, nRMSE, CRSQ, CVRE, VRSQ, VCE, VRE)]
print(metrics_table)
View(metrics_table)



#######




####

library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)

# Load datasets
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format Station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge Reconstructed and Observed
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Calibration and Validation Years
calibration_years <- 1940:1989
validation_years  <- 1990:2005

# Updated Validation Metrics Function with proper CVRE (LOOCV)
calc_metrics <- function(dt) {
  cal <- dt[year %in% calibration_years]
  val <- dt[year %in% validation_years]
  
  if (nrow(cal) < 2 || nrow(val) < 1) {
    return(data.table(CRSQ = NA_real_, VRSQ = NA_real_, CVRE = NA_real_,
                      VCE = NA_real_, VRE = NA_real_))
  }
  
  # Calibration and validation values
  Qa_cal <- cal$Qa; Q_cal <- cal$Q
  Qa_val <- val$Qa; Q_val <- val$Q
  
  # Scale Q to match Qa in mean and sd
  Q_cal_scaled <- mean(Qa_cal) + ((Q_cal - mean(Q_cal)) * sd(Qa_cal) / sd(Q_cal))
  Q_val_scaled <- mean(Qa_val) + ((Q_val - mean(Q_val)) * sd(Qa_val) / sd(Q_val))
  
  # CRSQ
  CRSQ <- 1 - sum((Qa_cal - Q_cal_scaled)^2, na.rm = TRUE) / sum((Qa_cal - mean(Qa_cal))^2, na.rm = TRUE)
  
  # Validation stats
  VRSQ <- cor(Q_val_scaled, Qa_val, use = "complete.obs")^2
  VRE  <- 1 - sum((Qa_val - Q_val_scaled)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_cal))^2, na.rm = TRUE)
  VCE  <- 1 - sum((Qa_val - Q_val_scaled)^2, na.rm = TRUE) / sum((Qa_val - mean(Qa_val))^2, na.rm = TRUE)
  
  # Leave-one-out PRESS for CVRE
  PRESS <- 0
  for (i in 1:nrow(cal)) {
    loo <- cal[-i]
    slope <- sd(loo$Qa) / sd(loo$Q)
    intercept <- mean(loo$Qa) - slope * mean(loo$Q)
    pred <- intercept + slope * cal$Q[i]
    PRESS <- PRESS + (Qa_cal[i] - pred)^2
  }
  SS_tot <- sum((Qa_cal - mean(Qa_cal))^2, na.rm = TRUE)
  CVRE <- 1 - PRESS / SS_tot
  
  return(data.table(CRSQ = CRSQ, VRSQ = VRSQ, CVRE = CVRE, VCE = VCE, VRE = VRE))
}

# Run metrics per station
metrics_by_station <- merged[, calc_metrics(.SD), by = ID]

# RMSE calculations
rmse_by_station <- merged[, .(
  RMSE = sqrt(mean((Qa - Q)^2, na.rm = TRUE)),
  nRMSE = sqrt(mean((Qa - Q)^2, na.rm = TRUE)) / mean(Qa, na.rm = TRUE)
), by = ID]

# Merge into main scores
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)
scoreDT <- merge(scoreDT, rmse_by_station, by = "ID", all.x = TRUE)
scoreDT <- merge(scoreDT, metrics_by_station, by = "ID", all.x = TRUE)

# Clean up nRMSE column
if ("nRMSE.y" %in% names(scoreDT)) {
  scoreDT[, nRMSE := nRMSE.y]
} else if ("nRMSE.x" %in% names(scoreDT)) {
  scoreDT[, nRMSE := nRMSE.x]
}
scoreDT[, c("nRMSE.x", "nRMSE.y") := NULL]

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# US map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Plot function
create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)),
               size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = NULL, limits = scale_limits,
                          breaks = scales::pretty_breaks(3),
                          na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}


# Generate all plots
p_ce    <- create_metric_plot(scoreDT[!is.na(CE)],    'CE',    c(-1, 1)) + labs(title = "CE")
p_nrmse <- create_metric_plot(scoreDT[!is.na(nRMSE)], 'nRMSE', c(0, 2)) + labs(title = "RMSE")
p_crsq  <- create_metric_plot(scoreDT[!is.na(CRSQ)],  'CRSQ',  c(0, 1)) + labs(title = "CRSQ")
p_cvre  <- create_metric_plot(scoreDT[!is.na(CVRE)],  'CVRE',  c(0, 1)) + labs(title = "CVRE")
p_vrsq  <- create_metric_plot(scoreDT[!is.na(VRSQ)],  'VRSQ',  c(0, 1)) + labs(title = "VRSQ")
p_vce   <- create_metric_plot(scoreDT[!is.na(VCE)],   'VCE',   c(0, 1)) + labs(title = "VCE")
p_vre   <- create_metric_plot(scoreDT[!is.na(VRE)],   'VRE',   c(0, 1)) + labs(title = "VRE")

# Final combined plot
final_panel <- (p_ce | p_nrmse | p_crsq) / (p_cvre | p_vrsq | p_vce) / p_vre
print(final_panel)
ggsave("combined_metrics_map_with_cvre_vre.png", final_panel, width = 16, height = 12, dpi = 300)

# View metrics table
metrics_table <- scoreDT[, .(ID, CE, nRMSE, CRSQ, CVRE, VRSQ, VCE, VRE)]
print(metrics_table)
View(metrics_table)



###########


# 6-metric plot function (excluding VCE)
p_ce    <- create_metric_plot(scoreDT[!is.na(CE)],    'CE',    c(-1, 1)) + labs(title = "CE")
p_nrmse <- create_metric_plot(scoreDT[!is.na(nRMSE)], 'nRMSE', c(0, 2)) + labs(title = "nRMSE")
p_crsq  <- create_metric_plot(scoreDT[!is.na(CRSQ)],  'CRSQ',  c(0, 1)) + labs(title = "CRSQ")
p_cvre  <- create_metric_plot(scoreDT[!is.na(CVRE)],  'CVRE',  c(0, 1)) + labs(title = "CVRE")
p_vrsq  <- create_metric_plot(scoreDT[!is.na(VRSQ)],  'VRSQ',  c(0, 1)) + labs(title = "VRSQ")
p_vre   <- create_metric_plot(scoreDT[!is.na(VRE)],   'VRE',   c(0, 1)) + labs(title = "VRE")

# Combine into a 2-row by 3-column layout
six_panel_plot <- (p_ce | p_nrmse | p_crsq) / (p_cvre | p_vrsq | p_vre)

# Print and save
print(six_panel_plot)
ggsave("six_metric_summary_plot.png", six_panel_plot, width = 16, height = 10, dpi = 300)






## bar plot

# Extract mean values of the 6 metrics (excluding NAs)
metric_means <- scoreDT[, .(
  CE    = mean(CE, na.rm = TRUE),
  nRMSE = mean(nRMSE, na.rm = TRUE),
  CRSQ  = mean(CRSQ, na.rm = TRUE),
  CVRE  = mean(CVRE, na.rm = TRUE),
  VRSQ  = mean(VRSQ, na.rm = TRUE),
  VRE   = mean(VRE, na.rm = TRUE)
)]

# Convert to long format for ggplot
metric_means_long <- melt(metric_means, variable.name = "Metric", value.name = "MeanValue")

# Create the bar plot
bar_plot <- ggplot(metric_means_long, aes(x = Metric, y = MeanValue, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_viridis_d(option = "magma", direction = -1) +
  labs(title = "Average Value of Verification Metrics Across Stations",
       y = "Mean Value", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

# Show and save
print(bar_plot)
ggsave("verification_metrics_barplot.png", bar_plot, width = 8, height = 6, dpi = 300)


#######

# Extract mean values of the 6 metrics (excluding NAs)
metric_means <- scoreDT[, .(
  CE    = mean(CE, na.rm = TRUE),
  nRMSE = mean(nRMSE, na.rm = TRUE),
  CRSQ  = mean(CRSQ, na.rm = TRUE),
  CVRE  = mean(CVRE, na.rm = TRUE),
  VRSQ  = mean(VRSQ, na.rm = TRUE),
  VRE   = mean(VRE, na.rm = TRUE)
)]

# Convert to long format for ggplot
metric_means_long <- melt(metric_means, variable.name = "Metric", value.name = "MeanValue")

# Create the bar plot (all blue bars)
bar_plot <- ggplot(metric_means_long, aes(x = Metric, y = MeanValue)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", fill = "steelblue") +
  labs(title = "Average Value of Verification Metrics Across Stations",
       y = "Mean Value", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

# Show and save
print(bar_plot)
ggsave("verification_metrics_barplot.png", bar_plot, width = 8, height = 6, dpi = 300)

###


# Extract mean values of the 6 metrics (excluding NAs)
metric_means <- scoreDT[, .(
  CE    = mean(CE, na.rm = TRUE),
  nRMSE = mean(nRMSE, na.rm = TRUE),
  CRSQ  = mean(CRSQ, na.rm = TRUE),
  CVRE  = mean(CVRE, na.rm = TRUE),
  VRSQ  = mean(VRSQ, na.rm = TRUE),
  VRE   = mean(VRE, na.rm = TRUE)
)]

# Convert to long format for ggplot
metric_means_long <- melt(metric_means, variable.name = "Metric", value.name = "MeanValue")

bar_plot <- ggplot(metric_means_long, aes(x = Metric, y = MeanValue)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", fill = "steelblue") +
  geom_text(aes(label = round(MeanValue, 3)), vjust = -0.5, size = 3.8) +
  labs(title = "Average Value of Verification Metrics Across Stations",
       y = "Mean Value", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

# Show and save
print(bar_plot)
ggsave("verification_metrics_barplot.png", bar_plot, width = 8, height = 6, dpi = 300)






#######

#subpolots


# Add individual plot labels in titles
p_ce    <- create_metric_plot(scoreDT[!is.na(CE)],    'CE',    c(-1, 1)) + labs(title = "2a. CE")
p_nrmse <- create_metric_plot(scoreDT[!is.na(nRMSE)], 'nRMSE', c(0, 2)) + labs(title = "2b. nRMSE")
p_crsq  <- create_metric_plot(scoreDT[!is.na(CRSQ)],  'CRSQ',  c(0, 1)) + labs(title = "2c. CRSQ")
p_cvre  <- create_metric_plot(scoreDT[!is.na(CVRE)],  'CVRE',  c(0, 1)) + labs(title = "2d. CVRE")
p_vrsq  <- create_metric_plot(scoreDT[!is.na(VRSQ)],  'VRSQ',  c(0, 1)) + labs(title = "2e. VRSQ")
p_vre   <- create_metric_plot(scoreDT[!is.na(VRE)],   'VRE',   c(0, 1)) + labs(title = "2f. VRE")

# Combine with patchwork layout
six_panel_plot <- (p_ce | p_nrmse | p_crsq) / (p_cvre | p_vrsq | p_vre)

# Optional: Add overall annotation if needed
six_panel_plot <- six_panel_plot + plot_annotation(title = "Figure 2: Verification Statistics Across Stations")

# Show and save
print(six_panel_plot)
ggsave("six_metric_summary_plot_labeled.png", six_panel_plot, width = 16, height = 10, dpi = 300)





#Also there are too many metrics here. Maybe narrow down to 3 or 4. 
#I’d say R2, RE, and CE would be sufficient.


#These are the 3 colors


# Create individual plots with labels
p_crsq <- create_metric_plot(scoreDT[!is.na(CRSQ)], 'R2', c(0, 1)) + labs(title = "2a. Calibration R²")
p_vre  <- create_metric_plot(scoreDT[!is.na(VRE)],  'RE',  c(0, 1))  + labs(title = "2b. Reduction of Error (RE)")
p_ce   <- create_metric_plot(scoreDT[!is.na(CE)],   'CE',   c(-1, 1)) + labs(title = "2c. Coefficient of Efficiency (CE)")

# Combine in one row using patchwork
three_panel_plot <- p_crsq | p_vre | p_ce

# Add overall title
three_panel_plot <- three_panel_plot + 
  plot_annotation(title = "Figure 2: Core Verification Statistics Across Stations")

# Show and save
print(three_panel_plot)
ggsave("core_verification_stats_plot.png", three_panel_plot, width = 15, height = 5.5, dpi = 300)


#vert


# Create individual plots with labels
p_crsq <- create_metric_plot(scoreDT[!is.na(R2)], 'R2', c(0, 1)) + 
  labs(title = "2a. Calibration R² (CRSQ")

p_vre  <- create_metric_plot(scoreDT[!is.na(VRE)], 'RE', c(0, 1)) + 
  labs(title = "2b. Reduction of Error (RE)")

p_ce   <- create_metric_plot(scoreDT[!is.na(CE)], 'CE', c(-1, 1)) + 
  labs(title = "2c. Coefficient of Efficiency (CE)")

# Combine vertically using patchwork
three_panel_plot <- p_crsq / p_vre / p_ce

# Add overall title
three_panel_plot <- three_panel_plot + 
  plot_annotation(title = "Figure 2: Core Verification Statistics Across Stations")

# Show the plot
print(three_panel_plot)

# Save the plot to file
ggsave("core_verification_stats_plot_vertical.png", 
       plot = three_panel_plot, 
       width = 7, height = 12, dpi = 300)

######


create_metric_plot <- function(data, metric, fill_limits = NULL) {
  us_map <- map_data("state")
  
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = 'white', color = "black") +
    geom_point(data = data, aes(x = Qlon, y = Qlat, color = get(metric)), size = 3) +
    scale_color_viridis_c(name = metric, limits = fill_limits) +
    coord_fixed(1.3, xlim = c(-125, -66.9), ylim = c(24.4, 49.4)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}
p_crsq <- create_metric_plot(scoreDT[!is.na(R2)], 'R2', c(0, 1)) + 
  labs(title = "2a. Calibration R² (CRSQ)")

p_vre  <- create_metric_plot(scoreDT[!is.na(VRE)], 'VRE', c(0, 1)) + 
  labs(title = "2b. Reduction of Error (RE)")

p_ce   <- create_metric_plot(scoreDT[!is.na(CE)], 'CE', c(-1, 1)) + 
  labs(title = "2c. Coefficient of Efficiency (CE)")


### rolling window

library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)

# Load data
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge recon & obs
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Define rolling CV metric function
calc_rolling_metrics <- function(dt, window_size = 15) {
  years <- sort(unique(dt$year))
  n_years <- length(years)
  if (n_years < window_size + 2) return(data.table(CRSQ = NA_real_, VRE = NA_real_, CE = NA_real_))
  
  metric_list <- list()
  for (start in 1:(n_years - window_size + 1)) {
    val_years <- years[start:(start + window_size - 1)]
    cal_years <- setdiff(years, val_years)
    
    cal <- dt[year %in% cal_years]
    val <- dt[year %in% val_years]
    
    if (nrow(cal) < 2 || nrow(val) < 1) next
    
    Qa_cal <- cal$Qa; Q_cal <- cal$Q
    Qa_val <- val$Qa; Q_val <- val$Q
    
    Q_cal_scaled <- mean(Qa_cal) + ((Q_cal - mean(Q_cal)) * sd(Qa_cal) / sd(Q_cal))
    Q_val_scaled <- mean(Qa_val) + ((Q_val - mean(Q_val)) * sd(Qa_val) / sd(Q_val))
    
    CRSQ <- 1 - sum((Qa_cal - Q_cal_scaled)^2) / sum((Qa_cal - mean(Qa_cal))^2)
    VRE  <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_cal))^2)
    CE   <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_val))^2)
    
    metric_list[[length(metric_list) + 1]] <- data.table(CRSQ = CRSQ, VRE = VRE, CE = CE)
  }
  
  return(rbindlist(metric_list)[, lapply(.SD, mean, na.rm = TRUE)])
}

# Run per-station
rolling_metrics_by_station <- merged[, calc_rolling_metrics(.SD, window_size = 15), by = ID]

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge with rolling CV metrics
scoreDT <- merge(scoreDT, rolling_metrics_by_station, by = "ID", all.x = TRUE)

# CLEAN UP conflicting columns
if ("CE.y" %in% names(scoreDT)) scoreDT[, CE := CE.y]
if ("VRE.y" %in% names(scoreDT)) scoreDT[, VRE := VRE.y]
if ("CRSQ.y" %in% names(scoreDT)) scoreDT[, CRSQ := CRSQ.y]
scoreDT[, c("CE.x", "CE.y", "VRE.x", "VRE.y", "CRSQ.x", "CRSQ.y") := NULL]

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# US map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Map plotting function
create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)),
               size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = NULL, limits = scale_limits,
                          breaks = scales::pretty_breaks(3),
                          na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

#######



library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)

# Load data
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge recon & obs
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Define rolling CV metric function
calc_rolling_metrics <- function(dt, window_size = 15) {
  years <- sort(unique(dt$year))
  n_years <- length(years)
  if (n_years < window_size + 2) return(data.table(CRSQ = NA_real_, VRE = NA_real_, CE = NA_real_))
  
  metric_list <- list()
  for (start in 1:(n_years - window_size + 1)) {
    val_years <- years[start:(start + window_size - 1)]
    cal_years <- setdiff(years, val_years)
    
    cal <- dt[year %in% cal_years]
    val <- dt[year %in% val_years]
    
    if (nrow(cal) < 2 || nrow(val) < 1) next
    
    Qa_cal <- cal$Qa; Q_cal <- cal$Q
    Qa_val <- val$Qa; Q_val <- val$Q
    
    Q_cal_scaled <- mean(Qa_cal) + ((Q_cal - mean(Q_cal)) * sd(Qa_cal) / sd(Q_cal))
    Q_val_scaled <- mean(Qa_val) + ((Q_val - mean(Q_val)) * sd(Qa_val) / sd(Q_val))
    
    CRSQ <- 1 - sum((Qa_cal - Q_cal_scaled)^2) / sum((Qa_cal - mean(Qa_cal))^2)
    VRE  <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_cal))^2)
    CE   <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_val))^2)
    
    metric_list[[length(metric_list) + 1]] <- data.table(CRSQ = CRSQ, VRE = VRE, CE = CE)
  }
  
  return(rbindlist(metric_list)[, lapply(.SD, mean, na.rm = TRUE)])
}

# Run per-station
rolling_metrics_by_station <- merged[, calc_rolling_metrics(.SD, window_size = 15), by = ID]

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge with rolling CV metrics
scoreDT <- merge(scoreDT, rolling_metrics_by_station, by = "ID", all.x = TRUE)

# CLEAN UP conflicting columns
if ("CE.y" %in% names(scoreDT)) scoreDT[, CE := CE.y]
if ("VRE.y" %in% names(scoreDT)) scoreDT[, VRE := VRE.y]
if ("CRSQ.y" %in% names(scoreDT)) scoreDT[, CRSQ := CRSQ.y]
scoreDT[, c("CE.x", "CE.y", "VRE.x", "VRE.y", "CRSQ.x", "CRSQ.y") := NULL]

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# US map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Map plotting function
create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)),
               size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = NULL, limits = scale_limits,
                          breaks = scales::pretty_breaks(3),
                          na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

# Create the 3 rolling CV plots
p_crsq <- create_metric_plot(scoreDT[!is.na(CRSQ)], 'CRSQ', c(0, 1)) + 
  labs(title = "2a. Calibration R² (CRSQ)")

p_vre <- create_metric_plot(scoreDT[!is.na(VRE)], 'VRE', c(0, 1)) + 
  labs(title = "2b. Reduction of Error (RE)")

p_ce <- create_metric_plot(scoreDT[!is.na(CE)], 'CE', c(-1, 1)) + 
  labs(title = "2c. Coefficient of Efficiency (CE)")

# Combine vertically
three_panel_plot <- p_crsq / p_vre / p_ce +
  plot_annotation(title = "Figure 2: Core Verification Statistics (Rolling Window Cross-Validation)")

# Print and save
print(three_panel_plot)
ggsave("core_verification_stats_rolling_cv.png", 
       plot = three_panel_plot, width = 7, height = 12, dpi = 300)


















########### Taeb

# Create a compact summary table of rolling CV stats
metrics_table <- scoreDT[, .(ID, CRSQ, VRE, CE)]

# View in console
print(metrics_table)

# Optionally: show as interactive table (if in RStudio or notebook)
View(metrics_table)

# Or: save to CSV for review
fwrite(metrics_table, "rolling_cv_metrics_summary.csv")

##

# Calculate mean and median of CRSQ, VRE, and CE
metric_stats <- scoreDT[, .(
  Mean_CRSQ = mean(CRSQ, na.rm = TRUE),
  Median_CRSQ = median(CRSQ, na.rm = TRUE),
  
  Mean_VRE = mean(VRE, na.rm = TRUE),
  Median_VRE = median(VRE, na.rm = TRUE),
  
  Mean_CE = mean(CE, na.rm = TRUE),
  Median_CE = median(CE, na.rm = TRUE)
)]

# Print result
print(metric_stats)
#  Mean_CRSQ   Median_CRSQ   Mean_VRE  Median_VRE  Mean_CE   Median_CE
#<
#     0.5111      0.5273      0.5742      0.568    0.4717     0.4626
CE_range <- range(scoreDT$CE, na.rm = TRUE)
cat("Minimum CE:", CE_range[1], "\n")
cat("Maximum CE:", CE_range[2], "\n")











library(data.table)
library(ggplot2)
library(reshape2)  # For melt if not using data.table::melt

# Extract mean values of the 3 metrics (excluding NAs)
metric_means <- scoreDT[, .(
  CE    = mean(CE, na.rm = TRUE),
  CRSQ  = mean(CRSQ, na.rm = TRUE),
  VRE   = mean(VRE, na.rm = TRUE)
)]

# Convert to long format for ggplot
metric_means_long <- melt(metric_means, variable.name = "Metric", value.name = "MeanValue")

# Create the bar plot with all bars in blue
bar_plot <- ggplot(metric_means_long, aes(x = Metric, y = MeanValue)) +
  geom_bar(stat = "identity", width = 0.6, fill = "steelblue", color = "black") +
  labs(title = "Average Value of Verification Metrics Across Stations",
       y = "Mean Value", x = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

# Show and save
print(bar_plot)
ggsave("verification_metrics_barplot_blue.png", bar_plot, width = 8, height = 6, dpi = 300)















######

library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

# Example station list (replace with your actual station vector)
stations <- unique(your_data$station_name)  # Replace `your_data` with your real data frame
n_stations <- length(stations)

# Utility: Adjust each time series to 175 years, ending at 0
adjust_series_to_175 <- function(df, start_year = 1200) {
  full_years <- start_year:(start_year + 174)
  padded <- data.frame(year = full_years) %>%
    left_join(df, by = "year")
  
  padded$value[175] <- 0  # Force last value to 0
  return(padded)
}

# Create paired time series plots per station
paired_horizontal <- map(stations, function(station) {
  df <- your_data %>% filter(station_name == station)  # Replace with your full data source
  
  adjusted_df <- adjust_series_to_175(df)
  
  ggplot(adjusted_df, aes(x = year, y = value)) +
    geom_line(color = "blue") +
    labs(title = station, x = "Year", y = "Value") +
    theme_minimal()
})

# Combine into one tall layout with shared legend
final_combined <- Reduce(`/`, paired_horizontal) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

# Display
print(final_combined)

# Save to file
ggsave("combined_plot_horizontal_pairing_taller.png",
       plot = final_combined,
       width = 14,
       height = 4.5 * n_stations,  # Scale height by number of stations
       dpi = 300)











##########



library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)

# Load data
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge recon & obs
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Define rolling CV metric function
calc_rolling_metrics <- function(dt, window_size = 15) {
  years <- sort(unique(dt$year))
  n_years <- length(years)
  if (n_years < window_size + 2) return(data.table(CRSQ = NA_real_, VRE = NA_real_, CE = NA_real_))
  
  metric_list <- list()
  for (start in 1:(n_years - window_size + 1)) {
    val_years <- years[start:(start + window_size - 1)]
    cal_years <- setdiff(years, val_years)
    
    cal <- dt[year %in% cal_years]
    val <- dt[year %in% val_years]
    
    if (nrow(cal) < 2 || nrow(val) < 1) next
    
    Qa_cal <- cal$Qa; Q_cal <- cal$Q
    Qa_val <- val$Qa; Q_val <- val$Q
    
    Q_cal_scaled <- mean(Qa_cal) + ((Q_cal - mean(Q_cal)) * sd(Qa_cal) / sd(Q_cal))
    Q_val_scaled <- mean(Qa_val) + ((Q_val - mean(Q_val)) * sd(Qa_val) / sd(Q_val))
    
    CRSQ <- 1 - sum((Qa_cal - Q_cal_scaled)^2) / sum((Qa_cal - mean(Qa_cal))^2)
    VRE  <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_cal))^2)
    CE   <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_val))^2)
    
    metric_list[[length(metric_list) + 1]] <- data.table(CRSQ = CRSQ, VRE = VRE, CE = CE)
  }
  
  return(rbindlist(metric_list)[, lapply(.SD, mean, na.rm = TRUE)])
}

# Run per-station
rolling_metrics_by_station <- merged[, calc_rolling_metrics(.SD, window_size = 15), by = ID]

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge with rolling CV metrics
scoreDT <- merge(scoreDT, rolling_metrics_by_station, by = "ID", all.x = TRUE)

# CLEAN UP conflicting columns
if ("CE.y" %in% names(scoreDT)) scoreDT[, CE := CE.y]
if ("VRE.y" %in% names(scoreDT)) scoreDT[, VRE := VRE.y]
if ("CRSQ.y" %in% names(scoreDT)) scoreDT[, CRSQ := CRSQ.y]
scoreDT[, c("CE.x", "CE.y", "VRE.x", "VRE.y", "CRSQ.x", "CRSQ.y") := NULL]

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# US map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Map plotting function
create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)),
               size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = NULL, limits = scale_limits,
                          breaks = scales::pretty_breaks(3),
                          na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

# Create the 3 rolling CV plots in desired order: CE, CRSQ, VRE
p_ce   <- create_metric_plot(scoreDT[!is.na(CE)], 'CE', c(-1, 1)) +
  labs(title = "2a. Coefficient of Efficiency (CE)")

p_crsq <- create_metric_plot(scoreDT[!is.na(CRSQ)], 'CRSQ', c(0, 1)) +
  labs(title = "2b. Calibration R² (CRSQ)")

p_vre  <- create_metric_plot(scoreDT[!is.na(VRE)], 'VRE', c(0, 1)) +
  labs(title = "2c. Validation Reduction of Error (VRE)")

# Combine vertically in new order
three_panel_plot <- p_ce / p_crsq / p_vre +
  plot_annotation(title = "Core Verification Statistics")

# Print and save
print(three_panel_plot)
ggsave("core_verification_stats_rolling_cv.png", 
       plot = three_panel_plot, width = 7, height = 12, dpi = 300)

####################











#table with the updated table 



##################

library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)

recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge recon & obs
merged <- merge(obs[, .(ID, year, Qa)],
                recon[, .(ID, year, Q)],
                by = c("ID", "year"))

calc_rolling_metrics <- function(dt, window_size = 15) {
  years <- sort(unique(dt$year))
  n_years <- length(years)
  if (n_years < window_size + 2) return(data.table(CRSQ = NA_real_, VRE = NA_real_, CE = NA_real_))
  
  metric_list <- list()
  for (start in 1:(n_years - window_size + 1)) {
    val_years <- years[start:(start + window_size - 1)]
    cal_years <- setdiff(years, val_years)
    
    cal <- dt[year %in% cal_years]
    val <- dt[year %in% val_years]
    
    if (nrow(cal) < 2 || nrow(val) < 1) next
    
    Qa_cal <- cal$Qa; Q_cal <- cal$Q
    Qa_val <- val$Qa; Q_val <- val$Q
    
    Q_cal_scaled <- mean(Qa_cal) + ((Q_cal - mean(Q_cal)) * sd(Qa_cal) / sd(Q_cal))
    Q_val_scaled <- mean(Qa_val) + ((Q_val - mean(Q_val)) * sd(Qa_val) / sd(Q_val))
    
    CRSQ <- 1 - sum((Qa_cal - Q_cal_scaled)^2) / sum((Qa_cal - mean(Qa_cal))^2)
    VRE  <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_cal))^2)
    CE   <- 1 - sum((Qa_val - Q_val_scaled)^2) / sum((Qa_val - mean(Qa_val))^2)
    
    metric_list[[length(metric_list) + 1]] <- data.table(CRSQ = CRSQ, VRE = VRE, CE = CE)
  }
  
  return(rbindlist(metric_list)[, lapply(.SD, mean, na.rm = TRUE)])
}

# --- Calculate metrics ---
rolling_metrics_by_station <- merged[, calc_rolling_metrics(.SD, window_size = 15), by = ID]

scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge with rolling CV metrics
scoreDT <- merge(scoreDT, rolling_metrics_by_station, by = "ID", all.x = TRUE)

if ("CE.y" %in% names(scoreDT)) scoreDT[, CE := CE.y]
if ("VRE.y" %in% names(scoreDT)) scoreDT[, VRE := VRE.y]
if ("CRSQ.y" %in% names(scoreDT)) scoreDT[, CRSQ := CRSQ.y]
scoreDT[, c("CE.x", "CE.y", "VRE.x", "VRE.y", "CRSQ.x", "CRSQ.y") := NULL]

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

create_metric_plot <- function(data, metric, scale_limits) {
  us_map <- map_data("state")
  
  breaks_vec <- if (metric == "CE") c(-1, 0, 1) else scales::pretty_breaks(3)(scale_limits)
  
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
                 fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)),
               size = 4, shape = 19) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_viridis_c(name = NULL, limits = scale_limits,
                          breaks = breaks_vec,
                          na.value = "grey80", option = "magma") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

p_ce <- create_metric_plot(scoreDT[!is.na(CE)], 'CE', c(-1, 1)) +
  labs(title = "2a. Coefficient of Efficiency (CE)")

p_crsq <- create_metric_plot(scoreDT[!is.na(CRSQ)], 'CRSQ', c(-1, 1)) +
  labs(title = "2b. Calibration R² (CRSQ)")

p_vre <- create_metric_plot(scoreDT[!is.na(VRE)], 'VRE', c(-1, 1)) +
  labs(title = "2c. Validation Reduction of Error (VRE)")


three_panel_plot <- p_ce / p_crsq / p_vre +
  plot_annotation(title = "Core Verification Statistics")


print(three_panel_plot)
ggsave("core_verification_stats_rolling_cv.png", 
       plot = three_panel_plot, width = 7, height = 12, dpi = 500)

#######


# --- Build a publishable table of rolling-CV metrics (does NOT modify scoreDT) ---
res <- data.table::copy(scoreDT)

# Pick a reasonable station-name column if it exists; otherwise leave blank
name_candidates <- intersect(names(res), c("station_name","Station","station","site_name","STATION_NAME","name","station"))
if (length(name_candidates) > 0) {
  res[, Station := get(name_candidates[1])]
} else {
  res[, Station := NA_character_]
}

# Prefer gauge coords if present; otherwise fall back to lon/lat
if (!("Qlon" %in% names(res)) && ("lon" %in% names(res))) res[, Qlon := lon]
if (!("Qlat" %in% names(res)) && ("lat" %in% names(res))) res[, Qlat := lat]

# Keep only rows with metrics available
res <- res[!is.na(CE) | !is.na(VRE) | !is.na(CRSQ)]

# Assemble the table
results_table <- res[
  , .(
    ID,
    Station,
    Lon = round(Qlon, 2),
    Lat = round(Qlat, 2),
    CRSQ = round(CRSQ, 3),
    VRE  = round(VRE,  3),
    CE   = round(CE,   3),
    `CE > 0` = ifelse(is.na(CE), NA, ifelse(CE > 0, "Yes", "No"))
  )
]

# Sort (top skill first); change to -CRSQ or -VRE if you prefer a different sort
data.table::setorder(results_table, -CE)

# Print to console
print(results_table)

# Optional: View in RStudio (will be ignored on headless servers)
if (interactive()) View(results_table)

# Save to CSV for Word/Excel (put wherever you want)
data.table::fwrite(results_table, "core_verification_stats_rolling_cv_table.csv")




# === Three-statistics table with station names ===
library(data.table)

res <- data.table::copy(scoreDT)

# Pick a station-name column if it exists
name_candidates <- intersect(names(res),
                             c("station_name","Station","station","site_name","STATION_NAME","name"))
if (length(name_candidates) > 0) {
  res[, Station := get(name_candidates[1])]
} else {
  res[, Station := NA_character_]
}

three_stats <- res[
  , .(
    ID,
    Station,
    CRSQ = round(CRSQ, 3),
    VRE  = round(VRE,  3),
    CE   = round(CE,   3)
  )
]

# Optional: sort (change to -CE if you prefer by skill)
data.table::setorder(three_stats, Station, ID)

# Show and save
print(three_stats)
data.table::fwrite(three_stats, "three_stats_by_station.csv")

# Optional: View in RStudio (will be ignored on headless servers)
if (interactive()) View(three_stats)
