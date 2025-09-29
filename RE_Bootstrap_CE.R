#CE stations 
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID')
### test

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("maps", quietly = TRUE)) install.packages("maps")
if (!requireNamespace("mapdata", quietly = TRUE)) install.packages("mapdata")
library(ggplot2)
library(maps)
library(mapdata)
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT$Qlon <- scoreDT$lon
  scoreDT$Qlat <- scoreDT$lat
}

lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = 'white', color = "black") +  # US map
    geom_point(data = data, aes(lon, lat, color = !!sym(metric)), size = 2) +  #
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)), size = 4, shape = 19,) +  # P
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_distiller(name = metric, palette = 'RdBu', direction = 1,
                          breaks = scales::pretty_breaks(3), limits = absRange(data[[metric]])) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

#Plot (CE)
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))

print(p_ce)



###################



###################
# This is the updated map
scoreDT[, Significance_Label := ifelse(Significant == 1, "Significant", "Not Significant")]

# Function to create the CE metric map
create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = 'white', color = "black") +  
    geom_point(data = data, aes(lon, lat, color = !!sym(metric), shape = factor(Significance_Label)), size = 2) +  
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric), shape = factor(Significance_Label)), size = 3) +  
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +  
    scale_color_distiller(name = metric, palette = 'RdBu', direction = 1, 
                          breaks = scales::pretty_breaks(3), 
                          limits = range(data[[metric]], na.rm = TRUE)) +  
    scale_shape_manual(values = c("Not Significant" = 16, "Significant" = 17), name = "") +  
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

# Create and display the CE score map
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))

print(p_ce)  # Show the updated map



#######




####### updated sig plot
library(ggplot2)

#significance labels
scoreDT[, Significance_Label := ifelse(Significant == 1, "Yes", "No")]

# CI interval plot
p_ci <- ggplot(scoreDT, aes(x = reorder(ID, CE), y = CE, color = factor(Significance_Label))) +
  geom_pointrange(aes(ymin = CE_Lower, ymax = CE_Upper), size = 0.8) +  # CI bars
  geom_point(size = 2) +  # Mean CE values
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # zero baseline
  scale_color_manual(values = c("No" = "gray", "Yes" = "red"), name = "Significance") +  
  coord_flip() +  # makes axis vertical
  labs(title = "Bootstrapped CE Values with 95% CI",
       x = "Station ID", y = "Coefficient of Efficiency (CE)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    legend.position = "right",  
    axis.text.y = element_text(size = 10)  
  )

print(p_ci)


######


#####

#significance table 

if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)
scoreDT_sorted <- scoreDT[order(CE)]
ci_table <- scoreDT_sorted[, .(ID, CE, CE_Lower, CE_Upper, Significance_Label)]

write_xlsx(ci_table, "Bootstrapped_CE_Results_Sorted.xlsx")

# Print
print(ci_table)

ci_table

View(ci_table)



########PCS Kept

total_pcs_created <- sum(sapply(pca_results, function(pca) {
  if (!is.null(pca)) ncol(pca$x) else 0
}))


print(paste("Total number of PCs created across all stations:", total_pcs_created))


# Create a data frame with station IDs, total PCs created, and PCs kept
pc_summary <- data.frame(
  Station = names(pca_results),
  TotalPCsCreated = sapply(pca_results, function(pca) {
    if (!is.null(pca)) ncol(pca$x) else NA
  }),
  PCsKept = sapply(ivs, function(selectedPC) length(selectedPC))
)

# Print the summary
print(pc_summary)

View(pc_summary)


########
#Variance explainedplot

library(ggplot2)

variance_explained <- data.frame(
  Station = names(pca_results),
  TotalVariance = sapply(pca_results, function(pca) {
    if (!is.null(pca)) sum(pca$sdev^2) else NA
  }),
  VarianceExplainedByKeptPCs = sapply(names(pca_results), function(s) {
    if (!is.null(pca_results[[s]]) && !is.null(ivs[[s]])) {
      sum(summary(pca_results[[s]])$importance[2, ivs[[s]]])  
    } else {
      NA
    }
  })
)

# Calculate % variance explained
variance_explained$PercentVarianceExplained <- variance_explained$VarianceExplainedByKeptPCs * 100

print(variance_explained)

ggplot(variance_explained, aes(x = Station, y = PercentVarianceExplained)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage of Variance Explained by Retained PCs",
       x = "Station",
       y = "Variance Explained (%)")






















# CE stations
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID')

# Define significance using CE > 0 threshold
scoreDT[, Significant := as.integer(CE > 0)]
scoreDT[, Significance_Label := ifelse(Significant == 1, "Significant", "Not Significant")]

# Load necessary libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("maps", quietly = TRUE)) install.packages("maps")
if (!requireNamespace("mapdata", quietly = TRUE)) install.packages("mapdata")
library(ggplot2)
library(maps)
library(mapdata)

# Add Qlon and Qlat columns if not present
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT$Qlon <- scoreDT$lon
  scoreDT$Qlat <- scoreDT$lat
}

# US map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Function to create CE map
create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = 'white', color = "black") +
    geom_point(data = data, aes(lon, lat, color = !!sym(metric), shape = factor(Significance_Label)), size = 2) +
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric), shape = factor(Significance_Label)), size = 3) +
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    scale_color_distiller(name = metric, palette = 'RdBu', direction = 1,
                          breaks = scales::pretty_breaks(3),
                          limits = range(data[[metric]], na.rm = TRUE)) +
    scale_shape_manual(values = c("Not Significant" = 16, "Significant" = 17), name = "") +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
}

# Plot CE map
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))
print(p_ce)

# CE bar plot (no CI since CE_Lower/Upper don't exist)
p_ci <- ggplot(scoreDT, aes(x = reorder(ID, CE), y = CE, color = factor(Significance_Label))) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("Not Significant" = "gray", "Significant" = "red"), name = "Significance") +
  coord_flip() +
  labs(title = "Cross-Validated CE by Station",
       x = "Station ID", y = "Coefficient of Efficiency (CE)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    axis.text.y = element_text(size = 10)
  )
print(p_ci)

# Export CE table (no CI)
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)
scoreDT_sorted <- scoreDT[order(CE)]
ci_table <- scoreDT_sorted[, .(ID, CE, Significance_Label)]
write_xlsx(ci_table, "Bootstrapped_CE_Results_Sorted.xlsx")
print(ci_table)
View(ci_table)

# PCA summary
total_pcs_created <- sum(sapply(pca_results, function(pca) {
  if (!is.null(pca)) ncol(pca$x) else 0
}))
print(paste("Total number of PCs created across all stations:", total_pcs_created))

pc_summary <- data.frame(
  Station = names(pca_results),
  TotalPCsCreated = sapply(pca_results, function(pca) {
    if (!is.null(pca)) ncol(pca$x) else NA
  }),
  PCsKept = sapply(ivs, function(selectedPC) length(selectedPC))
)
print(pc_summary)
View(pc_summary)

# Variance explained by retained PCs
variance_explained <- data.frame(
  Station = names(pca_results),
  TotalVariance = sapply(pca_results, function(pca) {
    if (!is.null(pca)) sum(pca$sdev^2) else NA
  }),
  VarianceExplainedByKeptPCs = sapply(names(pca_results), function(s) {
    if (!is.null(pca_results[[s]]) && !is.null(ivs[[s]])) {
      sum(summary(pca_results[[s]])$importance[2, ivs[[s]]])
    } else {
      NA
    }
  })
)
variance_explained$PercentVarianceExplained <- variance_explained$VarianceExplainedByKeptPCs * 100

print(variance_explained)
ggplot(variance_explained, aes(x = Station, y = PercentVarianceExplained)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Percentage of Variance Explained by Retained PCs",
       x = "Station", y = "Variance Explained (%)")










#################

# Export CE table (no CI)
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)
scoreDT_sorted <- scoreDT[order(CE)]
ci_table <- scoreDT_sorted[, .(ID, CE, Significance_Label)]
write_xlsx(ci_table, "Bootstrapped_CE_Results_Sorted.xlsx")
print(ci_table)
View(ci_table)








library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(maps)
library(writexl)

# Load data
recon <- fread('/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv')
obs   <- fread('/work/bmcdaniel/R_stuff/annual_streamflow_by_station.csv')

# Format station IDs
recon[, ID := sprintf("%08d", as.integer(ID))]
obs[,   ID := sprintf("%08d", as.integer(ID))]

# Merge recon & obs
merged <- merge(obs[, .(ID, year, Qa)], recon[, .(ID, year, Q)], by = c("ID", "year"))

# Rolling CV function (15-year blocks)
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

# Apply to each station
rolling_metrics_by_station <- merged[, calc_rolling_metrics(.SD, window_size = 15), by = ID]

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Merge in rolling CV metrics
scoreDT <- merge(scoreDT, rolling_metrics_by_station, by = "ID", all.x = TRUE)

# Clean conflicting columns
if ("CE.y" %in% names(scoreDT)) scoreDT[, CE := CE.y]
if ("VRE.y" %in% names(scoreDT)) scoreDT[, VRE := VRE.y]
if ("CRSQ.y" %in% names(scoreDT)) scoreDT[, CRSQ := CRSQ.y]
scoreDT[, c("CE.x", "CE.y", "VRE.x", "VRE.y", "CRSQ.x", "CRSQ.y") := NULL]

# Add lon/lat columns if missing
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# Define significance based on new CE
scoreDT[, Significant := as.integer(CE > 0)]
scoreDT[, Significance_Label := ifelse(Significant == 1, "Significant", "Not Significant")]

# US bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Mapping function
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

# Create rolling CV plots
p_crsq <- create_metric_plot(scoreDT[!is.na(CRSQ)], 'CRSQ', c(0, 1)) + 
  labs(title = "2a. Calibration R² (CRSQ)")

p_vre <- create_metric_plot(scoreDT[!is.na(VRE)], 'VRE', c(0, 1)) + 
  labs(title = "2b. Reduction of Error (RE)")

p_ce <- create_metric_plot(scoreDT[!is.na(CE)], 'CE', c(-1, 1)) + 
  labs(title = "2c. Coefficient of Efficiency (CE)")

# Combine and save
three_panel_plot <- p_crsq / p_vre / p_ce +
  plot_annotation(title = "Figure 2: Core Verification Statistics (Rolling Window Cross-Validation)")

print(three_panel_plot)
ggsave("core_verification_stats_rolling_cv.png", 
       plot = three_panel_plot, width = 7, height = 12, dpi = 300)

# Significance bar plot (no CI yet)
p_ci <- ggplot(scoreDT, aes(x = reorder(ID, CE), y = CE, color = factor(Significance_Label))) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("Not Significant" = "gray", "Significant" = "red"), name = "Significance") +
  coord_flip() +
  labs(title = "Cross-Validated CE by Station (Rolling CV)",
       x = "Station ID", y = "Coefficient of Efficiency (CE)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "right",
        axis.text.y = element_text(size = 10))
print(p_ci)

# Export CE results (rolling CV version)
scoreDT_sorted <- scoreDT[order(CE)]
ce_table <- scoreDT_sorted[, .(ID, CE, CRSQ, VRE, Significance_Label)]
write_xlsx(ce_table, "Rolling_CV_CE_Results.xlsx")
print(ce_table)
View(ce_table)




p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))
print(p_ce)



## CE map

# Custom CE map with significance coloring
p_ce_significance <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group),
               fill = 'white', color = "black") +
  geom_point(data = scoreDT, aes(Qlon, Qlat, fill = Significance_Label), 
             shape = 21, size = 4, color = "black", stroke = 0.5) +
  scale_fill_manual(values = c("Significant" = "red", "Not Significant" = "gray"),
                    name = "CE > 0 Significance") +
  coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
  labs(title = "2c. Coefficient of Efficiency (CE > 0 Significance)",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 13) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = "right")

# Show the map
print(p_ce_significance)

# Optionally overwrite the file
ggsave("rolling_ce_significance_map.png", plot = p_ce_significance,
       width = 7, height = 5, dpi = 300)













# Updated CE map with color = CE and shape = significance (circles only, no legend)
create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
                 fill = 'white', color = "black") +  
    
    # Outer black circle
    geom_point(data = data, aes(lon, lat, shape = factor(Significance_Label)), 
               size = 2.5, color = "black") +  
    
    # Inner CE-colored circle
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric), shape = factor(Significance_Label)), 
               size = 4) +
    
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    
    scale_color_distiller(name = "(CE)", 
                          palette = 'RdBu', direction = 1,
                          breaks = scales::pretty_breaks(3),
                          limits = c(-1, 1)) +   # <- Fixed color scale
    
    scale_shape_manual(values = c("Not Significant" = 16, "Significant" = 16),
                       name = NULL) +
    
    labs(x = "Longitude", y = "Latitude") +
    
    theme_minimal(base_size = 13) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.box = "vertical",
          legend.spacing.y = unit(0.3, 'cm'),
          legend.position = "right")
}

# Create and display updated CE plot
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))

print(p_ce)
########################











# Updated CE map with color = CE and shape = significance (circles only, no legend)
create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  ggplot() +
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
                 fill = 'white', color = "black") +  
    
    # Outer black circle by significance
    geom_point(data = data, aes(lon, lat, shape = factor(Significance_Label)), 
               size = 2.5, color = "black") +  
    
    # Inner CE-colored circle by significance
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric), shape = factor(Significance_Label)), 
               size = 4) +
    
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    
    # Fixed color scale
    scale_color_distiller(name = "(CE)", 
                          palette = 'RdBu', direction = 1,
                          breaks = c(-1, 0, 1),
                          limits = c(-1, 1)) +
    
    # Fixed circle shape for all (no legend displayed)
    scale_shape_manual(values = c("Not Significant" = 16, "Significant" = 16)) +
    guides(shape = "none") +  # <- Remove shape legend
    
    labs(x = "Longitude", y = "Latitude") +
    
    theme_minimal(base_size = 13) +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.box = "vertical",
          legend.spacing.y = unit(0.3, 'cm'),
          legend.position = "right")
}

# Create and display updated CE plot
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))

print(p_ce)

# Optional: Save to file
ggsave("ce_map_no_shape_legend.png", plot = p_ce, width = 7, height = 6, dpi = 300)
















################


# Load libraries
library(data.table)
library(ggplot2)
library(viridis)
library(maps)

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# Define map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Create CE plot function (no shape legend, circles only)
create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  
  ggplot() +
    # Background map
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
                 fill = 'white', color = "black") +
    
    # Outer black circle (visual border)
    geom_point(data = data, aes(lon, lat), size = 2.5, color = "black", shape = 16) +
    
    # Inner CE-colored circle
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)), 
               size = 4, shape = 16) +
    
    # Map projection
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    
    # Fixed CE color scale
    scale_color_distiller(name = "(CE)", palette = 'RdBu', direction = 1,
                          breaks = c(-1, 0, 1), limits = c(-1, 1)) +
    
    # Axes and theme
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 13) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.box = "vertical",
      legend.spacing.y = unit(0.3, 'cm'),
      legend.position = "right"
    )
}

# Generate CE map
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(
    plot.tag.position = c(0.16, 1.15),
    plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm')
  )

# Display and save
print(p_ce)
ggsave("ce_map_noshape.png", plot = p_ce, width = 7, height = 6, dpi = 300)

###############



# Load libraries
library(data.table)
library(ggplot2)
library(viridis)
library(maps)

# Load CE scores and metadata
scoreDT <- scores
scoreDT[, ID := availIDs]
scoreDT <- merge(scoreDT, instQmeta, by = 'ID', all.x = TRUE)

# Assign plotting coordinates if needed
if (!"Qlon" %in% names(scoreDT) | !"Qlat" %in% names(scoreDT)) {
  scoreDT[, Qlon := lon]
  scoreDT[, Qlat := lat]
}

# Define map bounds
lon_bounds <- c(-125, -66.93457)
lat_bounds <- c(24.396308, 49.384358)

# Create CE plot function (no shape legend, circles only)
create_metric_plot <- function(data, metric) {
  us_map <- map_data("state")
  
  ggplot() +
    # Background map
    geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
                 fill = 'white', color = "black") +
    
    # Outer black circle (visual border)
    geom_point(data = data, aes(lon, lat), size = 2.5, color = "black", shape = 16) +
    
    # Inner CE-colored circle
    geom_point(data = data, aes(Qlon, Qlat, color = !!sym(metric)), 
               size = 4, shape = 16) +
    
    # Map projection
    coord_fixed(xlim = lon_bounds, ylim = lat_bounds, ratio = 1.3) +
    
    # Fixed CE color scale
    scale_color_distiller(name = "(CE)", palette = 'RdBu', direction = 1,
                          breaks = c(-1, 0, 1), limits = c(-1, 1)) +
    
    # Axes and theme
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 13) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.box = "vertical",
      legend.spacing.y = unit(0.3, 'cm'),
      legend.position = "right"
    )
}

# Generate CE map
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(
    plot.tag.position = c(0.16, 1.15),
    plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm')
  )

# Display and save
print(p_ce)
ggsave("ce_map_noshape.png", plot = p_ce, width = 7, height = 6, dpi = 300)

