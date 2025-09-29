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




