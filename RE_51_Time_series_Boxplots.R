####### 

#THIS CODE IS OUTDATED AND USED IN PYTHON FOR BETTER TIME SERIES

########


#This is all the data from reconstructions only

all_data <- fread("/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")
reconst <- fread("/work/bmcdaniel/R_stuff/reconst.csv")

setDT(all_data)
setDT(reconst)

# Merge dataset by both "ID" and "year" to compare the two Q values
merged_data <- merge(all_data[, .(ID, year, station_nm, Q)], 
                     reconst[, .(ID, year, Q)], 
                     by = c("ID", "year"), 
                     all.x = TRUE, 
                     suffixes = c("_all_data", "_reconst"))

# Check the structure of the merged data
str(merged_data)
stations <- unique(merged_data$station_nm)
plot_list <- list()

# Loop through each station
for (i in seq_along(stations)) {
  station <- stations[i]
  station_data <- merged_data[station_nm == station]
  # Calculate the correlation coefficient
  correlation <- cor(station_data$Q_all_data, station_data$Q_reconst, use = "complete.obs")
  # plot
  p <- ggplot(station_data) +
    geom_line(aes(x = year, y = Q_reconst, color = "Observed (Reconst)"), size = 0.5) +  # Observed Q
    geom_line(aes(x = year, y = Q_all_data, color = "Reconstructed (All Data)"), size = 0.5) +  # Reconstructed Q
    labs(title = paste("Reconstucted Streamflow for Station", station),
         x = "Year",
         y = "Streamflow",
         color = "Legend") +
    scale_color_manual(values = c("Reconstructed (All Data)" = "blue")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"), # Increase size of plot title
      axis.text = element_text(size = 10), # Increase axis label size
      axis.title = element_text(size = 12) # Increase axis title size
    ) +
    annotate("text", x = -Inf, y = Inf, label = paste("R:", round(correlation, 2)), 
             hjust = -0.1, vjust = 2, size = 5, color = "black")
  
  plot_list[[i]] <- p
  
  # Print the plots in groups of 3
  if (i %% 3 == 0 || i == length(stations)) {
    grid.arrange(grobs = plot_list[max(1, i-2):i], ncol = 1)
  }
}



write.csv(merged_data, "/work/bmcdaniel/R_stuff/merged_data_dnames.csv", row.names = FALSE)


merged_data

sf <- fread("/work/bmcdaniel/R_stuff/merged_data_dnames.csv")

sf
View(sf)

#THe compared data
all_data <- fread("/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")
combined_data <- fread("/work/bmcdaniel/R_stuff/combined_data_data.csv")

setDT(all_data)
setDT(combined_data)


#Convert monthly data (Qm) to annual by averaging Qm over each year
combined_data_annual <- combined_data[, .(Q_annual = mean(Qm, na.rm = TRUE)), by = .(ID, year)]

# Merge the annualized data with all_data by ID and year
merged_data <- merge(all_data[, .(ID, year, station_nm, Q)], 
                     combined_data_annual[, .(ID, year, Q_annual)], 
                     by = c("ID", "year"), 
                     all.x = TRUE)
str(merged_data)

# List of unique stations
stations <- unique(merged_data$station_nm)
plot_list <- list()

for (i in seq_along(stations)) {
  station <- stations[i]
  
  station_data <- merged_data[station_nm == station]
  
  # Calculate the correlation coefficient 
  correlation <- cor(station_data$Q, station_data$Q_annual, use = "complete.obs")
  
  # ploy
  p <- ggplot(station_data) +
    geom_line(aes(x = year, y = Q, color = "Reconstructed"), size = 0.5) +  
    geom_line(aes(x = year, y = Q_annual, color = "Observed"), size = 0.5) +  
    labs(title = paste("Observed vs Reconstructed Streamflow for Station", station),
         x = "Year",
         y = "Streamflow",
         color = "Legend") +
    scale_color_manual(values = c("Reconstructed" = "blue", "Observed" = "red")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"), 
      axis.text = element_text(size = 10), 
      axis.title = element_text(size = 12) 
    ) +
    annotate("text", x = -Inf, y = Inf, label = paste("R:", round(correlation, 2)), 
             hjust = -0.1, vjust = 2, size = 5, color = "black")
  
  plot_list[[i]] <- p
  
  # Print the plots in groups of 3
  if (i %% 3 == 0 || i == length(stations)) {
    grid.arrange(grobs = plot_list[max(1, i-2):i], ncol = 1)
  }
}





#box plot


all_data <- fread("/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")
combined_data <- fread("/work/bmcdaniel/R_stuff/combined_data_data.csv")

setDT(all_data)
setDT(combined_data)

# Step 1: Convert to year
combined_data_annual <- combined_data[, .(Q_annual = mean(Qm, na.rm = TRUE)), by = .(ID, year)]

# Step 2: Merge the annualized data with all_data by ID and year
merged_data <- merge(all_data[, .(ID, year, station_nm, Q)], 
                     combined_data_annual[, .(ID, year, Q_annual)], 
                     by = c("ID", "year"), 
                     all.x = TRUE)

# Step 3: ilter the overlapping data 
overlap_data <- merged_data[!is.na(Q) & !is.na(Q_annual)]

# Step 4: Melt the data into long format for easier plotting
melted_data <- melt(overlap_data, id.vars = c("ID", "year", "station_nm"), 
                    measure.vars = c("Q", "Q_annual"), 
                    variable.name = "Type", value.name = "Streamflow")

# Step 5: Create boxplots for each station
for (station in unique(melted_data$station_nm)) {
  station_data <- melted_data[station_nm == station]
  
  # Step 6: Create the boxplot for the current station
  p <- ggplot(station_data, aes(x = Type, y = Streamflow, fill = Type)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of Observed vs Reconstructed Streamflow for Station", station),
         x = "Streamflow Type",
         y = "Streamflow") +
    scale_fill_manual(values = c("Q" = "blue", "Q_annual" = "red")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"), # Increaseu title size
      axis.text = element_text(size = 10), # Increase the e axis text size
      axis.title = element_text(size = 12), # Increase axisl title size
      legend.position = "none" 
    )
  
  
  print(p)
}


















# ---- Libraries ----
library(data.table)
library(ggplot2)
library(gridExtra)
library(reshape2)

# ---- Load Datasets ----
all_data     <- fread("/work/bmcdaniel/R_stuff/all_data_with_all_transformations.csv")
reconst      <- fread("/work/bmcdaniel/R_stuff/reconst.csv")
combined_raw <- fread("/work/bmcdaniel/R_stuff/combined_data_data.csv")

# ---- Merge Reconstructed vs All Data ----
merged_reconst <- merge(
  all_data[, .(ID, year, station_nm, Q)],
  reconst[, .(ID, year, Q)],
  by = c("ID", "year"),
  all.x = TRUE,
  suffixes = c("_all", "_reconst")
)

# ---- Plot Comparison: Reconstructed vs All Data ----
for (station in unique(merged_reconst$station_nm)) {
  df <- merged_reconst[station_nm == station]
  r <- cor(df$Q_all, df$Q_reconst, use = "complete.obs")
  
  p <- ggplot(df, aes(x = year)) +
    geom_line(aes(y = Q_reconst, color = "Observed (Reconst)"), size = 0.5) +
    geom_line(aes(y = Q_all, color = "Reconstructed (All Data)"), size = 0.5) +
    labs(title = paste("Reconstructed Streamflow for", station),
         x = "Year", y = "Streamflow", color = "Legend") +
    scale_color_manual(values = c("Reconstructed (All Data)" = "blue")) +
    annotate("text", x = -Inf, y = Inf, label = paste("R:", round(r, 2)), hjust = -0.1, vjust = 2, size = 5) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12))
  
  print(p)
}

# ---- Save Comparison Output ----
fwrite(merged_reconst, "/work/bmcdaniel/R_stuff/merged_data_dnames.csv")

# ---- Merge Observed Annual with Reconstructed ----
combined_annual <- combined_raw[, .(Q_annual = mean(Qm, na.rm = TRUE)), by = .(ID, year)]
merged_combined <- merge(
  all_data[, .(ID, year, station_nm, Q)],
  combined_annual,
  by = c("ID", "year"),
  all.x = TRUE
)

# ---- Plot Observed vs Reconstructed (Annual) ----
for (station in unique(merged_combined$station_nm)) {
  df <- merged_combined[station_nm == station]
  r <- cor(df$Q, df$Q_annual, use = "complete.obs")
  
  p <- ggplot(df, aes(x = year)) +
    geom_line(aes(y = Q, color = "Reconstructed"), size = 0.5) +
    geom_line(aes(y = Q_annual, color = "Observed"), size = 0.5) +
    labs(title = paste("Observed vs Reconstructed Streamflow for", station),
         x = "Year", y = "Streamflow", color = "Legend") +
    scale_color_manual(values = c("Reconstructed" = "blue", "Observed" = "red")) +
    annotate("text", x = -Inf, y = Inf, label = paste("R:", round(r, 2)), hjust = -0.1, vjust = 2, size = 5) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12))
  
  print(p)
}

# ---- Boxplots: Observed vs Reconstructed ----
overlap <- merged_combined[!is.na(Q) & !is.na(Q_annual)]
melted <- melt(overlap, id.vars = c("ID", "year", "station_nm"),
               measure.vars = c("Q", "Q_annual"),
               variable.name = "Type", value.name = "Streamflow")

for (station in unique(melted$station_nm)) {
  df <- melted[station_nm == station]
  
  p <- ggplot(df, aes(x = Type, y = Streamflow, fill = Type)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of Observed vs Reconstructed for", station),
         x = "Streamflow Type", y = "Streamflow") +
    scale_fill_manual(values = c("Q" = "blue", "Q_annual" = "red")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "none")
  
  print(p)
}
