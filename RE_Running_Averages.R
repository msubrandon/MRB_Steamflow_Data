#install.packages("zoo")
library(ggplot2)
library(dplyr)
library(zoo)

# Load the normalized data
data <- read.csv("/work/bmcdaniel/R_stuff/normalized_data.csv")

# Function to calculate running mean
calculate_running_mean <- function(data, window) {
  data %>%
    group_by(site_no) %>%
    arrange(year) %>%
    mutate(running_mean = rollmean(Q_normalized, k = window, fill = NA, align = 'right')) %>%
    ungroup()
}

# Calculate running means
data_20yr <- calculate_running_mean(data, 20)
data_50yr <- calculate_running_mean(data, 50)
data_100yr <- calculate_running_mean(data, 100)

# Combine the data for easier plotting
data_combined <- data_20yr %>%
  rename(running_mean_20yr = running_mean) %>%
  left_join(data_50yr %>% rename(running_mean_50yr = running_mean), by = c("site_no", "year", "Q_normalized")) %>%
  left_join(data_100yr %>% rename(running_mean_100yr = running_mean), by = c("site_no", "year", "Q_normalized"))

# Function to create time series plots for each station
create_time_series_plots <- function(stations) {
  for (i in seq(1, length(stations), by = 2)) {
    subset_stations <- stations[i:min(i+1, length(stations))]
    data_subset <- data_combined %>% filter(site_no %in% subset_stations)
    
    p <- ggplot(data_subset, aes(x = year)) +
      geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
      geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
      geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
      facet_wrap(~site_no, scales = "free_y") +
      labs(title = "Running Mean of Various Times in Streamflow", x = "Year", y = "Normalized Q") +
      theme_minimal() +
      scale_color_manual(values = c("20-Year Running Mean" = "blue", "50-Year Running Mean" = "green", "100-Year Running Mean" = "red")) +
      theme(legend.position = "bottom")
    
    print(p)
  }
}


# Extract unique station IDs
unique_stations <- unique(data$site_no)

# Create time series plots for each station (grouped in sets of 2)
create_time_series_plots(unique_stations)

# Function to create time series plots for each station (grouped in sets of 10)
create_time_series_plots <- function(stations) {
  for (i in seq(1, length(stations), by = 10)) {
    subset_stations <- stations[i:min(i+9, length(stations))]
    data_subset <- data_combined %>% filter(site_no %in% subset_stations)
    
    p <- ggplot(data_subset, aes(x = year)) +
      geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
      geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
      geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
      facet_wrap(~site_no, scales = "free_y") +
      labs(title = "Running Mean of Various Times in Streamflow", x = "Year", y = "Normalized Q") +
      theme_minimal() +
      scale_color_manual(values = c("20-Year Running Mean" = "blue", 
                                    "50-Year Running Mean" = "green", 
                                    "100-Year Running Mean" = "red")) +
      guides(color = guide_legend(title = NULL)) +  # Removes the word "colour" from the legend
      theme(legend.position = "bottom")
    
    print(p)
  }
}

# Extract unique station IDs
unique_stations <- unique(data$site_no)

# Create time series plots for each station (grouped in sets of 10)
create_time_series_plots(unique_stations)











##### 6 stations 


selected_stations <- c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500")

data_combined$site_no <- as.character(data_combined$site_no)

# Filter 
data_subset <- data_combined %>% filter(site_no %in% selected_stations)

# Plot
p <- ggplot(data_subset, aes(x = year)) +
  geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
  geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
  geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
  facet_wrap(~site_no, scales = "free_y") +
  labs(title = "Running Mean of Various Times in Streamflow", x = "Year", y = "Normalized Q") +
  theme_minimal() +
  scale_color_manual(values = c("20-Year Running Mean" = "blue", 
                                "50-Year Running Mean" = "green", 
                                "100-Year Running Mean" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "bottom")

# Print 
print(p)
######


selected_stations <- c("Ohio River at Metropolis, IL", "Mississippi River at Clinton, IA", "Missouri River at Omaha, NE", "Missouri River at Hermann, MO", "Mississippi River at Chester, IL", "Red River at Alexandria, LA")

data_combined$site_no <- as.character(data_combined$site_no)

# Filter 
data_subset <- data_combined %>% filter(site_no %in% selected_stations)

# Plot
p <- ggplot(data_subset, aes(x = year)) +
  geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
  geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
  geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
  facet_wrap(~site_no, scales = "free_y") +
  labs(title = "Running Mean of Various Times in Streamflow", x = "Year", y = "Normalized Q") +
  theme_minimal() +
  scale_color_manual(values = c("20-Year Running Mean" = "blue", 
                                "50-Year Running Mean" = "green", 
                                "100-Year Running Mean" = "red")) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "bottom")

# Print 
print(p)


########

library(dplyr)
library(ggplot2)

# Station site_no to full name mapping
station_names <- data.frame(
  site_no = c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500"),
  station_name = c(
    "Ohio River at Metropolis, IL",
    "Mississippi River at Clinton, IA",
    "Missouri River at Omaha, NE",
    "Missouri River at Hermann, MO",
    "Mississippi River at Chester, IL",
    "Red River at Alexandria, LA"
  ),
  stringsAsFactors = FALSE
)

# Ensure site_no is character
data_combined$site_no <- as.character(data_combined$site_no)

# Filter and join names
data_subset <- data_combined %>% 
  filter(site_no %in% station_names$site_no) %>%
  left_join(station_names, by = "site_no")

# Plot
p <- ggplot(data_subset, aes(x = year)) +
  geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
  geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
  geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
  facet_wrap(~station_name, scales = "free_y") +
  labs(title = "Running Mean of Various Times in Streamflow", 
       x = "Year", y = "Normalized Q") +
  theme_minimal() +
  scale_color_manual(values = c(
    "100-Year Running Mean" = "red", 
    "50-Year Running Mean" = "green", 
    "20-Year Running Mean" = "blue"
  )) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    #strip.text = element_text(face = "bold")  # T
  )

print(p)






#####












##########

# ---- Load Normalized Streamflow Data ----
data <- read.csv("/work/bmcdaniel/R_stuff/normalized_data.csv")

# ---- Calculate Running Means ----
calculate_running_mean <- function(data, window) {
  data %>%
    group_by(site_no) %>%
    arrange(year) %>%
    mutate(running_mean = rollmean(Q_normalized, k = window, fill = NA, align = 'right')) %>%
    ungroup()
}

data_combined <- calculate_running_mean(data, 20) %>%
  rename(running_mean_20yr = running_mean) %>%
  left_join(calculate_running_mean(data, 50)  %>% rename(running_mean_50yr  = running_mean), by = c("site_no", "year", "Q_normalized")) %>%
  left_join(calculate_running_mean(data, 100) %>% rename(running_mean_100yr = running_mean), by = c("site_no", "year", "Q_normalized"))

# ---- Plotting Function (Adjustable Group Size) ----
create_time_series_plots <- function(stations, group_size = 10) {
  for (i in seq(1, length(stations), by = group_size)) {
    subset_stations <- stations[i:min(i + group_size - 1, length(stations))]
    data_subset <- filter(data_combined, site_no %in% subset_stations)
    
    p <- ggplot(data_subset, aes(x = year)) +
      geom_line(aes(y = running_mean_20yr,  color = "20-Year Running Mean")) +
      geom_line(aes(y = running_mean_50yr,  color = "50-Year Running Mean")) +
      geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
      facet_wrap(~site_no, scales = "free_y") +
      labs(title = "Running Mean of Streamflow", x = "Year", y = "Normalized Q") +
      theme_minimal() +
      scale_color_manual(values = c(
        "20-Year Running Mean"  = "blue",
        "50-Year Running Mean"  = "green",
        "100-Year Running Mean" = "red"
      )) +
      guides(color = guide_legend(title = NULL)) +
      theme(legend.position = "bottom")
    
    print(p)
  }
}

# ---- Run for All Stations ----
create_time_series_plots(unique(data$site_no), group_size = 10)

# ---- Plot for Selected 6 Stations ----
selected_stations <- c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500")
data_subset <- filter(data_combined, site_no %in% selected_stations)

p_selected <- ggplot(data_subset, aes(x = year)) +
  geom_line(aes(y = running_mean_20yr,  color = "20-Year Running Mean")) +
  geom_line(aes(y = running_mean_50yr,  color = "50-Year Running Mean")) +
  geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
  facet_wrap(~site_no, scales = "free_y") +
  labs(title = "Running Mean for Selected Stations", x = "Year", y = "Normalized Q") +
  theme_minimal() +
  scale_color_manual(values = c(
    "20-Year Running Mean"  = "blue",
    "50-Year Running Mean"  = "green",
    "100-Year Running Mean" = "red"
  )) +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "bottom")

print(p_selected)





##########



library(dplyr)
library(ggplot2)

# Station site_no to full name mapping
station_names <- data.frame(
  site_no = c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500"),
  station_name = c(
    "Ohio River at Metropolis, IL",
    "Mississippi River at Clinton, IA",
    "Missouri River at Omaha, NE",
    "Missouri River at Hermann, MO",
    "Mississippi River at Chester, IL",
    "Red River at Alexandria, LA"
  ),
  stringsAsFactors = FALSE
)

# Ensure site_no is character
data_combined$site_no <- as.character(data_combined$site_no)

# Filter and join names
data_subset <- data_combined %>% 
  filter(site_no %in% station_names$site_no) %>%
  left_join(station_names, by = "site_no")

# Plot
p <- ggplot(data_subset, aes(x = year)) +
  geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
  geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
  geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
  facet_wrap(~station_name, scales = "free_y") +
  labs(title = "Running Mean of Various Times in Streamflow", 
       x = "Year", y = "Normalized Q") +
  theme_minimal() +
  scale_color_manual(values = c(
    "100-Year Running Mean" = "red", 
    "50-Year Running Mean" = "green", 
    "20-Year Running Mean" = "blue"
  )) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    #strip.text = element_text(face = "bold")  # T
  )

print(p)



######


library(dplyr)
library(ggplot2)

# Station site_no to full name mapping
station_names <- data.frame(
  site_no = c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500"),
  station_name = c(
    "Ohio River at Metropolis, IL",
    "Mississippi River at Clinton, IA",
    "Missouri River at Omaha, NE",
    "Missouri River at Hermann, MO",
    "Mississippi River at Chester, IL",
    "Red River at Alexandria, LA"
  ),
  stringsAsFactors = FALSE
)

# Ensure site_no is character
data_combined$site_no <- as.character(data_combined$site_no)

# Filter and join names
data_subset <- data_combined %>% 
  filter(site_no %in% station_names$site_no) %>%
  left_join(station_names, by = "site_no")

# Plot
p <- ggplot(data_subset, aes(x = year)) +
  geom_line(aes(y = running_mean_20yr, color = "20-Year Running Mean")) +
  geom_line(aes(y = running_mean_50yr, color = "50-Year Running Mean")) +
  geom_line(aes(y = running_mean_100yr, color = "100-Year Running Mean")) +
  facet_wrap(~station_name, scales = "free_y") +
  labs(
    title = "Running Mean of Various Times in Streamflow", 
    x = "Year", 
    y = "Normalized Streamflow"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99",   # Deep violet
      "50-Year Running Mean" = "#e66101",   # Burnt orange
      "100-Year Running Mean" = "#4393c3"   # Steel blue
    )
  ) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom"
  )

print(p)






####











#These are the new colors



# Create an ordered factor for color mapping
data_subset <- data_subset %>%
  mutate(line_type = factor(
    case_when(
      !is.na(running_mean_20yr) ~ "20-Year Running Mean",
      !is.na(running_mean_50yr) ~ "50-Year Running Mean",
      !is.na(running_mean_100yr) ~ "100-Year Running Mean"
    ),
    levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
  ))

# Pivot data to long format for easier plotting
library(tidyr)

data_long <- data_subset %>%
  pivot_longer(
    cols = starts_with("running_mean"),
    names_to = "window",
    values_to = "value"
  ) %>%
  mutate(
    window = recode(window,
                    "running_mean_20yr" = "20-Year Running Mean",
                    "running_mean_50yr" = "50-Year Running Mean",
                    "running_mean_100yr" = "100-Year Running Mean"
    ),
    window = factor(window, levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"))
  )

# Plot
p <- ggplot(data_long, aes(x = year, y = value, color = window)) +
  geom_line() +
  facet_wrap(~station_name, scales = "free_y") +
  labs(
    title = "Running Mean of Various Times in Streamflow",
    x = "Year", y = "Normalized Streamflow"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99", 
      "50-Year Running Mean" = "#e66101", 
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom"
  )

print(p)










#######

######## POWER SPECTRUM ########


library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Define full year range
full_years <- 1200:2005
n_years <- length(full_years)

# Ensure full time series per station and window (pad with NAs if needed)
pad_series <- function(data, station, window_label) {
  df <- data %>%
    filter(station_name == station, window == window_label) %>%
    select(year, value)
  
  # Create full-year series and left join to pad
  full_df <- data.frame(year = full_years)
  merged <- left_join(full_df, df, by = "year")
  return(merged$value)  # Return padded values
}

# Compute spectrum for one time series
compute_spectrum <- function(ts, station, window_label) {
  ts_clean <- ts[!is.na(ts)]
  if (length(ts_clean) < 5) return(NULL)
  
  spec <- spectrum(ts_clean, plot = FALSE)
  freq <- spec$freq  # cycles/year
  
  data.frame(
    frequency = freq,
    period = 1 / freq,  # years
    spectrum = spec$spec,
    station_name = station,
    window = window_label
  )
}

# Prepare all combinations of station + window
station_window_list <- data_long %>%
  distinct(station_name, window) %>%
  arrange(station_name, window)

# Compute spectra
spectra_df <- pmap_dfr(station_window_list, function(station_name, window) {
  ts_vals <- pad_series(data_long, station_name, window)
  compute_spectrum(ts_vals, station_name, window)
})

# Format factor levels
spectra_df$window <- factor(spectra_df$window,
                            levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"))

# Optional: limit periods to improve plot readability
spectra_df <- spectra_df %>% filter(period <= 200)

# Final plot: Linear scale on Y-axis, Period (Years) on X-axis
p_spec <- ggplot(spectra_df, aes(x = period, y = spectrum, color = window)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) +  # Long periods on right, short on left
  scale_y_continuous(labels = scales::label_number()) +  # Linear Y scale
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  labs(
    title = "Power Spectrum of Smoothed Streamflow Time Series (1200–2005)",
    x = "Period (Years)",
    y = "Spectral Density"
  ) +
  facet_wrap(~station_name, nrow = 2) +
  theme_minimal() +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "bottom")


# Display
print(p_spec)







###
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Expand plot output size (Option 2)
options(repr.plot.width = 14, repr.plot.height = 8)  # Works in R Notebooks / Jupyter / RStudio

# Define full year range
full_years <- 1200:2005
n_years <- length(full_years)

# Ensure full time series per station and window (pad with NAs if needed)
pad_series <- function(data, station, window_label) {
  df <- data %>%
    filter(station_name == station, window == window_label) %>%
    select(year, value)
  
  full_df <- data.frame(year = full_years)
  merged <- left_join(full_df, df, by = "year")
  return(merged$value)
}

# Compute spectrum for one time series
compute_spectrum <- function(ts, station, window_label) {
  ts_clean <- ts[!is.na(ts)]
  if (length(ts_clean) < 5) return(NULL)
  
  spec <- spectrum(ts_clean, plot = FALSE)
  freq <- spec$freq
  
  data.frame(
    frequency = freq,
    period = 1 / freq,
    spectrum = spec$spec,
    station_name = station,
    window = window_label
  )
}

# Prepare all combinations of station + window
station_window_list <- data_long %>%
  distinct(station_name, window) %>%
  arrange(station_name, window)

# Compute spectra
spectra_df <- pmap_dfr(station_window_list, function(station_name, window) {
  ts_vals <- pad_series(data_long, station_name, window)
  compute_spectrum(ts_vals, station_name, window)
})

# Format factor levels
spectra_df$window <- factor(
  spectra_df$window,
  levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
)

# Optional: limit to periods ≤ 200 years for focus
spectra_df <- spectra_df %>% filter(period <= 200)

# Final plot
p_spec <- ggplot(spectra_df, aes(x = period, y = spectrum, color = window)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) +  # Longer periods to the right
  scale_y_continuous(labels = scales::label_number()) +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  labs(
    title = "Power Spectrum of Smoothed Streamflow Time Series (1200–2005)",
    x = "Period (Years)",
    y = "Spectral Density"
  ) +
  facet_wrap(~station_name, nrow = 2) +
  theme_minimal(base_size = 14) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 20, 10, 20)
  )

# Display
print(p_spec)








#### Significance test


library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# ---- Parameters ----
n_simulations <- 1000
alpha <- 0.05

# ---- Red-noise simulation function ----
generate_red_noise <- function(ts, n_sim) {
  ar_model <- ar(ts, method = "yw")
  ar_coeff <- ar_model$ar[1]
  ts_sd <- sd(ts, na.rm = TRUE)
  ts_mean <- mean(ts, na.rm = TRUE)
  n <- length(ts)
  
  sims <- replicate(n_sim, {
    sim <- arima.sim(n = n, model = list(ar = ar_coeff), sd = ts_sd)
    sim + ts_mean
  })
  
  return(sims)
}

# ---- Running mean + confidence intervals ----
compute_running_CI <- function(ts, window, n_sim) {
  sims <- generate_red_noise(ts, n_sim)
  smooth_sims <- apply(sims, 2, function(x) rollmean(x, window, fill = NA, align = "center"))
  
  lower <- apply(smooth_sims, 1, quantile, probs = alpha/2, na.rm = TRUE)
  upper <- apply(smooth_sims, 1, quantile, probs = 1 - alpha/2, na.rm = TRUE)
  
  return(list(lower = lower, upper = upper))
}

# ---- Main loop by station ----
results_with_ci <- data_subset %>%
  group_by(station_name) %>%
  group_modify(~{
    ts <- .x$Q_normalized
    year <- .x$year
    out <- tibble(year = year)
    
    for (window in c(20, 50, 100)) {
      ci <- compute_running_CI(ts, window, n_simulations)
      smoothed <- rollmean(ts, window, fill = NA, align = "center")
      
      out[[paste0("mean_", window)]] <- smoothed
      out[[paste0("lower_", window)]] <- ci$lower
      out[[paste0("upper_", window)]] <- ci$upper
    }
    
    out$station_name <- .x$station_name[1]
    return(out)
  }) %>%
  ungroup()

# ---- Pivot to long for line plotting ----
plot_data <- results_with_ci %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "window", values_to = "value") %>%
  mutate(window = gsub("mean_", "", window),
         window = paste0(window, "-Year Running Mean"))

# ---- Pivot to long for confidence intervals ----
plot_ci <- results_with_ci %>%
  pivot_longer(cols = starts_with("lower_") | starts_with("upper_"),
               names_to = c("bound", "window"), names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(window = paste0(window, "-Year Running Mean"))

# ---- Final plot ----
ggplot() +
  geom_ribbon(data = plot_ci, aes(x = year, ymin = lower, ymax = upper, fill = window),
              alpha = 0.2) +
  geom_line(data = plot_data, aes(x = year, y = value, color = window), linewidth = 0.7) +
  facet_wrap(~station_name, scales = "free_y") +
  labs(title = "Running Mean Significance Test on Normalized Streamflow",
       x = "Year", y = "Normalized Streamflow") +
  scale_color_manual(values = c(
    "20-Year Running Mean" = "#5e3c99", 
    "50-Year Running Mean" = "#e66101", 
    "100-Year Running Mean" = "#4393c3"
  )) +
  scale_fill_manual(values = c(
    "20-Year Running Mean" = "#5e3c99", 
    "50-Year Running Mean" = "#e66101", 
    "100-Year Running Mean" = "#4393c3"
  )) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = NULL),
         fill = guide_legend(title = NULL))

##########


########









library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(data.table)

# ---- PARAMETERS ----
n_simulations <- 1000
alpha <- 0.05
windows <- c(20, 50, 100)

# ---- RED-NOISE SIMULATION FUNCTION ----
generate_red_noise <- function(ts, n_sim) {
  ar_model <- ar(ts, method = "yw")
  ar_coeff <- ar_model$ar[1]
  ts_sd <- sd(ts, na.rm = TRUE)
  ts_mean <- mean(ts, na.rm = TRUE)
  n <- length(ts)
  
  sims <- replicate(n_sim, {
    sim <- arima.sim(n = n, model = list(ar = ar_coeff), sd = ts_sd)
    sim + ts_mean
  })
  return(sims)
}

# ---- CI FUNCTION ----
compute_running_CI <- function(ts, window, n_sim = 1000, alpha = 0.05) {
  # Remove missing values
  ts <- ts[!is.na(ts)]
  
  # Must have enough values
  if (length(ts) < window + 5) {
    n <- length(ts)
    return(list(
      lower = rep(NA, n),
      upper = rep(NA, n)
    ))
  }
  
  # Try AR model
  ar_model <- tryCatch({
    ar(ts, method = "yw")
  }, error = function(e) return(NULL))
  
  if (is.null(ar_model) || length(ar_model$ar) < 1) {
    warning("AR model failed on a time series")
    n <- length(ts)
    return(list(
      lower = rep(NA, n),
      upper = rep(NA, n)
    ))
  }
  
  ar_coeff <- ar_model$ar[1]
  ts_sd <- sd(ts, na.rm = TRUE)
  ts_mean <- mean(ts, na.rm = TRUE)
  n <- length(ts)
  
  # Simulate
  sims <- replicate(n_sim, {
    sim <- arima.sim(n = n, model = list(ar = ar_coeff), sd = ts_sd)
    sim + ts_mean
  })
  
  # Convert to smoothed matrix
  smooth_sims <- apply(sims, 2, function(x) rollmean(x, window, fill = NA, align = "center"))
  
  # Ensure it's a matrix
  if (!is.matrix(smooth_sims)) {
    smooth_sims <- do.call(cbind, lapply(1:n_sim, function(i) rollmean(sims[, i], window, fill = NA, align = "center")))
  }
  
  # Compute CIs
  lower <- apply(smooth_sims, 1, quantile, probs = alpha / 2, na.rm = TRUE)
  upper <- apply(smooth_sims, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
  
  return(list(lower = lower, upper = upper))
}


# ---- MAIN SIGNIFICANCE ANALYSIS ----
results_with_sig <- data_subset %>%
  group_by(station_name) %>%
  group_modify(~{
    ts <- .x$Q_normalized
    year <- .x$year
    out <- tibble(year = year)
    
    for (window in windows) {
      ci <- compute_running_CI(ts, window)
      smoothed <- rollmean(ts, window, fill = NA, align = "center")
      out[[paste0("mean_", window)]]  <- smoothed
      out[[paste0("lower_", window)]] <- ci$lower
      out[[paste0("upper_", window)]] <- ci$upper
      out[[paste0("sig_", window)]]   <- ifelse(smoothed < ci$lower, "Below",
                                                ifelse(smoothed > ci$upper, "Above", "Not Significant"))
    }
    
    out$station_name <- .y$station_name  # FIXED: use .y for grouping column
    return(out)
  }) %>%
  ungroup()













####


library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(data.table)

# ---- PARAMETERS ----
n_simulations <- 1000
alpha <- 0.05
windows <- c(20, 50, 100)

# ---- RED-NOISE SIMULATION FUNCTION ----
generate_red_noise <- function(ts, n_sim) {
  ar_model <- ar(ts, method = "yw")
  ar_coeff <- ar_model$ar[1]
  ts_sd <- sd(ts, na.rm = TRUE)
  ts_mean <- mean(ts, na.rm = TRUE)
  n <- length(ts)
  
  sims <- replicate(n_sim, {
    sim <- arima.sim(n = n, model = list(ar = ar_coeff), sd = ts_sd)
    sim + ts_mean
  })
  return(sims)
}

# ---- CI FUNCTION ----
compute_running_CI <- function(ts, window, n_sim = 1000, alpha = 0.05) {
  ts <- ts[!is.na(ts)]
  if (length(ts) < window + 5) {
    n <- length(ts)
    return(list(lower = rep(NA, n), upper = rep(NA, n)))
  }
  
  ar_model <- tryCatch({ ar(ts, method = "yw") }, error = function(e) return(NULL))
  if (is.null(ar_model) || length(ar_model$ar) < 1) {
    warning("AR model failed")
    n <- length(ts)
    return(list(lower = rep(NA, n), upper = rep(NA, n)))
  }
  
  ar_coeff <- ar_model$ar[1]
  ts_sd <- sd(ts, na.rm = TRUE)
  ts_mean <- mean(ts, na.rm = TRUE)
  n <- length(ts)
  
  sims <- replicate(n_sim, {
    sim <- arima.sim(n = n, model = list(ar = ar_coeff), sd = ts_sd)
    sim + ts_mean
  })
  
  smooth_sims <- apply(sims, 2, function(x) rollmean(x, window, fill = NA, align = "center"))
  if (!is.matrix(smooth_sims)) {
    smooth_sims <- do.call(cbind, lapply(1:n_sim, function(i) rollmean(sims[, i], window, fill = NA, align = "center")))
  }
  
  lower <- apply(smooth_sims, 1, quantile, probs = alpha / 2, na.rm = TRUE)
  upper <- apply(smooth_sims, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
  
  return(list(lower = lower, upper = upper))
}


# ---- SIGNIFICANCE ANALYSIS LOOP ----
results_with_sig <- data_subset %>%
  group_by(station_name) %>%
  group_modify(~{
    ts <- .x$Q_normalized
    year <- .x$year
    out <- tibble(year = year)
    
    for (window in windows) {
      ci <- compute_running_CI(ts, window)
      smoothed <- rollmean(ts, window, fill = NA, align = "center")
      out[[paste0("mean_", window)]]  <- smoothed
      out[[paste0("lower_", window)]] <- ci$lower
      out[[paste0("upper_", window)]] <- ci$upper
      out[[paste0("sig_", window)]]   <- ifelse(smoothed < ci$lower, "Below",
                                                ifelse(smoothed > ci$upper, "Above", "Not Significant"))
    }
    
    return(out)  # ← No station_name inside group_modify
  }) %>%
  ungroup()

# ---- REATTACH STATION NAMES ----
results_with_sig$station_name <- rep(unique(data_subset$station_name),
                                     each = nrow(results_with_sig) / length(unique(data_subset$station_name)))

# ---- PREP FOR PLOTTING ----
plot_long <- results_with_sig %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "mean_var", values_to = "value") %>%
  mutate(window = gsub("mean_", "", mean_var)) %>%
  left_join(
    results_with_sig %>%
      select(year, station_name, starts_with("sig_")) %>%
      pivot_longer(cols = starts_with("sig_"), names_to = "sig_var", values_to = "significance") %>%
      mutate(window = gsub("sig_", "", sig_var)),
    by = c("station_name", "year", "window")
  )

# ---- PLOT ----
ggplot(plot_long, aes(x = year, y = value, color = significance)) +
  geom_line(linewidth = 0.7) +
  facet_grid(station_name ~ window, scales = "free_y") +
  scale_color_manual(values = c(
    "Above" = "firebrick",
    "Below" = "dodgerblue",
    "Not Significant" = "gray60"
  )) +
  labs(title = "Running Mean Streamflow with Red-Noise Significance",
       y = "Normalized Streamflow", x = "Year", color = "Significance") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ---- SIGNIFICANT PERIOD TABLE ----
summary_dt <- as.data.table(plot_long)
summary_dt[, run_id := rleid(significance), by = .(station_name, window)]

sig_table <- summary_dt[significance != "Not Significant", .(
  start_year = min(year, na.rm = TRUE),
  end_year   = max(year, na.rm = TRUE),
  duration   = .N,
  type       = unique(significance)
), by = .(station_name, window, run_id)][order(station_name, window, start_year)]

# View summary table
print(sig_table)
View(sig_table)
# Optional: save to CSV
# fwrite(sig_table, "significant_periods_by_station.csv")





#########################
# Summarize number of significant periods by station and type
sig_counts <- sig_table %>%
  group_by(station_name, type) %>%
  summarise(n_periods = n(), .groups = "drop")

library(ggplot2)

ggplot(sig_fraction, aes(x = station_name, y = percent, fill = significance)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~window, ncol = 1) +  # Top to bottom: 20, 50, 100
  scale_fill_manual(values = c("Above" = "firebrick", "Below" = "dodgerblue")) +
  labs(
    title = "Percent of Significant Streamflow Years by Station and Smoothing Window",
    y = "% of Time Series Significantly Above or Below Red Noise",
    x = "Station",
    fill = "Anomaly Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#####

# Use the plot_long object that has year-by-year significance flags
sig_fraction <- plot_long %>%
  filter(!is.na(significance)) %>%
  group_by(station_name, window, significance) %>%
  summarise(n_years = n(), .groups = "drop") %>%
  group_by(station_name, window) %>%
  mutate(total = sum(n_years),
         percent = round(100 * n_years / total, 1)) %>%
  filter(significance != "Not Significant")

ggplot(sig_fraction, aes(x = station_name, y = percent, fill = significance)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~window, ncol = 1) +
  scale_fill_manual(values = c("Above" = "firebrick", "Below" = "dodgerblue")) +
  labs(
    title = "Percent of Significant Streamflow Years by Station",
    y = "% of Time Series Significantly Above or Below Red Noise",
    x = "Station",
    fill = "Anomaly Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))





# Total average percent of significant (Above + Below)
mean_total_percent <- sig_fraction %>%
  summarise(mean_percent = round(mean(percent), 2))

# Mean percent by window (across stations)
mean_by_window <- sig_fraction %>%
  group_by(window) %>%
  summarise(mean_percent = round(mean(percent), 2))

print(mean_by_window)


print(mean_total_percent)



View(sig_fraction)



######

#####




















###
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Expand plot output size (Option 2)
options(repr.plot.width = 14, repr.plot.height = 8)  # Works in R Notebooks / Jupyter / RStudio

# Define full year range
full_years <- 1200:2005
n_years <- length(full_years)

# Ensure full time series per station and window (pad with NAs if needed)
pad_series <- function(data, station, window_label) {
  df <- data %>%
    filter(station_name == station, window == window_label) %>%
    select(year, value)
  
  full_df <- data.frame(year = full_years)
  merged <- left_join(full_df, df, by = "year")
  return(merged$value)
}

# Compute spectrum for one time series
compute_spectrum <- function(ts, station, window_label) {
  ts_clean <- ts[!is.na(ts)]
  if (length(ts_clean) < 5) return(NULL)
  
  spec <- spectrum(ts_clean, plot = FALSE)
  freq <- spec$freq
  
  data.frame(
    frequency = freq,
    period = 1 / freq,
    spectrum = spec$spec,
    station_name = station,
    window = window_label
  )
}

# Prepare all combinations of station + window
station_window_list <- data_long %>%
  distinct(station_name, window) %>%
  arrange(station_name, window)

# Compute spectra
spectra_df <- pmap_dfr(station_window_list, function(station_name, window) {
  ts_vals <- pad_series(data_long, station_name, window)
  compute_spectrum(ts_vals, station_name, window)
})

# Format factor levels
spectra_df$window <- factor(
  spectra_df$window,
  levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
)

# Optional: limit to periods ≤ 200 years for focus
spectra_df <- spectra_df %>% filter(period <= 200)

# Final plot
p_spec <- ggplot(spectra_df, aes(x = period, y = spectrum, color = window)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) +  # Longer periods to the right
  scale_y_continuous(labels = scales::label_number()) +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  labs(
    title = "Power Spectrum of Smoothed Streamflow Time Series (1200–2005)",
    x = "Period (Years)",
    y = "Spectral Density"
  ) +
  facet_wrap(~station_name, nrow = 2) +
  theme_minimal(base_size = 14) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 20, 10, 20)
  )

# Display
print(p_spec)







# Create an ordered factor for color mapping
data_subset <- data_subset %>%
  mutate(line_type = factor(
    case_when(
      !is.na(running_mean_20yr) ~ "20-Year Running Mean",
      !is.na(running_mean_50yr) ~ "50-Year Running Mean",
      !is.na(running_mean_100yr) ~ "100-Year Running Mean"
    ),
    levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
  ))

# Pivot data to long format for easier plotting
library(tidyr)

data_long <- data_subset %>%
  pivot_longer(
    cols = starts_with("running_mean"),
    names_to = "window",
    values_to = "value"
  ) %>%
  mutate(
    window = recode(window,
                    "running_mean_20yr" = "20-Year Running Mean",
                    "running_mean_50yr" = "50-Year Running Mean",
                    "running_mean_100yr" = "100-Year Running Mean"
    ),
    window = factor(window, levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"))
  )

# Plot
p <- ggplot(data_long, aes(x = year, y = value, color = window)) +
  geom_line() +
  facet_wrap(~station_name, scales = "free_y") +
  labs(
    title = "Running Mean of Various Times in Streamflow",
    x = "Year", y = "Normalized Streamflow"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99", 
      "50-Year Running Mean" = "#e66101", 
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom"
  )

print(p)

###
########
################
########
###



library(ggplot2)
library(patchwork)
library(dplyr)

# ---- SPLIT FACETS INTO LISTS OF PLOTS ----

# Create list of unique stations (ensure same order in both datasets)
stations <- unique(data_long$station_name)

# Split time series plot (p) into separate plots per station
p_list <- lapply(stations, function(station) {
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Running Means"),
         x = "Year", y = "Normalized Streamflow") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# Split spectrum plots into list
spec_list <- lapply(stations, function(station) {
  ggplot(filter(spectra_df, station_name == station),
         aes(x = period, y = spectrum, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Spectrum"),
         x = "Period (Years)", y = "Spectral Density") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = "none") +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# ---- COMBINE PAIRED PLOTS FOR EACH STATION ----

paired_vertical <- mapply(function(ts, spec) {
  ts / spec  # vertical stack
}, ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# ---- STACK ALL STATIONS HORIZONTALLY ----
final_combined <- Reduce(`|`, paired_vertical) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

# ---- DISPLAY ----
print(final_combined)

# Optional: Save the figure
ggsave("combined_runningmean_spectrum_by_station.png",
       plot = final_combined, width = 16, height = 10, dpi = 300)




#########

# twiesw

###



library(ggplot2)
library(patchwork)
library(dplyr)

# Get station list
stations <- unique(data_long$station_name)

# Create time series plots
p_list <- lapply(stations, function(station) {
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Running Means"),
         x = "Year", y = "Normalized Streamflow") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# Create spectrum plots
spec_list <- lapply(stations, function(station) {
  ggplot(filter(spectra_df, station_name == station),
         aes(x = period, y = spectrum, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Spectrum"),
         x = "Period (Years)", y = "Spectral Density") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = "none") +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# Combine as horizontal pairs (Time Series | Spectrum) for each station
paired_horizontal <- mapply(function(ts, spec) {
  ts | spec  # horizontal stack
}, ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# Stack all station pairs vertically
final_combined <- Reduce(`/`, paired_horizontal) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

# Display
print(final_combined)

# Optional: Save output
ggsave("combined_plot_horizontal_pairing.png",
       plot = final_combined, width = 14, height = 3 * length(stations), dpi = 300)

######



# Determine number of stations
n_stations <- length(stations)

# Final combined plot (same as before)
final_combined <- Reduce(`/`, paired_horizontal) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

# Display in viewer (if needed)
print(final_combined)

# Save taller version — increase height per station
ggsave("combined_plot_horizontal_pairing_taller.png",
       plot = final_combined,
       width = 14, 
       height = 4.5 * n_stations,  # Increase height per station row
       dpi = 300)



########################


#######################












library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Expand plot output size
options(repr.plot.width = 14, repr.plot.height = 8)

# Define full year range
full_years <- 1200:2005
n_years <- length(full_years)

# Pad time series to full range and force 0 at year 1374 (175th year)
pad_series <- function(data, station, window_label) {
  df <- data %>%
    filter(station_name == station, window == window_label) %>%
    select(year, value)
  
  full_df <- data.frame(year = full_years)
  merged <- left_join(full_df, df, by = "year")
  
  # Force 0 at year 1374 (175th index)
  target_year <- full_years[175]  # 1200 + 174 = 1374
  merged$value[merged$year == target_year] <- 0
  
  return(merged$value)
}

# Compute spectrum for one time series
compute_spectrum <- function(ts, station, window_label) {
  ts_clean <- ts[!is.na(ts)]
  if (length(ts_clean) < 5) return(NULL)
  
  spec <- spectrum(ts_clean, plot = FALSE)
  freq <- spec$freq
  
  data.frame(
    frequency = freq,
    period = 1 / freq,
    spectrum = spec$spec,
    station_name = station,
    window = window_label
  )
}

# Prepare all combinations of station + window
station_window_list <- data_long %>%
  distinct(station_name, window) %>%
  arrange(station_name, window)

# Compute spectra
spectra_df <- pmap_dfr(station_window_list, function(station_name, window) {
  ts_vals <- pad_series(data_long, station_name, window)
  compute_spectrum(ts_vals, station_name, window)
})

# Format factor levels
spectra_df$window <- factor(
  spectra_df$window,
  levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
)

# Optional: limit to periods ≤ 200 years for focus
spectra_df <- spectra_df %>% filter(period <= 165)

# Final plot
p_spec <- ggplot(spectra_df, aes(x = period, y = spectrum, color = window)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  labs(
    title = "Power Spectrum of Smoothed Streamflow Time Series (1200–2005, Forced 0 at Year 1374)",
    x = "Period (Years)",
    y = "Spectral Density"
  ) +
  facet_wrap(~station_name, nrow = 2) +
  theme_minimal(base_size = 14) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 20, 10, 20)
  )

# Display
print(p_spec)





#########################################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Expand plot output size
options(repr.plot.width = 14, repr.plot.height = 8)

# Define full year range
full_years <- 1200:2005
n_years <- length(full_years)

# Pad time series to full range and force 0 at year 1374 (175th year)
pad_series <- function(data, station, window_label) {
  df <- data %>%
    filter(station_name == station, window == window_label) %>%
    select(year, value)
  
  full_df <- data.frame(year = full_years)
  merged <- left_join(full_df, df, by = "year")
  
  # Force 0 at year 1374 (175th year in full_years)
  target_year <- full_years[175]  # 1200 + 174 = 1374
  merged$value[merged$year == target_year] <- 0
  
  return(merged$value)
}

# Compute spectrum for one time series
compute_spectrum <- function(ts, station, window_label) {
  ts_clean <- ts[!is.na(ts)]
  if (length(ts_clean) < 5) return(NULL)
  
  spec <- spectrum(ts_clean, plot = FALSE)
  freq <- spec$freq
  
  data.frame(
    frequency = freq,
    period = 1 / freq,
    spectrum = spec$spec,
    station_name = station,
    window = window_label
  )
}

# Prepare all combinations of station + window
station_window_list <- data_long %>%
  distinct(station_name, window) %>%
  arrange(station_name, window)

# Compute spectra
spectra_df <- pmap_dfr(station_window_list, function(station_name, window) {
  ts_vals <- pad_series(data_long, station_name, window)
  compute_spectrum(ts_vals, station_name, window)
})

# Format factor levels
spectra_df$window <- factor(
  spectra_df$window,
  levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
)

# Zero out power spectrum for periods > 175 (but keep full axis to 200)
spectra_df <- spectra_df %>%
  mutate(spectrum = ifelse(period > 190, 0, spectrum)) %>%
  filter(period <= 200)

# Final plot
p_spec <- ggplot(spectra_df, aes(x = period, y = spectrum, color = window)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  labs(
    title = "Power Spectrum of Smoothed Streamflow Time Series (1200–2005)",
    x = "Period (Years)",
    y = "Spectral Density"
  ) +
  facet_wrap(~station_name, nrow = 2) +
  theme_minimal(base_size = 14) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, face = "italic"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 20, 10, 20)
  )

# Display plot
print(p_spec)


#

# Create an ordered factor for color mapping
data_subset <- data_subset %>%
  mutate(line_type = factor(
    case_when(
      !is.na(running_mean_20yr) ~ "20-Year Running Mean",
      !is.na(running_mean_50yr) ~ "50-Year Running Mean",
      !is.na(running_mean_100yr) ~ "100-Year Running Mean"
    ),
    levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean")
  ))

# Pivot data to long format for easier plotting
library(tidyr)

data_long <- data_subset %>%
  pivot_longer(
    cols = starts_with("running_mean"),
    names_to = "window",
    values_to = "value"
  ) %>%
  mutate(
    window = recode(window,
                    "running_mean_20yr" = "20-Year Running Mean",
                    "running_mean_50yr" = "50-Year Running Mean",
                    "running_mean_100yr" = "100-Year Running Mean"
    ),
    window = factor(window, levels = c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"))
  )

# Plot
p <- ggplot(data_long, aes(x = year, y = value, color = window)) +
  geom_line() +
  facet_wrap(~station_name, scales = "free_y") +
  labs(
    title = "Running Mean of Various Times in Streamflow",
    x = "Year", y = "Normalized Streamflow"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "20-Year Running Mean" = "#5e3c99", 
      "50-Year Running Mean" = "#e66101", 
      "100-Year Running Mean" = "#4393c3"
    )
  ) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "bottom"
  )

print(p)




###############
#Combone

library(patchwork)
library(dplyr)

# --- STEP 1: Prepare plots (same as before) ---

stations <- unique(data_long$station_name)

# Time series plots
p_list <- lapply(stations, function(station) {
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Running Means"),
         x = "Year", y = "Normalized Streamflow (ft³/s)") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# Spectrum plots
spec_list <- lapply(stations, function(station) {
  ggplot(filter(spectra_df, station_name == station),
         aes(x = period, y = spectrum, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Spectrum"),
         x = "Period (Years)", y = "Spectral Density") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = "none") +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# Pair up horizontally
paired_horizontal <- mapply(function(ts, spec) {
  ts | spec
}, ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# --- STEP 2: Plot 3 pairs at a time ---

# Function to plot any N-pair block
plot_station_chunk <- function(paired_list, chunk_index, chunk_size = 3) {
  start_idx <- (chunk_index - 1) * chunk_size + 1
  end_idx <- min(start_idx + chunk_size - 1, length(paired_list))
  subset_plots <- paired_list[start_idx:end_idx]
  
  # Combine vertically
  combined_plot <- Reduce(`/`, subset_plots) +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
  
  return(combined_plot)
}

# Example: Plot the first 3 station pairs
plot_chunk_1 <- plot_station_chunk(paired_horizontal, chunk_index = 1)
print(plot_chunk_1)

# Optional: Save it
ggsave("chunk_1_stations_pairplot.png", 
       plot = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 300)





# Plot the second 3 station pairs (stations 4–6)
plot_chunk_2 <- plot_station_chunk(paired_horizontal, chunk_index = 2)
print(plot_chunk_2)

# Optional: Save to file
ggsave("chunk_2_stations_pairplot.png", 
       plot = plot_chunk_2, width = 14, height = 4.5 * 3, dpi = 300)



###########

stations <- unique(data_long$station_name)

# Time series plots
p_list <- lapply(stations, function(station) {
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(title = paste(station, "- Running Means"),
         x = "Year", y = "Normalized Streamflow (ft³/s)") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99", 
        "50-Year Running Mean" = "#e66101", 
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10),
          strip.text = element_blank())
})

# Spectrum plots
# Spectrum plots (cleaned up version)
spec_list <- lapply(stations, function(station) {
  df <- filter(spectra_df, station_name == station)
  
  ggplot(df, aes(x = period, y = spectrum, color = window)) +
    geom_line(size = 1) +
    labs(title = paste(station, "- Spectrum"),
         x = "Period (Years)", y = "Spectral Density") +
    scale_color_manual(
      values = c(
        "20-Year Running Mean" = "#5e3c99",
        "50-Year Running Mean" = "#e66101",
        "100-Year Running Mean" = "#4393c3"
      )
    ) +
    scale_x_continuous(
      limits = c(0, 200),
      breaks = c(0, 25, 50, 100, 150, 200)
    ) +
    scale_y_continuous(
      limits = c(0, NA),  # auto upper limit
      expand = expansion(mult = c(0, 0.05))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)),
      strip.text = element_blank(),
      panel.grid.major = element_line(color = "grey80", size = 0.3),
      panel.grid.minor = element_blank()
    )
})


# Pair up horizontally
paired_horizontal <- mapply(function(ts, spec) {
  ts | spec
}, ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# --- STEP 2: Plot 3 pairs at a time ---

# Function to plot any N-pair block
plot_station_chunk <- function(paired_list, chunk_index, chunk_size = 3) {
  start_idx <- (chunk_index - 1) * chunk_size + 1
  end_idx <- min(start_idx + chunk_size - 1, length(paired_list))
  subset_plots <- paired_list[start_idx:end_idx]
  
  # Combine vertically
  combined_plot <- Reduce(`/`, subset_plots) +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
  
  return(combined_plot)
}

# Example: Plot the first 3 station pairs
plot_chunk_1 <- plot_station_chunk(paired_horizontal, chunk_index = 1)
print(plot_chunk_1)

# Optional: Save it
ggsave("chunk_1_stations_pairplot.png", 
       plot = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 300)










############

library(patchwork)

# Plot the last 6 station pairs using wrap_plots (safer than Reduce)
plot_chunk_2 <- wrap_plots(paired_horizontal[7:12], ncol = 1, guides = "collect") &
  theme(legend.position = 'bottom')

# Display
print(plot_chunk_2)

# Save to file
ggsave("chunk_2_stations_pairplot.png", 
       plot = plot_chunk_2, width = 14, height = 4.5 * 6, dpi = 300)




















library(ggplot2)
library(dplyr)
library(patchwork)

# --- Step 1: Station list ---
stations <- unique(data_long$station_name)

# --- Step 2: Time Series Plots (titles only for Omaha & Hermann) ---
p_list <- lapply(seq_along(stations), function(i) {
  station <- stations[i]
  is_bottom <- i %% 3 == 0  # last in chunk row
  show_ylabel <- station %in% c("Missouri River at Omaha, NE", "Missouri River at Hermann, MO")
  
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(
      title = paste(station, "- Running Means"),
      x = if (is_bottom) "Year" else NULL,
      y = if (show_ylabel) "Normalized Streamflow (ft³/s)" else NULL
    ) +
    scale_color_manual(values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      strip.text = element_blank(),
      axis.title.x = if (is_bottom) element_text() else element_blank(),
      axis.title.y = if (show_ylabel) element_text() else element_blank()
    )
})

# --- Step 3: Spectrum Plots (titles only for Omaha & Hermann) ---
spec_list <- lapply(seq_along(stations), function(i) {
  station <- stations[i]
  is_bottom <- i %% 3 == 0
  show_ylabel <- station %in% c("Missouri River at Omaha, NE", "Missouri River at Hermann, MO")
  
  ggplot(filter(spectra_df, station_name == station),
         aes(x = period, y = spectrum, color = window)) +
    geom_line() +
    labs(
      title = paste(station, "- Spectrum"),
      x = if (is_bottom) "Period (Years)" else NULL,
      y = if (show_ylabel) "Spectral Density" else NULL
    ) +
    scale_color_manual(values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )) +
    theme_minimal(base_size = 12) +
    guides(color = "none") +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      strip.text = element_blank(),
      axis.title.x = if (is_bottom) element_text() else element_blank(),
      axis.title.y = if (show_ylabel) element_text() else element_blank()
    )
})

# --- Step 4: Pair Time Series | Spectrum side-by-side ---
paired_horizontal <- mapply(function(ts, spec) {
  ts | spec
}, ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# --- Step 5: Chunk plotting function ---
plot_station_chunk <- function(paired_list, chunk_index, chunk_size = 3) {
  start_idx <- (chunk_index - 1) * chunk_size + 1
  end_idx <- min(start_idx + chunk_size - 1, length(paired_list))
  subset_plots <- paired_list[start_idx:end_idx]
  
  # Stack vertically
  combined_plot <- Reduce(`/`, subset_plots) +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
  
  return(combined_plot)
}

# --- Step 6: Plot and save first 3 station pairs ---
plot_chunk_1 <- plot_station_chunk(paired_horizontal, chunk_index = 1)
print(plot_chunk_1)

ggsave("chunk_1_stations_pairplot.png", 
       plot = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 500)

# --- Step 7: Plot and save second 3 station pairs ---
plot_chunk_2 <- plot_station_chunk(paired_horizontal, chunk_index = 2)
print(plot_chunk_2)

ggsave("chunk_2_stations_pairplot.png", 
       plot = plot_chunk_2, width = 14, height = 4.5 * 3, dpi = 500)


###############

#Running means sh


###

library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)

# --- Load normalized data ---
data <- fread("/work/bmcdaniel/R_stuff/normalized_data.csv")

# --- Define selected stations ---
selected_stations <- c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500")
station_names <- c(
  "3611500" = "Station 1",
  "5420500" = "Station 2",
  "6610000" = "Station 3",
  "6934500" = "Station 4",
  "7020500" = "Station 5",
  "7355500" = "Station 6"
)

# --- Filter and assign station names ---
data <- data[ID %in% selected_stations]
data[, station_name := station_names[as.character(ID)]]

# --- Compute 20, 50, 100-year running means ---
data_long <- rbindlist(lapply(selected_stations, function(station_id) {
  df <- data[ID == station_id]
  station <- station_names[station_id]
  data.table(
    year = df$year,
    value = c(
      rollmean(df$Q_norm, 20, fill = NA, align = "center"),
      rollmean(df$Q_norm, 50, fill = NA, align = "center"),
      rollmean(df$Q_norm, 100, fill = NA, align = "center")
    ),
    window = rep(c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"), each = nrow(df)),
    station_name = station
  )
}))

# --- Compute power spectrum of original normalized Q for each station ---
compute_spectrum <- function(ts, years) {
  ts_clean <- ts[!is.na(ts)]
  spec <- spectrum(ts_clean, plot = FALSE)
  data.table(
    period = 1 / spec$freq,
    spectrum = spec$spec
  )[period <= length(years) / 2]
}

spectra_df <- rbindlist(lapply(selected_stations, function(station_id) {
  df <- data[ID == station_id]
  out <- compute_spectrum(df$Q_norm, df$year)
  out[, station_name := station_names[station_id]]
  out[, window := "Original Reconstruction"]
  return(out)
}))

# --- Time series plot list ---
p_list <- lapply(unique(data_long$station_name), function(station) {
  ggplot(data_long[station_name == station], aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(
      title = paste(station, "- Running Means"),
      x = "Year",
      y = "Normalized Streamflow (ft³/s)"
    ) +
    scale_color_manual(values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      axis.title.x = element_text(),
      axis.title.y = element_text()
    )
})

# --- Spectrum plot list (raw normalized streamflow only) ---
spec_list <- lapply(unique(spectra_df$station_name), function(station) {
  ggplot(spectra_df[station_name == station], aes(x = period, y = spectrum)) +
    geom_line(color = "black", linewidth = 0.8) +
    labs(
      title = paste(station, "- Spectrum"),
      x = "Period (Years)",
      y = "Spectral Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      axis.title.x = element_text(),
      axis.title.y = element_text()
    )
})

# --- Combine each time series and spectrum horizontally ---
paired_plots <- mapply(function(ts, spec) ts | spec,
                       ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# --- Stack all paired rows vertically ---
final_plot <- Reduce(`/`, paired_plots) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

# --- Show and save ---
print(final_plot)

ggsave("normalized_running_means_and_spectrum.png",
       plot = final_plot, width = 14, height = 4.5 * 6, dpi = 500)


############3
#############

library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)

# --- Load normalized data ---
data <- fread("/work/bmcdaniel/R_stuff/normalized_data.csv")

# Convert ID to character for merging
data[, ID := as.character(ID)]

# --- Selected stations and proper names ---
station_names_df <- data.frame(
  site_no = c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500"),
  station_name = c(
    "Ohio River at Metropolis, IL",
    "Mississippi River at Clinton, IA",
    "Missouri River at Omaha, NE",
    "Missouri River at Hermann, MO",
    "Mississippi River at Chester, IL",
    "Red River at Alexandria, LA"
  )
)

# Merge station names into data
data <- merge(data, station_names_df, by.x = "ID", by.y = "site_no")

# --- Compute 20, 50, 100-year running means ---
data_long <- rbindlist(lapply(unique(data$ID), function(station_id) {
  df <- data[ID == station_id]
  station <- df$station_name[1]
  data.table(
    year = df$year,
    value = c(
      rollmean(df$Q_norm, 20, fill = NA, align = "center"),
      rollmean(df$Q_norm, 50, fill = NA, align = "center"),
      rollmean(df$Q_norm, 100, fill = NA, align = "center")
    ),
    window = rep(c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"), each = nrow(df)),
    station_name = station
  )
}))

# --- Compute spectrum for each station (truncate at period = 100) ---
compute_spectrum <- function(ts, years) {
  ts_clean <- ts[!is.na(ts)]
  spec <- spectrum(ts_clean, plot = FALSE)
  data.table(
    period = 1 / spec$freq,
    spectrum = spec$spec
  )[period <= 155]  # Limit to period <= 100 years
}

spectra_df <- rbindlist(lapply(unique(data$ID), function(station_id) {
  df <- data[ID == station_id]
  out <- compute_spectrum(df$Q_norm, df$year)
  out[, station_name := df$station_name[1]]
  return(out)
}))

# --- Color palette for spectrum (alternate colors per station) ---
spectrum_colors <- c("#5e3c99", "#e66101", "#4393c3", "#5e3c99", "#e66101", "#4393c3")

# --- Station list ---
stations <- unique(data_long$station_name)

# --- Time Series Plots ---
p_list <- lapply(seq_along(stations), function(i) {
  station <- stations[i]
  is_bottom <- i %% 3 == 0
  show_ylabel <- station %in% c("Missouri River at Omaha, NE", "Missouri River at Hermann, MO")
  
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(
      title = paste(station, "- Running Means"),
      x = if (is_bottom) "Year" else NULL,
      y = if (show_ylabel) "Normalized Streamflow (ft³/s)" else NULL
    ) +
    scale_color_manual(values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      strip.text = element_blank(),
      axis.title.x = if (is_bottom) element_text() else element_blank(),
      axis.title.y = if (show_ylabel) element_text() else element_blank()
    )
})

# --- Spectrum Plots ---
spec_list <- lapply(seq_along(stations), function(i) {
  station <- stations[i]
  is_bottom <- i %% 3 == 0
  show_ylabel <- station %in% c("Missouri River at Omaha, NE", "Missouri River at Hermann, MO")
  
  ggplot(filter(spectra_df, station_name == station),
         aes(x = period, y = spectrum)) +
    geom_line(color = spectrum_colors[i], linewidth = 0.6) +  # thinner line
    labs(
      title = paste(station, "- Spectrum"),
      x = if (is_bottom) "Period (Years)" else NULL,
      y = if (show_ylabel) "Spectral Density" else NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      strip.text = element_blank(),
      axis.title.x = if (is_bottom) element_text() else element_blank(),
      axis.title.y = if (show_ylabel) element_text() else element_blank()
    )
})

# --- Pair plots horizontally ---
paired_horizontal <- mapply(function(ts, spec) ts | spec,
                            ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# --- Function to plot station chunks ---
plot_station_chunk <- function(paired_list, chunk_index, chunk_size = 3) {
  start_idx <- (chunk_index - 1) * chunk_size + 1
  end_idx <- min(start_idx + chunk_size - 1, length(paired_list))
  subset_plots <- paired_list[start_idx:end_idx]
  Reduce(`/`, subset_plots) +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
}

# --- Plot and save first 3 stations ---
plot_chunk_1 <- plot_station_chunk(paired_horizontal, chunk_index = 1)
print(plot_chunk_1)
ggsave("chunk_1_stations_pairplot.png", 
       plot = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 500)

# --- Plot and save second 3 stations ---
plot_chunk_2 <- plot_station_chunk(paired_horizontal, chunk_index = 2)
print(plot_chunk_2)
ggsave("chunk_2_stations_pairplot.png", 
       plot = plot_chunk_2, width = 14, height = 4.5 * 3, dpi = 500)











#####Log

library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)

# --- Load normalized data ---
data <- fread("/work/bmcdaniel/R_stuff/normalized_data.csv")

# Convert ID to character for merging
data[, ID := as.character(ID)]

# --- Selected stations and proper names ---
station_names_df <- data.frame(
  site_no = c("3611500", "5420500", "6610000", "6934500", "7020500", "7355500"),
  station_name = c(
    "Ohio River at Metropolis, IL",
    "Mississippi River at Clinton, IA",
    "Missouri River at Omaha, NE",
    "Missouri River at Hermann, MO",
    "Mississippi River at Chester, IL",
    "Red River at Alexandria, LA"
  )
)

# Merge station names into data
data <- merge(data, station_names_df, by.x = "ID", by.y = "site_no")

# --- Compute 20, 50, 100-year running means ---
data_long <- rbindlist(lapply(unique(data$ID), function(station_id) {
  df <- data[ID == station_id]
  station <- df$station_name[1]
  data.table(
    year = df$year,
    value = c(
      rollmean(df$Q_norm, 20, fill = NA, align = "center"),
      rollmean(df$Q_norm, 50, fill = NA, align = "center"),
      rollmean(df$Q_norm, 100, fill = NA, align = "center")
    ),
    window = rep(c("20-Year Running Mean", "50-Year Running Mean", "100-Year Running Mean"), each = nrow(df)),
    station_name = station
  )
}))

# --- Compute spectrum for each station (linear spectral density, log-scale period axis) ---
compute_spectrum <- function(ts, years) {
  ts_clean <- ts[!is.na(ts)]
  spec <- spectrum(ts_clean, plot = FALSE)
  data.table(
    period = 1 / spec$freq,
    spectrum = spec$spec
  )[period <= 155]
}

spectra_df <- rbindlist(lapply(unique(data$ID), function(station_id) {
  df <- data[ID == station_id]
  out <- compute_spectrum(df$Q_norm, df$year)
  out[, station_name := df$station_name[1]]
  return(out)
}))

# --- Color palette for spectrum (alternate colors per station) ---
spectrum_colors <- c("#5e3c99", "#e66101", "#4393c3", "#5e3c99", "#e66101", "#4393c3")

# --- Station list ---
stations <- unique(data_long$station_name)

# --- Time Series Plots ---
p_list <- lapply(seq_along(stations), function(i) {
  station <- stations[i]
  is_bottom <- i %% 3 == 0
  show_ylabel <- station %in% c("Missouri River at Omaha, NE", "Missouri River at Hermann, MO")
  
  ggplot(filter(data_long, station_name == station),
         aes(x = year, y = value, color = window)) +
    geom_line() +
    labs(
      title = paste(station, "- Running Means"),
      x = if (is_bottom) "Year" else NULL,
      y = if (show_ylabel) "Normalized Streamflow (ft³/s)" else NULL
    ) +
    scale_color_manual(values = c(
      "20-Year Running Mean" = "#5e3c99",
      "50-Year Running Mean" = "#e66101",
      "100-Year Running Mean" = "#4393c3"
    )) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(title = NULL)) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      strip.text = element_blank(),
      axis.title.x = if (is_bottom) element_text() else element_blank(),
      axis.title.y = if (show_ylabel) element_text() else element_blank()
    )
})

# --- Spectrum Plots (linear spectral density, log10 x-axis) ---
spec_list <- lapply(seq_along(stations), function(i) {
  station <- stations[i]
  is_bottom <- i %% 3 == 0
  show_ylabel <- station %in% c("Missouri River at Omaha, NE", "Missouri River at Hermann, MO")
  
  ggplot(filter(spectra_df, station_name == station),
         aes(x = period, y = spectrum)) +
    geom_line(color = spectrum_colors[i], linewidth = 0.6) +
    scale_x_log10() +
    labs(
      title = paste(station, "- Spectrum"),
      x = if (is_bottom) "Year" else NULL,
      y = if (show_ylabel) "Spectral Density" else NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      strip.text = element_blank(),
      axis.title.x = if (is_bottom) element_text() else element_blank(),
      axis.title.y = if (show_ylabel) element_text() else element_blank()
    )
})

# --- Pair plots horizontally ---
paired_horizontal <- mapply(function(ts, spec) ts | spec,
                            ts = p_list, spec = spec_list, SIMPLIFY = FALSE)

# --- Function to plot station chunks ---
plot_station_chunk <- function(paired_list, chunk_index, chunk_size = 3) {
  start_idx <- (chunk_index - 1) * chunk_size + 1
  end_idx <- min(start_idx + chunk_size - 1, length(paired_list))
  subset_plots <- paired_list[start_idx:end_idx]
  Reduce(`/`, subset_plots) +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
}

# --- Plot and save first 3 stations ---
plot_chunk_1 <- plot_station_chunk(paired_horizontal, chunk_index = 1)
print(plot_chunk_1)
ggsave("chunk_1_stations_log_spectrum.png", 
       plot = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 500)

# --- Plot and save second 3 stations ---
plot_chunk_2 <- plot_station_chunk(paired_horizontal, chunk_index = 2)
print(plot_chunk_2)
ggsave("chunk_2_stations_log_spectrum.png", 
       plot = plot_chunk_2, width = 14, height = 4.5 * 3, dpi = 500)

###########