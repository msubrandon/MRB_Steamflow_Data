
library(ggplot2)
library(dplyr)
library(zoo)

# Load the normalized data
data <- read.csv("/work/bmcdaniel/R_stuff/normalized_data.csv")

#  calculate running mean
calculate_running_mean <- function(data, window) {
  data %>%
    group_by(site_no) %>%
    arrange(year) %>%
    mutate(running_mean = rollmean(Q_normalized, k = window, fill = NA, align = 'right')) %>%
    ungroup()
}

# Calculate 
data_20yr <- calculate_running_mean(data, 20)
data_50yr <- calculate_running_mean(data, 50)
data_100yr <- calculate_running_mean(data, 100)

# Combine 
data_combined <- data_20yr %>%
  rename(running_mean_20yr = running_mean) %>%
  left_join(data_50yr %>% rename(running_mean_50yr = running_mean), by = c("site_no", "year", "Q_normalized")) %>%
  left_join(data_100yr %>% rename(running_mean_100yr = running_mean), by = c("site_no", "year", "Q_normalized"))

# Function to create time 
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

# Create time series plots for each station 
create_time_series_plots(unique_stations)

# Function to create time series plots for each statio
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


 = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 300)





