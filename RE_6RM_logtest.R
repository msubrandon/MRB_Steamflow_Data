### Log Time Test

library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)

#normalized data
norm <- fread("/work/bmcdaniel/R_stuff/normalized_data.csv")

View(norm)

#convert the ID to character 
norm[, ID := as.character(ID)]

# Change the names
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

norm <- merge(norm, station_names_df, by.x = "ID", by.y = "site_no")

# 20, 50, 100-year running means
data_long <- rbindlist(lapply(unique(norm$ID), function(station_id) {
  df <- norm[ID == station_id]
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

###########
# POWERSPEC
compute_spectrum <- function(ts, years) {
  ts_clean <- ts[!is.na(ts)]            # drop NAs
  spec <- spectrum(ts_clean, plot = FALSE)  # PS using stats::spectrum()
  data.table(
    period   = 1 / spec$freq,           # Convert frequency (cycles/year) into period (years)
    spectrum = spec$spec                # Spectral density 
  )[period <= 155]                      # Keep periods of 155 years (kept these as the PS tapers off after this)
}

spectra_df <- rbindlist(lapply(unique(norm$ID), function(station_id) {
  df  <- norm[ID == station_id]
  out <- compute_spectrum(df$Q_norm, df$year) # computes power spectrum for a station's normalized streamflow
  out[, station_name := df$station_name[1]]   # Label with station name
  out
}))

# Power spectrum colors
spectrum_colors <- c("#5e3c99", "#e66101", "#4393c3", "#5e3c99", "#e66101", "#4393c3")

stations <- unique(data_long$station_name)

# Running means plots
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

# Powerspec plots
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

###PLOTS
paired_horizontal <- mapply(function(ts, spec) ts | spec,
                            ts = p_list, spec = spec_list, SIMPLIFY = FALSE)


plot_station_chunk <- function(paired_list, chunk_index, chunk_size = 3) {
  start_idx <- (chunk_index - 1) * chunk_size + 1
  end_idx <- min(start_idx + chunk_size - 1, length(paired_list))
  subset_plots <- paired_list[start_idx:end_idx]
  Reduce(`/`, subset_plots) +
    plot_layout(guides = "collect") &
    theme(legend.position = 'bottom')
}


plot_chunk_1 <- plot_station_chunk(paired_horizontal, chunk_index = 1)
print(plot_chunk_1)
#ggsave("chunk_1_stations_log_spectrum.png", 
      # plot = plot_chunk_1, width = 14, height = 4.5 * 3, dpi = 500)


plot_chunk_2 <- plot_station_chunk(paired_horizontal, chunk_index = 2)
print(plot_chunk_2)
#ggsave("chunk_2_stations_log_spectrum.png", 
      # plot = plot_chunk_2, width = 14, height = 4.5 * 3, dpi = 500)

