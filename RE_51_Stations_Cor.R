#Station we test
s1 <- c('06934500')

## Merge the MADA with the instrumental data and calculate correlations for each combination
subCor <- merge(nada, instQ[ID %in% s1], by = 'year'
)[,  #These are both data tables and merge them,merges nada and instQ (station ID s1) by the ‘year’ column, ((matching ‘year’ value??))
  {
    ct <- cor.test(pdsi, Qa)  #Doing a correlation test between the two 
    list(rho = ct$estimate, p.value = ct$p.value)    #finds the correlation coefficient and p-value, the list 
  },
  by = .(ID, lon, lat) #coverts to a numeric 
][instQmeta[, .(ID, name, lon, lat)], on = 'ID', nomatch = NULL] #Joins data with instaQmeta, and does not incluse them without a mathc

setnames(subCor, c('i.lon', 'i.lat'), c('Qlon', 'Qlat')) #changes the names of these columsn


## Determine significance
subCor[, signif := p.value < 0.05] #set the significance 
## Determine the boundaries of the significance area. 
## This is done with the signif_area() function
setkey(subCor, lon, lat) #Setkey. seems to sort by a specific column (lat, lon)

subCor[, point := .GRP, by = .(lon, lat)]

subCorSignif <- subCor[, signif_area(.SD, 0.5, 0.5), by = .(ID, name)] # Fids the significance area #MAY HAVE TO LOOK IN TO TAKE LONG
corPlot <- ggplot(subCor) +  #Using subCor dataset
  geom_tile(aes(lon, lat, fill = rho), width = 0.5, height = 0.5) +   #This creates a raster layer that fills in the roe values, over lat lon
  geom_segment(aes(x = lon - 0.25, xend = lon + 0.25, y = lat + 0.25, yend = lat + 0.25), #These segments layers draw lines significant areas
               subCorSignif[{top}], size = 0.1) +
  geom_segment(aes(x = lon - 0.25, xend = lon + 0.25, y = lat - 0.5, yend = lat - 0.25),
               subCorSignif[{bottom}], size = 0.1) +
  geom_segment(aes(x = lon - 0.25, xend = lon - 0.25, y = lat - 0.5, yend = lat + 0.25),
               subCorSignif[{left}], size = 0.1) +
  geom_segment(aes(x = lon + 0.25, xend = lon + 0.25, y = lat - 0.5, yend = lat + 0.25),
               subCorSignif[{right}], size = 0.1) +
  geom_point(aes(Qlon, Qlat), colour = 'red') + ##This shows the location of the station that is being referenced 
  scale_x_continuous(expand = c(0, 0), labels = pasteLong) + #This shows how the labels should be formatted (in this case long)
  scale_y_continuous(expand = c(0, 0), labels = pasteLat) + #This shows how the labels should be formatted (in this case lat)
  scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1, #shows the color scale for the cor
                       breaks = scales::pretty_breaks(3), limits = absRange(subCor$rho)) + #This sets the number of breaks on the legend , range
  coord_quickmap() + # coordinate system approximation
  labs(x = NULL, y = NULL) + #remoeves the axis label 
  facet_wrap(~name, ncol = 1, strip.position = 'right') + #creates a sepate plot for each level of the name, in a single columns
  theme_bw() + #black and white theme to the plot 
  theme(panel.grid = element_blank(), #removed the grid lines
        axis.line = element_blank(), #Removes the axis line
        legend.position = 'top', #positions axis to the top
        legend.key.width = unit(0.6, 'cm'), #sets the width on the legend
        strip.background = element_blank()) #removes the backgroung of facts labels

corPlot




########## All of the 51 stations looped ############

#all of the stations
station_ids <- unique(instQ$ID)

#plot
for (s1 in station_ids) {
  
  # Merge the MADA with the instrumental data and calculate correlations for the current station
  subCor <- merge(nada, instQ[ID %in% s1], by = 'year'
  )[,  
    {
      ct <- cor.test(pdsi, Qa)  # Doing a correlation test between the two
      list(rho = ct$estimate, p.value = ct$p.value)  # Get correlation coefficient and p-value
    },
    by = .(ID, lon, lat)
  ][instQmeta[, .(ID, name, lon, lat)], on = 'ID', nomatch = NULL]
  
  setnames(subCor, c('i.lon', 'i.lat'), c('Qlon', 'Qlat'))  # Rename columns
  
  #  significance
  subCor[, signif := p.value < 0.05]
  setkey(subCor, lon, lat)
  subCor[, point := .GRP, by = .(lon, lat)]
  
  # Find significance 
  subCorSignif <- subCor[, signif_area(.SD, 0.5, 0.5), by = .(ID, name)]
  
  # plot
  corPlot <- ggplot(subCor) +
    geom_tile(aes(lon, lat, fill = rho), width = 0.5, height = 0.5) +  # correlation
    geom_segment(aes(x = lon - 0.25, xend = lon + 0.25, y = lat + 0.25, yend = lat + 0.25),
                 subCorSignif[{top}], size = 0.1) +
    geom_segment(aes(x = lon - 0.25, xend = lon + 0.25, y = lat - 0.5, yend = lat - 0.25),
                 subCorSignif[{bottom}], size = 0.1) +
    geom_segment(aes(x = lon - 0.25, xend = lon - 0.25, y = lat - 0.5, yend = lat + 0.25),
                 subCorSignif[{left}], size = 0.1) +
    geom_segment(aes(x = lon + 0.25, xend = lon + 0.25, y = lat - 0.5, yend = lat + 0.25),
                 subCorSignif[{right}], size = 0.1) +
    geom_point(aes(Qlon, Qlat), colour = 'red') +  # station location in red
    scale_x_continuous(expand = c(0, 0), labels = pasteLong) +  # longitude labels
    scale_y_continuous(expand = c(0, 0), labels = pasteLat) +  # atitude labels
    scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1,
                         breaks = scales::pretty_breaks(3), limits = absRange(subCor$rho)) +
    coord_quickmap() +
    labs(x = NULL, y = NULL) +  
    facet_wrap(~name, ncol = 1, strip.position = 'right') +  # Facet by station name
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          legend.position = 'top',
          legend.key.width = unit(0.6, 'cm'),
          strip.background = element_blank())
  

  print(corPlot)
}

###############

#3x3 maps

###############


library(ggplot2)
library(patchwork)

#plots
plot_list <- list()

#all station IDs
station_ids <- unique(instQ$ID)

# Loop anD plot
for (s1 in station_ids) {
  
  #merge the MADA with the instrumental data and calculate correlations
  subCor <- merge(nada, instQ[ID %in% s1], by = 'year'
  )[,  
    {
      ct <- cor.test(pdsi, Qa)  #correlation 
      list(rho = ct$estimate, p.value = ct$p.value)  
    },
    by = .(ID, lon, lat)
  ][instQmeta[, .(ID, name, lon, lat)], on = 'ID', nomatch = NULL] 
  
  setnames(subCor, c('i.lon', 'i.lat'), c('Qlon', 'Qlat'))  # columns
  
  # Generate plot
  corPlot <- ggplot(subCor) +
    geom_tile(aes(lon, lat, fill = rho), width = 0.5, height = 0.5) +  
    geom_point(aes(Qlon, Qlat), colour = 'red') +  
    scale_x_continuous(expand = c(0, 0), labels = pasteLong) +  
    scale_y_continuous(expand = c(0, 0), labels = pasteLat) +  
    scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1,
                         breaks = scales::pretty_breaks(3), limits = absRange(subCor$rho)) +
    coord_quickmap() +
    labs(x = NULL, y = NULL) +  
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          legend.position = 'top',
          legend.key.width = unit(0.6, 'cm'),
          strip.background = element_blank())
  
  plot_list[[s1]] <- corPlot
}

# 3x3 grids in batches
plot_in_batches <- function(plots, batch_size = 9, cols = 3) {
  n <- length(plots)
  
  # Loop
  for (i in seq(1, n, by = batch_size)) {
    batch <- plots[i:min(i + batch_size - 1, n)]  
    combined_plot <- wrap_plots(batch, ncol = cols)  
    print(combined_plot)  
  }
}

# Call the function to display 3x3 grids
plot_in_batches(plot_list, batch_size = 9, cols = 3)



