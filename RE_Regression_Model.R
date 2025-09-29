#This file will be my take on the reconstruction
#install.packages(c('ldsr', 'patchwork', 'cowplot', 'VSURF', 
#                   'sf', 'geosphere', 'doFuture', 'dplR', 'glue'))
#install.packages("RNetCDF")
#install.packages("meteR")
#install.packages("doRNG")
library(metR)
library(data.table)
library(ggplot2)
library(doFuture)
library(doRNG)
library(microbenchmark)
source('/work/bmcdaniel/R_stuff/init.R')                  # Packages and utilities (functions, variables)
source('/work/bmcdaniel/R_stuff/geo_functions.R')         # Geographical processing functions
source('/work/bmcdaniel/R_stuff/correlation_functions.R') # Correlation tools
source('/work/bmcdaniel/R_stuff/plot_metric_map.R')       # For Figure 4
source('/work/bmcdaniel/R_stuff/flow_history.R')       # For Figures 5 and S5
options(digits = 4)                 # For concise printing

#The data was collected as montley streamflow discharge data

instQmonthly <- fread('/work/bmcdaniel/R_stuff/combined_data_data.csv', key = 'ID',
                      colClasses = c(ID = 'character'))          # Instrumental streamflow
View(instQmonthly)
#THis reads in the lat lon points. 
instQmeta <- fread('/work/bmcdaniel/R_stuff/processed_lat_lon_points.csv', 
                   colClasses = c(ID = 'character'),
                   key = 'ID')
View(instQmeta)
#This uses the lat lons and applies them to the station data.

names(stationIDs) <- stationIDs <- instQmeta$ID 


## IDs of available stations (such as the ones along the rivers)
names(availIDs) <- availIDs <- unique(instQmonthly$ID) 

##parameters
instQmeta <- instQmeta[ID %in% unique(instQmonthly$ID)]
pRange <- c('0' = 0, '0.5' = 0.5, '2/3' = 2/3, '1' = 1, '1.5' = 1.5, '2' = 2) #Keep these the same
names(pNames) <- pNames <- names(pRange)

################################################


#################### DATA #######################


#################################################


#########################


## CONVERT THE RIVERS ###


#########################

instQ <- instQmonthly[, .(Qa = mean(Qm)), by = .(ID, year)]
View(instQ)

View(instQ)


#########################


####### DESNISITY #######


#########################




library(e1071)  # for skewness()

# Calculate skewness before and after log transform
skew_info <- instQ[, .(
  skew_orig = skewness(Qa, na.rm = TRUE),
  skew_log  = skewness(log(Qa), na.rm = TRUE)
), by = ID]

# Count how many stations have reduced skewness after log-transform
log_transformed_count <- skew_info[abs(skew_log) < abs(skew_orig), .N]

print(log_transformed_count)

####
library(e1071)

# Determine which stations benefit from log-transformation
trans_flags <- instQ[, .(
  skew_orig = skewness(Qa, na.rm = TRUE),
  skew_log  = skewness(log(Qa), na.rm = TRUE)
), by = ID][, .(ID, use_log = abs(skew_log) < abs(skew_orig))]

# Merge with original data
instQ_with_flag <- merge(instQ, trans_flags, by = "ID")

# Apply transformation conditionally
instQ_with_flag[, Qa_trans := ifelse(use_log, log(Qa), Qa)]
instQ_with_flag[, Qa_std := standardize(Qa_trans), by = ID]

#########################


###### NADA GRIDS #######


#########################

#########################


###### COR MATRIX #######


#########################

lon_limits <- c(-125, -65)
lat_limits <- c(25, 50)
#This gets the data into amatrix
nada <- ReadNetCDF('/work/bmcdaniel/R_stuff/nada_hd2_cl.nc', 'pdsi')[!is.na(pdsi)][which(pdsi > -99.999)] #Reads in the dataset
dt_wide <- dcast(nada, time ~ paste(lon, lat, sep = "_"), value.var = "pdsi")
#pdsi_matrix <- as.matrix(dt_wide[, -1, with = FALSE])
pdsi_matrix <- as.matrix(dt_wide[, -1])
pdsi_matrix
nada2mat <- pdsi_matrix
print(nada2mat)
head(pdsi_matrix)
# 
# 
# #####
lon_limits <- c(-125, -65)
lat_limits <- c(25, 50)


nada <- ReadNetCDF('/work/bmcdaniel/R_stuff/nada_hd2_cl.nc', 'pdsi')[!is.na(pdsi) & pdsi > -99.999]
# Convert 
dt_wide <- dcast(nada, time ~ paste(lon, lat, sep = "_"), value.var = "pdsi")
# Convert 
pdsi_matrix <- as.matrix(dt_wide[, -1])  
print(pdsi_matrix)

nada2mat <- pdsi_matrix
print(nada2mat)
####



row.names(nada2mat) <- 0:2005 #THINK THIS IS THE PROBLEn ###DO thees need to be same leght
#This removes the years 1-1200 from the dataset
nada2mat <- nada2mat[-(1:1200), ]

setnames(nada, 'time', 'year')
#This sets up the benchmark

# microbenchmark::mrobenchmark(
nadaxy <- nada[, .SD[1], by = .(lon, lat)]

#THis is the plot of the United States (Idit this to ger better map)
ggplot(nadaxy) +
  geom_point(aes(lon, lat, color = year), size = 0.1)
  
  #######

output_file <- "/work/bmcdaniel/R_stuff/nada2mat.csv"

write.csv(nada2mat, file = output_file, row.names = FALSE)

print(paste("File saved to:", output_file))

#################################################


############## RECONSTRUCTION ###################


#################################################


#########################


#SELECT NADA GRID POINTS#


#########################

#This selects the station that I will use. 
s1 <- c('06934500')

## Merge the MADA with the instrumental data and calculate correlations for each combination
subCor <- merge(nada, instQ[ID %in% s1], by = 'year'
)[,  #These are both data tables and merge them,merges nada and instQ (station ID s1) by the ‘year’ column, ((matching ‘year’ value??))
  {
    ct <- cor.test(pdsi, Qa)  #Doing a correlation test between the two 
    list(rho = ct$estimate, p.value = ct$p.value)    #finds the correlation coefficient and p-value, the list 
  },
  by = .(ID, lon, lat) #coverts
][instQmeta[, .(ID, name, lon, lat)], on = 'ID', nomatch = NULL] #Joins data with instaQmeta, and does not incluse them without a mathc

setnames(subCor, c('i.lon', 'i.lat'), c('Qlon', 'Qlat')) #changes the names of these columsn

# subCor[, name := factor(name, levels = instQmeta[s2, name])] #modifies to specific levels

##  significance
subCor[, signif := p.value < 0.05] #set the significance 
## Determine the 
## This is done with the signif_area() function
setkey(subCor, lon, lat) #Setkey. seems to sort by a specific column (lat, lon)

subCor[, point := .GRP, by = .(lon, lat)]

subCorSignif <- subCor[, signif_area(.SD, 0.5, 0.5), by = .(ID, name)] # Fids the significance area #MAY HAVE TO LOOK IN TO TAKE LONG
corPlot <- ggplot(subCor) +  
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




#########################


######### PCA ###########


#########################

hasNA <- apply(nada2mat, 2, \(x) any(is.na(x)))
nada2mat <- nada2mat[, !hasNA]

instQ <- instQ[year <= 2005]
corMat <- 
  instQ[, 
        {
          ind <- which(1200:2005 %in% year)
          as.data.frame(cor(Qa, nada2mat[ind, ], use = 'complete.obs'))
        },
        keyby = ID] %>% 
  as.matrix(rownames = TRUE)

setkey(instQ, ID)
#instQ <- instQ[ID != "02489500"]



#doFuture::registerDoFuture()
#future::plan(future::multisession(workers = 4))

pca_results <- list() 
for (i in 1:nrow(instQmeta)) {
  s <- instQmeta[i, ID]
  corVector <- corMat[s, ]
  idx <- which(corVector > 0.4)
  if (length(idx) > 0) {
    X <- nada2mat[, idx]
    #check 
    
    if (nrow(X) > 1 && ncol(X) > 1) {
      pca <- prcomp(X)      
      pca_results[[as.character(s)]] <- pca  
    } else {
      pca_results[[as.character(s)]] <- NULL# 
    }
  } else {
    pca_results[[as.character(s)]] <- NULL 
  }
}

# see how many PCs are there
sapply(pca_results, \(x) ncol(x$x))

#print #use the list 
for (station in names(pca_results)) {
  cat("Station ID:", station, "\n")
  print(pca_results[[station]])
  cat("\n")
}
ivs <- foreach(s = availIDs,  # for loop where s loops over all the availIDs
               .packages = 'data.table',
               .final = function(x) setNames(x, availIDs)) %dorng% { # Specifies to use the final function
                 message("Processing ID: ", s)  # This will print as I proceed
                 if (is.null(pca_results[[s]])) {
                   message("Skipping ID ", s, ": PCA results are NULL.") #It should tell me that it will skop
                   return(NULL)
                 }
                 Qa <- instQ[s][!is.na(Qa)]
                 idx <- which(1200:2005 %in% Qa$year)
                 pca_data <- pca_results[[s]]$x
                 if (is.null(pca_data) || length(pca_data) == 0) {
                   message("Skipping ID ", s, ": PCA data is NULL or empty.")
                   return(NULL)  # Skip  if pca_data is NULL or empty
                 }
                 pca_data <- pca_data[idx, , drop = FALSE]
                 input_selection(pca_results[[s]]$x[idx, , drop = FALSE], Qa$Qa, 
                                 nvmax = 8, method = 'VSURF', parallel = FALSE)
               }


?input_selection



#########################


#### CROSS VALIDATION ###


#########################




cvPoints <- lapply(split(instQ, by = 'ID'), function(DT) {
  print(DT$ID[1])
  make_Z(DT$Qa, frac = 0.25, nRuns = 30, contiguous = TRUE)  
})

trans <- instQ[, .(trans = ifelse(abs(hinkley(log(Qa))) < abs(hinkley(Qa)), 'log', 'none')), 
               by = ID]

print(trans) #It worked
print(sum(is.na(trans$trans))) #0 bad values

library(glue)

doFuture::registerDoFuture()
future::plan(future::multisession)

s <- availIDs[42]
scores <- 
  foreach(s = availIDs, .combine = rbind) %dopar% {
    Qa <- instQ[s]
    Z <- cvPoints[[s]]
    transform <- trans[ID == s, trans]
    obs <- if (transform == 'log') log(Qa$Qa) else Qa$Qa
    
    selectedPC <- ivs[[s]]
    pcs <- as.data.table(pca_results[[s]]$x[, selectedPC, drop = FALSE])
    
    out <- cvPCR(Qa, pcs, 1200, transform = transform, Z = Z)$metrics
    out[, ID := s]
  }

s
######################

##################

### actual recon #######

##################

#####################



reconst <- 
  foreach(s = availIDs, .combine = rbind) %dopar% {
    
    Qa <- instQ[s]
    Z <- cvPoints[[s]]
    transform <- trans[ID == s, trans]
    obs <- if (transform == 'log') log(Qa$Qa) else Qa$Qa
    
    selectedPC <- ivs[[s]]
    pcs <- as.data.table(pca_results[[s]]$x[, selectedPC, drop = FALSE])
    out <- PCR_reconstruction(Qa, pcs, 1200, transform = transform)$rec
    out[, ID := s]
  }

print (reconst)


pc_count <- sapply(ivs, function(selectedPC) length(selectedPC))

# Print the number of PCs used for each station
pc_count


####
pc_data <- data.frame(
  Station = names(pc_count),
  NumPCs = pc_count
)

ggplot(pc_data, aes(x = Station, y = NumPCs)) +
  geom_bar(stat = "identity",fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Principal Components Used per Station",
       x = "Station",
       y = "Number of PCs")


################# RESULTS #######################



#########################


###### Score MAP   #####


#########################








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

# Create the plot for the  (CE)
p_ce <- create_metric_plot(scoreDT, 'CE') +
  theme(plot.tag.position = c(0.16, 1.15),
        plot.margin = margin(t = 1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm'))

print(p_ce)




##############




##### ###Cross cor### ####



#####

# Correlations in instrumental data
#trimmedIDs <- trim_ID(instQmeta[order(ID), ID]) #chan
instCorMat <- instQ[, dcast(.SD, year ~ ID, value.var = 'Qa')
][, cor(.SD, use = 'pairwise.complete'), .SDcols = -1]
instCor <- data.table(instCorMat, keep.rownames = 'ID1') %>% 
  melt(id.var = 'ID1', variable.name = 'ID2', value.name = 'rho', variable.factor = FALSE)
instCor[, c('ID1', 'ID2') := lapply(.SD, function(x) factor(x)),
        .SDcols = c('ID1', 'ID2')]
instCor <- instCor[as.numeric(ID1) < as.numeric(ID2)]


setkey(reconst, ID)

# Corelations in reconstructions
# Where instrumental data are available, use the same years as instrumental data
# Otherwise, use a nominal period 1950:2012
recCorMat <- rbind(reconst[instQ[, .(ID, year)], on = c('ID', 'year')],
                   reconst[!instQ][year %in% 1950:2012]
)[, dcast(.SD, year ~ ID, value.var = 'Q')
][, cor(.SD, use = 'pairwise.complete'), .SDcols = -1]

recCor <- data.table(recCorMat, keep.rownames = 'ID1') %>% 
  melt(id.var = 'ID1', variable.name = 'ID2', value.name = 'rho', variable.factor = FALSE)

recCor[, c('ID1', 'ID2') := lapply(.SD, function(x) factor(x)),
       .SDcols = c('ID1', 'ID2')]

recCor <- recCor[as.numeric(ID1) > as.numeric(ID2)]
limits <- absRange(c(instCor$rho, recCor$rho))
DT <- rbind(instCor, recCor)
ggplot(DT) +
  geom_tile(aes(ID1, ID2, fill = rho)) +
  geom_line(aes(x, y), data.table(x = c(0.5, length(unique(DT$ID1)) + 0.5), y = c(0.5, length(unique(DT$ID2)) + 0.5)), 
            size = 1.5, lineend = 'round') +
  coord_equal() +
  scale_x_discrete(limits = unique(DT$ID1)) +
  scale_y_discrete(limits = unique(DT$ID2)) +
  scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1, 
                       breaks = seq(-0.8, 0.8, 0.2), limits = limits) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
        axis.text.y = element_text(vjust = 0.5, size = 7),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.key.height = unit(2, 'cm'))




##############


############

#########################


###### RECORD LENGTH ####


#########################


instN <- instQ[, .(num_years = uniqueN(year)), by = ID]

ggplot(instN) +
  geom_bar(aes(x = num_years), fill = blues9[6]) +
  labs(x = 'Number of non-missing years', y = 'Number of stations') +
  scale_x_binned(breaks = seq(15, 150, 5), expand = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 20, 2), expand = c(0, 0)) +
  theme(axis.line = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE)




########### Stats #########


### PCs kept at each station 

# This is the PCs per station
pc_count <- sapply(ivs, function(selectedPC) length(selectedPC))
pc_count


#variance explained by pc. So this shows what PCs are explaing the most variance. 
sapply(pca_results, \(pca) if (!is.null(pca)) summary(pca)$importance[2, ] else NULL)


#shows the first 8 variance 
sapply(pca_results, \(pca) if (!is.null(pca)) sum(summary(pca)$importance[2, 1:8]) else NULL)



#scree plot
lapply(pca_results, function(pca) if (!is.null(pca)) plot(pca, type="l"))




#### PC kept all

# Count the number of PCs selected for each station
pc_count <- sapply(ivs, function(selectedPC) length(selectedPC))

# Calculate the total number of PCs kept across all stations
total_pcs_kept <- sum(pc_count, na.rm = TRUE)

print(paste("Total number of PCs kept across all stations:", total_pcs_kept))
###


##

print(paste("Total number of PCs created across all stations:", total_pcs_created))

###### 26,236 were kept total. some went up to 8+

##




library(data.table)

# Safely extract up to top 3 selected PCs and their variance explained
top3_pc_variance <- lapply(names(ivs), function(station_id) {
  selected <- ivs[[station_id]]
  pca <- pca_results[[station_id]]
  
  if (!is.null(pca) && !is.null(selected) && length(selected) >= 1) {
    prop_var <- summary(pca)$importance[2, ]
    selected <- selected[!is.na(selected)]  # extra safeguard
    max_n <- min(3, length(selected), length(prop_var))
    
    data.table(
      Station = station_id,
      PC = paste0("PC", selected[1:max_n]),
      VarianceExplained = round(prop_var[selected[1:max_n]], 4)
    )
  } else {
    NULL
  }
})
####









# Clean: remove columns in nada2mat that are all NA
nada2mat_clean <- nada2mat[, colSums(is.na(nada2mat)) < nrow(nada2mat)]

# Initialize list
pca_results <- list()

# Loop through each station
for (i in seq_len(nrow(instQmeta))) {
  s <- instQmeta[i, ID]
  
  # Check that correlation vector exists
  if (!s %in% rownames(corMat)) next
  
  corVector <- corMat[s, ]
  idx <- which(corVector > 0.4)
  
  if (length(idx) > 1) {
    X <- nada2mat_clean[, idx, drop = FALSE]
    
    # Remove rows with any NA values
    X <- X[complete.cases(X), , drop = FALSE]
    
    # Run PCA only if enough observations
    if (nrow(X) > 10 && ncol(X) > 1) {
      message("Running PCA for station: ", s)
      pca_results[[s]] <- prcomp(X, center = TRUE, scale. = TRUE)
    } else {
      message("Skipping ", s, ": Not enough valid data (", nrow(X), " rows, ", ncol(X), " cols).")
      pca_results[[s]] <- NULL
    }
  } else {
    message("Skipping ", s, ": No gridpoints with cor > 0.4.")
    pca_results[[s]] <- NULL
  }
}

length(pca_results)
sum(!sapply(pca_results, is.null))  # Should be > 0 now


sum(!sapply(pca_results, is.null))


# View PCA result for a station
example_station <- names(pca_results)[[1]]
print(summary(pca_results[[example_station]]))
#######
sum(!sapply(pca_results, is.null))  # Expect > 0
sum(sapply(ivs, function(x) !is.null(x) && length(x) > 0))  # Should be > 0
valid_stations <- intersect(
  names(pca_results)[!sapply(pca_results, is.null)],
  names(ivs)[sapply(ivs, function(x) !is.null(x) && length(x) > 0)]
)
print(length(valid_stations))
print(valid_stations)


#######
selected <- ivs[["06934500"]]
pca <- pca_results[["06934500"]]
prop_var <- summary(pca)$importance[2, ]

print(selected)
print(length(prop_var))
print(selected[selected <= length(prop_var)])


#
#######
#Workong code
#####


top3_pc_variance <- lapply(valid_stations, function(station_id) {
  selected_labels <- ivs[[station_id]]
  pca <- pca_results[[station_id]]
  
  if (is.null(pca) || is.null(selected_labels) || length(selected_labels) == 0) return(NULL)
  
  # Convert "PC1" to 1, "PC29" to 29, etc.
  selected_nums <- as.integer(gsub("PC", "", selected_labels))
  
  prop_var <- summary(pca)$importance[2, ]
  selected_valid <- selected_nums[!is.na(selected_nums) & selected_nums <= length(prop_var)]
  
  if (length(selected_valid) == 0) return(NULL)
  
  max_n <- min(3, length(selected_valid))
  
  data.table(
    Station = station_id,
    PC = paste0("PC", selected_valid[1:max_n]),
    VarianceExplained = round(prop_var[selected_valid[1:max_n]], 4)
  )
})

top3_pc_variance_dt <- rbindlist(top3_pc_variance, use.names = TRUE)
print(head(top3_pc_variance_dt, 20))

View(top3_pc_variance_dt)


#############

top3_pc_variance_dt[, PC_Rank := sequence(.N), by = Station]

ggplot(top3_pc_variance_dt, aes(x = factor(PC_Rank), y = VarianceExplained)) +
  geom_boxplot(fill = "#3182bd", alpha = 0.6) +
  labs(
    title = "Distribution of Variance Explained by Top 1–3 Selected PCs",
    x = "PC Rank (1 = Most Important)",
    y = "Variance Explained"
  ) +
  theme_minimal()





###########

#Number of grid points related to the station

# Count the number of NADA grid points used per station (cor > 0.4)
grid_count_per_station <- apply(corMat, 1, function(cor_vec) sum(cor_vec > 0.4, na.rm = TRUE))

# Create a summary table
grid_count_summary <- data.table(
  Station_ID = names(grid_count_per_station),
  Num_Grids_Used = grid_count_per_station
)

# Calculate and print the average number of grid points used
avg_grids_used <- mean(grid_count_per_station)
cat("Average number of NADA grid points used per station:", avg_grids_used, "\n")

# Optionally plot the distribution
ggplot(grid_count_summary, aes(x = Num_Grids_Used)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Number of NADA Grid Points Used per Station",
       x = "Grid Points (correlation > 0.4)",
       y = "Number of Stations") +
  theme_minimal()

#####




ggplot(grid_count_dt[order(Num_Grids_Used)], aes(x = Num_Grids_Used, y = reorder(station_name, Num_Grids_Used))) +
  geom_point(color = "#08519c", size = 3) +
  labs(title = "Grid Points Used per Station (cor > 0.4)",
       x = "Number of Grid Points",
       y = "Station Name") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 9))



#####


# Count number of correlated NADA grid points per station
grid_count_per_station <- apply(corMat, 1, function(cor_vec) sum(cor_vec > 0.4, na.rm = TRUE))

# Create data.table with station metadata
grid_count_table <- data.table(
  ID = names(grid_count_per_station),
  Num_Grids_Used = grid_count_per_station
)

# Merge with metadata to get station names
grid_count_table <- merge(grid_count_table, instQmeta[, .(ID, station_name = name)], by = "ID", all.x = TRUE)

# Reorder for clarity
setorder(grid_count_table, -Num_Grids_Used)

# Print table
print(grid_count_table)
View(grid_count_table)




### Top 10 OCA ####

# Extract top 10 eigenvalues and variance explained (proportion of variance) per station
top10_pca_summary <- lapply(names(pca_results), function(station_id) {
  pca <- pca_results[[station_id]]
  if (!is.null(pca)) {
    eigvals <- pca$sdev^2
    total_var <- sum(eigvals)
    prop_var <- eigvals / total_var
    data.table(
      Station = station_id,
      PC = paste0("PC", 1:min(10, length(eigvals))),
      Eigenvalue = eigvals[1:min(10, length(eigvals))],
      VarianceExplained = prop_var[1:min(10, length(prop_var))]
    )
  } else {
    NULL
  }
})

# Combine into one data.table
top10_pca_summary_dt <- rbindlist(top10_pca_summary, use.names = TRUE, fill = TRUE)

# Print the first few rows
print(head(top10_pca_summary_dt, 20))

# Optional: Save to file
#fwrite(top10_pca_summary_dt, "/work/bmcdaniel/R_stuff/top10_eigenvalues_variance.csv")

View(top10_pca_summary_dt)




#

# Try one station manually to see if PCA exists and is populated
pca_results[['06934500']]  # or use names(pca_results)[[1]]




# Filter to PC1–PC3
top3_only <- top3_pc_variance_dt[PC %in% c("PC1", "PC2", "PC3")]

# Compute mean variance explained per PC across stations
avg_var_by_pc <- top3_only[, .(
  MeanVariance = mean(VarianceExplained, na.rm = TRUE),
  MedianVariance = median(VarianceExplained, na.rm = TRUE)
), by = PC]

print(avg_var_by_pc)







# Filter to PC1–PC3
top5_only <- top3_pc_variance_dt[PC %in% c("PC1", "PC2", "PC3", 'PC4','PC5',"PC6", "PC7", "PC8", 'PC9','PC10')]

# Compute mean variance explained per PC across stations
avg_var_by_pc <- top5_only[, .(
  MeanVariance = mean(VarianceExplained, na.rm = TRUE),
  MedianVariance = median(VarianceExplained, na.rm = TRUE)
), by = PC]

print(avg_var_by_pc)

#PC1	0.25029	0.2385	1
#PC2	0.14870	0.1487	2
#PC3	0.08553	0.0828	3
#PC4	0.06350	0.0635	4
#PC5	0.06000	0.0600	5
#PC6	0.04290	0.0429	6
#PC7	0.03820	0.0382	7
#PC8	0.02750	0.0275	8
#PC9	0.02870	0.0287	9
#PC10	0.02273	0.0223	10

# Ensure `ivs` is your list of selected PCs by station
# Calculate number of PCs selected at each station
num_pcs_per_station <- sapply(ivs, function(x) {
  if (!is.null(x)) length(x) else 0
})

# Filter to non-zero stations
num_pcs_per_station <- num_pcs_per_station[num_pcs_per_station > 0]

# Calculate summary statistics
avg_pcs <- mean(num_pcs_per_station)
median_pcs <- median(num_pcs_per_station)
min_pcs <- min(num_pcs_per_station)
max_pcs <- max(num_pcs_per_station)

# Print results
cat("Summary of PCs selected per station:\n")
cat("Average:", round(avg_pcs, 2), "\n")
cat("Median :", median_pcs, "\n")
cat("Min    :", min_pcs, "\n")
cat("Max    :", max_pcs, "\n")

#####
# Flatten all selected PCs into a single vector
all_selected_pcs <- unlist(ivs)

# Count frequency of each selected PC label
pc_freq <- sort(table(all_selected_pcs), decreasing = TRUE)

# View
print(pc_freq)


