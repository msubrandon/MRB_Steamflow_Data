#Maps


#This is a map of the entire plot w/contours
ggplot(nadaxy) +
  geom_point(aes(lon, lat, color = year), size = 0.1) +
  geom_contour(aes(lon, lat, z = year)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))



### GOOD ONE 
#Same without Gridpoints
ggplot(nadaxy) +
  geom_point(aes(lon, lat, color = year), size = .9)+
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .9))


