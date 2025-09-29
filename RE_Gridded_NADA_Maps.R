#Map 

ggplot(nadaxy) +
  geom_point(aes(lon, lat, color = year), size = .9)+
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .9))


