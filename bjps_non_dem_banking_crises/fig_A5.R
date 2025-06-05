# Figure A5 - Map of countries with DC scheme

# Map of DC_Any ####

# Load world map
world <- ne_countries(scale = "medium", type = 'map_units', returnclass = "sf")

# Merge your data with the world map data
data_dc_any<-bmr_data %>% 
  dplyr::select(country,year,DC_Mand_Revise2,index_canon_no) %>% 
  filter(DC_Mand_Revise2==1) %>% 
  group_by(country) %>% 
  summarise(DC_Mand_Revise2 = mean(DC_Mand_Revise2), index_canon_no = mean(index_canon_no),year = paste(year,collapse=",\n")) %>% 
  mutate(country = ifelse(country=="Eswatini", "eSwatini",country),
         year = gsub(", ","\n",year)) 

world_data <- world %>% left_join(data_dc_any, by = c("name" = "country"))

world_data<-world_data %>% filter(!grepl("Antar",name)) %>% 
  filter(as.numeric(label_x)<173)

#brewer
colors <- brewer.pal(n = 9, name = "YlOrRd")

# Plot
world_data<-world_data %>% 
  arrange(DC_Mand_Revise2,name) 

world_data$name %>% head(n=15)
# [1] "Chile"      "Ghana"      "Indonesia"  "Kazakhstan" "Kenya"      "Malaysia"   "Nepal"      "Nigeria"    "Peru"      
# [10] "Russia"     "Singapore"  "Sri Lanka"  "Uganda"     "Zambia"     "eSwatini"  
# 

world_data$nudgex<- c(-500000,-250000,-1800000,0,550000,1300000,300000,-300000,-550000,
                      0,300000,0,-600000,-700000,200000, rep(0, nrow(world_data)-15))

world_data$nudgey<- c(-500000,-500000,-600000,0,-630000,800000,250000,-800000,-200000,
                      500000,500000,-450000,-10000, 0,-300000,rep(0, nrow(world_data)-15))

ggplot(data = world_data) +
  geom_sf(aes(fill = ifelse(DC_Mand_Revise2 == 1, index_canon_no, "grey")), color = "white") +
  scale_fill_gradientn(colours = colors, na.value = "grey", guide = "colorbar") +
  labs(fill = "Bailout Policy\nIndex") +
  theme_void() + 
  theme(legend.position = "right")+
  coord_sf(crs = st_crs("+proj=robin"),ylim = c())+
  theme(plot.background = element_rect(fill = "white"),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8))+
  geom_sf_text(aes(label=as.character(year)), 
               size = 2.5, # Adjust size as needed
               check_overlap = F, 
               nudge_x = world_data$nudgex,
               nudge_y = world_data$nudgey,
               hjust = 'centre', vjust = 'centre') # Adjust horizontal and vertical justification
ggsave(paste0(FIGURE_PATH,"/fig_A5.png"), width = 12, height = 7)  