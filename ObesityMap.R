# Clear Environment
rm(list=ls())

# Load Libraries
library(animation)
library(dplyr)
library(viridis)
library(gganimate)
library(transformr)

# Set working directory
setwd('H:/TOOLS/R/Mapping Examples/BRFSS Trends')

# Import Obesity Data
obs <- NULL
for(y in 2011:2017) {
    imp <- read.csv(paste0('DATA/bmi_data/BRFSS BMI-',y,'.csv'), stringsAsFactors = FALSE)
    obs <- rbind(obs, imp)
}

clean_obs <- obs %>%
    filter(!(LocationAbbr %in% c('US','UW'))) %>%
    filter(grepl('Obese', Response)) %>%
    dplyr::select(Year, LocationAbbr, Response, Data_Value) %>%
    rename(STATE = LocationAbbr, Value = Data_Value) %>%
    mutate(Value = as.numeric(Value))

# Import Map Data
load('DATA/map_data/us50')

# Join map data to obesity data
map_obs <- left_join(us50, clean_obs, by = c('id' = 'STATE'))

# Create custom theme for map
theme_map <- function(...) {
    theme_minimal() +
        theme(
            text = element_text(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            # panel.grid.minor = element_blank(),
            panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
            panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
            plot.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.background = element_rect(fill = "#f5f5f2", color = NA),
            legend.background = element_rect(fill = "#f5f5f2", color = NA),
            panel.border = element_blank(),
            ...
        )
}

# Build Map
myMap <- ggplot(data=map_obs)+
    geom_map(map=map_obs, aes(x=long, y=lat, map_id=id, group=group, fill = Value), 
             color="white", size=0.1)+
    scale_fill_viridis(option = "magma", direction = -1)+
    labs(title = 'Obesity Rates by State ({closest_state})',
         subtitle = 'Data from www.cdc.gov/brfss',
         x = NULL, y = NULL,
         fill = 'Obesity %')+
    theme_map()+
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(size = 14))
# myMap

# Animate Map
gifMap <- myMap + transition_states(Year, transition_length = 7, state_length = 7)
anim_save(file.path(getwd(), 'FIGURES/myMap.gif'), gifMap, width = 750, height = 450)

