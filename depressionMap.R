# Clear Enivronment
rm(list=ls())

# Load Libraries
library(animation)
library(data.table)
library(dplyr)
library(gganimate)
library(ggplot2)
library(gridExtra)
library(magrittr)

# Set working directory
setwd('H:/TOOLS/R/Mapping Examples/BRFSS Trends')

# Import BRFSS Data
csv.name <- 'Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present_.csv'
df <- data.table(fread(file.path(getwd(), 'DATA', csv.name)), stringsAsFactors = F)

# Limit to Depression Answers only
clean.df <- df %>%
    .[Class == 'Chronic Health Indicators' & 
                 Topic == 'Depression' &
                 Response == 'Yes' &
                 Break_Out == 'Overall'
                 # Year == 2017 & Locationabbr == 'OH'
                 ,] %>%
    .[, State := Locationabbr] %>%
    .[, Confidence_Limit_Spread := Confidence_limit_High - Confidence_limit_Low]

# Get yearly Us average spread of the confidence limit    
confLimit.df <- clean.df %>%
    .[, .(Mean_Confidence_Limit_Spread = mean(Confidence_Limit_Spread, na.rm = T)), by = list(Year)]

# Import Map Data
load('DATA/map_data/us50')

# Join Map Data to Depression Data
map_obs <- left_join(us50, clean.df, by = c('id' = 'State')) %>%
    left_join(confLimit.df, by = c('Year'))


# Build Plot of Confidence Limit Spread
confSpreadPlot <- ggplot(map_obs)+
    geom_bar(aes(x = factor(Year), y = Mean_Confidence_Limit_Spread),
             stat = 'identity', fill = '#A3307EFF')+
    geom_text(aes(x = factor(Year), y = Mean_Confidence_Limit_Spread, 
                  label = round(Mean_Confidence_Limit_Spread, 2)),
              col = 'white', vjust = 2, fontface = 'bold')+
    scale_x_discrete(limits = c('2011','2012','2013','2014','2015','2016','2017'), 
                     labels = c('2011','2012','2013','2014','2015','2016','2017'), 
                     drop = FALSE)+
    scale_y_continuous(limits = c(0, 3))+
    labs(title = 'Mean Confidence Limit Spread', x = '', y = '')+
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "none")
# print(confSpreadPlot)

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
    geom_map(map=map_obs, aes(x=long, y=lat, map_id=id, group=group, fill = Data_value), 
             color="darkgrey", size=0.1)+
    scale_fill_viridis_c(option = "magma", direction = -1)+
    labs(title = 'Depression Rates by State ({closest_state})',
         subtitle = 'Data from www.cdc.gov/brfss',
         x = '', y = '',
         fill = 'Depression %')+
    theme_map()+
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(size = 14))
# print(myMap)

### GIF:Take 1
# Animate Map
gifMap <- myMap + transition_states(Year)
anim_save(file.path(getwd(), 'FIGURES/depressionMap.gif'), gifMap, width = 750, height = 450)
rm(gifMap)

### GIF: Take 2
# Inset barchart onto map and animate using SaveGIF({})
build_GIF <- function(path = "H:/TOOLS/R/Mapping Examples/BRFSS Trends/FIGURES/depressionMap(2).gif", 
                      width = 750, height = 450) {
    saveGIF({
        ani.options(interval = 1.5) # Set speed (smaller number is faster)
        for (i in 2011:2017) {
            # build barchart
            bchart <- ggplot(confLimit.df[Year <= i, ])+
                geom_bar(aes(x = factor(Year), y = Mean_Confidence_Limit_Spread),
                         stat = 'identity', fill = '#C83E73FF')+
                geom_text(aes(x = factor(Year), y = Mean_Confidence_Limit_Spread, 
                              label = round(Mean_Confidence_Limit_Spread, 2),
                              vjust = 1),
                          col = 'white', fontface = 'bold')+
                scale_x_discrete(limits = as.character(2011:2017), 
                                 labels = as.character(2011:2017), 
                                 drop = FALSE)+
                scale_y_continuous(limits = c(0, 3))+
                labs(title = 'Mean Confidence Limit Spread', x = '', y = '')+
                theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
                      panel.background = element_rect(fill = "#f5f5f2", color = NA),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      legend.position = "none")
            # build map
            map <- ggplot(data=map_obs %>% filter(Year == i))+
                geom_map(map=map_obs, aes(x=long, y=lat, map_id=id, group=group, fill = Data_value), 
                         color="darkgrey", size=0.1)+
                scale_fill_viridis_c(option = "magma", direction = -1, 
                                     limits = range(map_obs$Data_value))+
                labs(title = paste0('Depression Rates by State (',i,')'),
                     subtitle = 'Data from www.cdc.gov/brfss',
                     x = '', y = '',
                     fill = 'Depression %')+
                theme_map()+
                theme(plot.title = element_text(size = 20),
                      plot.subtitle = element_text(size = 14),
                      legend.title = element_text(size = 14))
            # add barchart to map
            p <- map + annotation_custom(ggplotGrob(bchart), 
                                      xmin = 400000, xmax = 2750000, 
                                      ymin = -2700000, ymax = -2000000)
            print(p)
        }
    }, movie.name = path, img.name = "plot", ani.height = height, ani.width = width)
}
build_GIF()


### GIF Part 2
library(magick)
datalist <- split(map_obs, map_obs$Year)
rm(img)
img <- image_graph(1200, 750, res = 96)
out <- lapply(datalist, function(data){
    bchart <- ggplot(data=confLimit.df %>% filter(Year <= unique(data$Year)))+
        geom_bar(aes(x = factor(Year), y = Mean_Confidence_Limit_Spread),
                 stat = 'identity', fill = '#C83E73FF')+
        geom_text(aes(x = factor(Year), y = Mean_Confidence_Limit_Spread, 
                      label = round(Mean_Confidence_Limit_Spread, 2),
                      vjust = 1),
                  col = 'white', fontface = 'bold')+
        scale_x_discrete(limits = as.character(2011:2017), 
                         labels = as.character(2011:2017), 
                         drop = FALSE)+
        scale_y_continuous(limits = c(0, 3))+
        labs(title = 'Mean Confidence Limit Spread', x = '', y = '')+
        theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
              panel.background = element_rect(fill = "#f5f5f2", color = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              legend.position = "none")
    # build map
    map <- ggplot(data)+
        geom_map(map=data, aes(x=long, y=lat, map_id=id, group=group, fill = Data_value), 
                 color="darkgrey", size=0.1)+
        scale_fill_viridis_c(option = "magma", direction = -1, 
                             limits = range(map_obs$Data_value))+
        labs(title = paste0('Depression Rates by State (',data$Year,')'),
             subtitle = 'Data from www.cdc.gov/brfss',
             x = '', y = '',
             fill = 'Depression %')+
        theme_map()+
        theme(plot.title = element_text(size = 20),
              plot.subtitle = element_text(size = 14),
              legend.title = element_text(size = 14))
    # add barchart to map
    p <- map + annotation_custom(ggplotGrob(bchart), 
                                 xmin = 400000, xmax = 2750000, 
                                 ymin = -2700000, ymax = -2000000)
    print(p)
})
# dev.off()
animation <- image_animate(img, fps = 0.5)
print(animation)
image_write(animation, "H:/TOOLS/R/Mapping Examples/BRFSS Trends/FIGURES/depressionMap(3).gif")

