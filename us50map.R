# Clear Environment
rm(list=ls())

# Load Libraries
library(magrittr)
library(maptools)
library(rgdal)

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

# Read in map data
dsn <- "DATA/map_data"
layer <- "cb_2014_us_state_5m"
us = rgdal::readOGR(path.expand(dsn), layer)

# Save map data
save(us, file = "DATA/map_data/us")

# Transform geographical coordinates to Lambert Azimuth Equal Area projection
us_aea = spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id = rownames(us_aea@data)

# Move Alaska (scaled down) and Hawaii
alaska = us_aea[us_aea$STATEFP=="02",]
alaska = maptools::elide(alaska, rotate=-50)
alaska = maptools::elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska = maptools::elide(alaska, shift=c(-2300000, -2500000))
proj4string(alaska) = proj4string(us_aea)

hawaii = us_aea[us_aea$STATEFP=="15",]
hawaii = maptools::elide(hawaii, rotate=-35)
hawaii = maptools::elide(hawaii, shift=c(5200000, -1200000))
proj4string(hawaii) = proj4string(us_aea)

# Remove Alaska and Hawaii from base map and substitute transformed versions
us_aea2 = us_aea[!us_aea$STATEFP %in% c("02", "15"),]
us_aea3 = rbind(us_aea2, alaska, hawaii)

# Prepare a data frame for plotting and optionally save
us50 <- fortify(us_aea3, region="STUSPS")
us50 %<>% filter(!(id %in% c('AS','MP','GU','PR','VI')))

# Plot the map
p <- ggplot(data=us50)+
    geom_map(map=us50, aes(x=long, y=lat, map_id=id, group=group), fill="white", color="dark grey", size=0.15)+
    theme_map()
p

save(us50, file = 'DATA/map_data/us50')
