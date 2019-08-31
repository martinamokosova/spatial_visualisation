library(rgdal)
library(tidyverse)
library(plyr)
library(mapproj)

#.....................load data......................................................................................
#--------------------------------------------------------------------------------------------------------------------
eumap <- readOGR(dsn = "D:\\R\\seismicitymap", layer ="NUTS_RG_01M_2016_4326_LEVL_1" )


#..................transform data....................................................................................
#--------------------------------------------------------------------------------------------------------------------
eumap@data$id <- rownames(eumap@data)
eumap_df <- fortify(eumap)
eumap_df <- join(eumap_df, eumap@data, by="id")
northwestmap <- eumap_df[eumap_df$long > -10 & eumap_df$long < 4 & eumap_df$lat > 49.4 & eumap_df$lat < 62, ]
ukmap <- northwestmap[northwestmap$CNTR_CODE == "UK", ]
nwmap <- northwestmap[northwestmap$CNTR_CODE == "IE" | northwestmap$CNTR_CODE == "FR", ]



#................create basic map....................................................................................
#--------------------------------------------------------------------------------------------------------------------


test_map_01 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "mercator", x=NULL, y=NULL) +
  theme_bw() + coord_map("mercator") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_01

test_map_02 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "sinusoidal", x=NULL, y=NULL) +
  theme_bw() + coord_map("sinusoidal") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_02

test_map_03 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "cylequalarea", x=NULL, y=NULL) +
  theme_bw() + coord_map("cylequalarea", lat0=55) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_03

test_map_04 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "cylindrical", x=NULL, y=NULL) +
  theme_bw() + coord_map("cylindrical") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_04

test_map_05 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "rectangular", x=NULL, y=NULL) +
  theme_bw() + coord_map("rectangular", lat0=55) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_05

test_map_06 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "gall", x=NULL, y=NULL) +
  theme_bw() + coord_map("gall", lat0=55) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_06

test_map_07 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "mollweide", x=NULL, y=NULL) +
  theme_bw() + coord_map("mollweide") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_07

test_map_08 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "gilbert", x=NULL, y=NULL) +
  theme_bw() + coord_map("gilbert") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_08

test_map_09 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "azequidistant", x=NULL, y=NULL) +
  theme_bw() + coord_map("azequidistant") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_09

test_map_10 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "azequalarea", x=NULL, y=NULL) +
  theme_bw() + coord_map("azequalarea") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_10

test_map_11 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "gnomonic", x=NULL, y=NULL) +
  theme_bw() + coord_map("gnomonic") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_11

test_map_12 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "perspective", x=NULL, y=NULL) +
  theme_bw() + coord_map("perspective") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_12

test_map_13 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "orthographic", x=NULL, y=NULL) +
  theme_bw() + coord_map("orthographic") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_13

test_map_14 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "mercator", x=NULL, y=NULL) +
  theme_bw() + coord_map("mercator") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_14

test_map_15 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "mercator", x=NULL, y=NULL) +
  theme_bw() + coord_map("mercator") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_15

test_map_16 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "mercator", x=NULL, y=NULL) +
  theme_bw() + coord_map("mercator") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_16


ggsave(filename = "test_map_01.png", plot = test_map_01, device = "png", width = 7, height = 7, units = "in")


#....................code testing....................................................................................
#--------------------------------------------------------------------------------------------------------------------
?coord_map
?cylequalarea
?ggtitle
?labs