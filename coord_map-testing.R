library(rgdal)
library(tidyverse)
library(plyr)
library(mapproj)

#.....................load data......................................................................................
#--------------------------------------------------------------------------------------------------------------------
eumap <- readOGR(dsn = "D:\\R\\seismicitymap", layer ="NUTS_RG_01M_2016_4326_LEVL_1" )

?coord_map

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
  labs(title = "cylequalarea 55", x=NULL, y=NULL) +
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
  labs(title = "rectangular 55", x=NULL, y=NULL) +
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
  labs(title = "gall 55", x=NULL, y=NULL) +
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

#test_map_12 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
#  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
#  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
#                     sec.axis = dup_axis()) +
#  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
#                     sec.axis = dup_axis()) + 
#  labs(title = "perspective", x=NULL, y=NULL) +
#  theme_bw() + coord_map("perspective", dist=0) +
#  guides(fill = FALSE) +
#  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
#        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
#        axis.ticks = element_blank())
#test_map_12

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
  labs(title = "stereographic", x=NULL, y=NULL) +
  theme_bw() + coord_map("stereographic") +
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
  labs(title = "laue", x=NULL, y=NULL) +
  theme_bw() + coord_map("laue") +
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
  labs(title = "fisheye 2", x=NULL, y=NULL) +
  theme_bw() + coord_map("fisheye", 2) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_16

test_map_17 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "newyorker 0", x=NULL, y=NULL) +
  theme_bw() + coord_map("newyorker", 0) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_17

test_map_18 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "conic 55", x=NULL, y=NULL) +
  theme_bw() + coord_map("conic", lat0=55) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_18

test_map_19 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "simpleconic, 50, 60", x=NULL, y=NULL) +
  theme_bw() + coord_map("simpleconic", lat0=50, lat1=60) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_19

test_map_20 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "lambert, 50, 60", x=NULL, y=NULL) +
  theme_bw() + coord_map("lambert", lat0=50, lat1=60) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_20

test_map_21 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "albers, 50, 60", x=NULL, y=NULL) +
  theme_bw() + coord_map("albers", lat0=50, lat1=60) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_21

test_map_22 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "bonne, 55", x=NULL, y=NULL) +
  theme_bw() + coord_map("bonne", lat0=55) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_22

test_map_23 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "polyconic", x=NULL, y=NULL) +
  theme_bw() + coord_map("polyconic") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_23

test_map_24 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "aitoff", x=NULL, y=NULL) +
  theme_bw() + coord_map("aitoff") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_24

test_map_25 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "lagrange", x=NULL, y=NULL) +
  theme_bw() + coord_map("lagrange") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_25

test_map_26 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "bicentric, long -2", x=NULL, y=NULL) +
  theme_bw() + coord_map("bicentric", lon0=-2) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_26

test_map_27 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "elliptic, long -2", x=NULL, y=NULL) +
  theme_bw() + coord_map("elliptic", lon0=-2) +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_27

test_map_28 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "globular", x=NULL, y=NULL) +
  theme_bw() + coord_map("globular") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_28

test_map_29 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "vandergrinten", x=NULL, y=NULL) +
  theme_bw() + coord_map("vandergrinten") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_29

test_map_30 <- ggplot() + geom_polygon(data=nwmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), alpha=0, colour="#dddddd") +
  scale_y_continuous(expand=c(0,0), breaks = seq(50,61,1), 
                     sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-9,3,1),
                     sec.axis = dup_axis()) + 
  labs(title = "eisenlohr", x=NULL, y=NULL) +
  theme_bw() + coord_map("eisenlohr") +
  guides(fill = FALSE) +
  theme(panel.grid = element_line(colour = "#cccccc", size = .1),
        panel.grid.minor = element_line(colour = "#dddddd", size = .05),
        axis.ticks = element_blank())
test_map_30


#gall lagrange

ggsave(filename = "test_map_01.png", plot = test_map_01, device = "png", width = 7, height = 7, units = "in")


#....................code testing....................................................................................
#--------------------------------------------------------------------------------------------------------------------
?coord_map
?cylequalarea
?ggtitle
?labs