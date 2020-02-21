install.packages(c("odbc","rgdal", "rgeos", "plyr", "tidyverse","maptools"))

library(odbc)           # database connection
library(rgdal)          # to read shapefile in R
library(rgeos)          # to find polygon centroids - gCentroid() function
library(plyr)           # to join the data frames
library(tidyverse)      # includes stringr and ggplot packages
library(maptools)       # needed for SpatialPolygons functions

# I also tried other way of connecting to server which worked better for me
con <- dbConnect(odbc::odbc(),
                  Driver = "SQL Server",
                  Server = "rif_dbprod",
                  Database = "MOKOSM",
                  Trusted_Connection = TRUE)


test <- dbGetQuery(con,"SELECT top 10 * FROM [MOKOSM].[dbo].[NTISv10_road_selection]")

Road_Sections <- dbGetQuery(con,"SELECT * FROM [MOKOSM].[dbo].[NTISv10_road_selection_aggr_by_section_1month]")

Road_Sections <- Road_Sections[order(Road_Sections$Technology, Road_Sections$Section),]
Road_Sections$TechSec <- paste(Road_Sections$Technology, Road_Sections$Section, sep = "_")
View(Road_Sections)

#Road_Sections_MAIN <- dbGetQuery(con,"SELECT * FROM [MOKOSM].[dbo].[NTISv10_road_selection_aggr_by_section_MAIN]")






#........map data transformation
#..................................................................................................................
eumap <- readOGR(dsn = "\\\\rifgisuser\\mokosm\\smart_motorways_analysis\\My Analysis\\shp", layer ="NUTS_RG_03M_2016_4326_LEVL_1" )

View(eumap@data[eumap@data$CNTR_CODE == "UK",])


eumap@data$id <- case_when(
  eumap@data$CNTR_CODE == "UK" & eumap@data$FID != "UKL" & eumap@data$FID != "UKM" & eumap@data$FID != "UKN" ~ "ENG",
  eumap@data$FID == "UKL" ~ "WLS",
  eumap@data$FID == "UKM" ~ "SCT",
  eumap@data$FID == "UKN" ~ "NIR",
  TRUE ~ as.character(eumap@data$CNTR_CODE)
)

eumap_dis2 <- gBuffer(eumap, byid=TRUE, width=0)
eumap_dis <- unionSpatialPolygons(SpP = eumap_dis2, IDs = eumap@data$id)

#...............lets turn the new data into a data.frame
#...............the join has tho be with the eumap, as that ones still has the id2 and all necessary data
#...............the id at the end has to be removed
eu_dis <- fortify(eumap_dis)
#eu_dis$id2 <- eu_dis$id
eu_dis <- join(eu_dis, eumap@data, by="id")
#eu_dis <- eu_dis[,c(1:13)]
View(eu_dis$id)

#map dataframes
nwmap <- eu_dis[eu_dis$long > -9 & eu_dis$long < 3 & eu_dis$lat > 49 & eu_dis$lat < 60
                & eu_dis$CNTR_CODE != "UK", ]
ukmap <- eu_dis[eu_dis$CNTR_CODE == "UK" & eu_dis$id != "ENG", ]
enmap <- eu_dis[eu_dis$id == "ENG", ]



#..........analysis of the VM data
#..................................................................................................................
#....Main Carriageway only
ggplot() + geom_histogram(data = Road_Sections, aes(x = SUM_b.VM, fill = Technology), binwidth = 200000) +
  theme_bw() +
  scale_fill_viridis_d(option = "inferno", begin = 0.2, end = 0.8)
ggplot() + geom_histogram(data = Road_Sections, aes(x = SUM_b.VM, fill = TechSec), binwidth = 200000) +
  theme_bw() +
  scale_fill_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  facet_wrap(~ROAD_NAME)






#..........the background map code
#..................................................................................................................
x_labels <- c("8°W", "6°W", "4°W", "2°W", "0°", "2°E")
y_labels <- c("49°N", "50°N","51°N","52°N","53°N","54°N","55°N","56°N","57°N","58°N","59°N","60°N")
background_map <- ggplot() + geom_polygon(data=ukmap, aes(x = long, y=lat, group=group), fill="#eeeeee") +
  geom_polygon(data=enmap, aes(x = long, y=lat, group=group, fill=NUTS_ID), colour="white") +
  coord_quickmap() +
  theme_bw() +
  scale_fill_grey(start = 0.8, end = 0.7) +
  guides(fill=FALSE) +
  scale_y_continuous(expand=c(0,0), breaks = seq(49,60,1), labels = y_labels, sec.axis = dup_axis()) +
  scale_x_continuous(expand=c(0,0), breaks = seq(-8,2,2), labels = x_labels, sec.axis = dup_axis()) +
  theme(panel.grid = element_line(colour = "#f2f2f2", size = .1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        rect = element_rect(fill = "transparent")) +
  xlab("") + ylab("")







#........................roadsections volume data graphs
#..................................................................................................................


ggplot() + geom_line(data = Road_Sections, aes(x = as.Date(DateTimePeriodID), y = SUM_b.VM/1000000, colour = TechSec)) + 
  facet_wrap(~ ROAD_NAME) +
  theme_bw() +
  scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.8) +
  guides(fill=FALSE) +
  theme(panel.grid = element_line(colour = "#f2f2f2", size = .1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        rect = element_rect(fill = "transparent")) +
  xlab("date") + ylab("volume (million VM)")


ggplot() + geom_line(data = Road_Sections[Road_Sections$ROAD_NAME == "M1", ], 
                     aes(x = as.Date(DateTimePeriodID), y = SUM_b.VM/1000000, 
                         colour = TechSec, linetype = TechSec),
                     size = 1) + 
  theme_bw() +
  scale_colour_viridis_d(option = "inferno", begin = 0.1, end = 0.9) +
  guides(fill=FALSE) +
  theme(panel.grid = element_line(colour = "#f2f2f2", size = .1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        rect = element_rect(fill = "transparent")) +
  xlab("date") + ylab("volume ( million VM)")


View(head(Road_Sections))
traceback()
