library(rgeos)
library(sp)
library(rgdal)
library(data.table)

system.time(
  G97_TW_U0200_2015 <- readOGR("data/G97_TW_U0200_2015/臺灣_最小統計區_面.shp",
                               # encoding = "UTF-8",
                               stringsAsFactors = FALSE)
)
#    user  system elapsed 
# 195.428   2.988 194.252

G97_TW_U0200_2015@data %>% setDT
proj4string(G97_TW_U0200_2015) <- sp::CRS("+init=epsg:3826") # TM2 (TWD97 中央經線121度)

system.time(
  G97_TW_U0200_2015_WGS84 <- spTransform(G97_TW_U0200_2015, CRS("+proj=longlat +datum=WGS84"))
)
save(G97_TW_U0200_2015_WGS84, file = "data/G97_TW_U0200_2015_WGS84.RData")

