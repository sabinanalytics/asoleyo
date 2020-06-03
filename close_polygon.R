#
library(tidyverse)
library(rgdal)



temp <- readOGR(dsn = "sample_2.dxf", layer ="entities", driver = "DXF")
ogrListLayers("sample_1.dxf")
plot(temp, xlim = c(tem))
axis(1)
axis(2)
box()


fortify_dxf <- temp %>% 
  fortify() %>% 
  as_tibble()

fortify_dxf <- fortify_dxf %>% 
  group_by(id) %>% 
  mutate(length = sqrt((long - lead(long))^2 + (lat - lead(lat))^2 )   ) %>% 
  ungroup()

### force last value to be equat to first
## delete any row with length 0 and then reorder
fortify_dxf_new <- fortify_dxf %>% 
  group_by(id) %>% 
  mutate(long = if_else(order == max(order), long[min(order)], long),
         lat = if_else(order == max(order), lat[min(order)], lat),
         length = sqrt((long - lead(long))^2 + (lat - lead(lat))^2 ) 
  ) %>% 
  filter(length != 0 | is.na(length)) %>% 
  ungroup() %>% 
  select(-length) %>% 
  as.data.frame()
   
SpatialLines(fortify_dxf_new)
SpatialLinesDataFrame(fortify_dxf_new)



temp_data <- data.frame(temp$Layer, temp$PaperSpace, temp$SubClasses, temp$Linetype, temp$EntityHandle, temp$Text) %>% 
  rename_all(~gsub("temp.", "", .))
row.names(temp_data) <- c('0','1')

list(
     fortify_dxf_new %>% filter(id == 0) %>% select(long,lat) %>% Line() %>% list() %>% Lines(ID = '0'), 
     fortify_dxf_new %>% filter(id == 1) %>% select(long,lat) %>% Line()  %>% list() %>% Lines(ID = '1')
     ) %>% 
  SpatialLines() %>% 
  SpatialLinesDataFrame(., temp_data) %>% 
  writeOGR(obj = ., dsn = "sample_2_new.dxf", layer = '', driver = "DXF")


# L1 = Line(cbind(rnorm(5),rnorm(5)))
# L2 = Line(cbind(rnorm(5),rnorm(5)))
# L3 = Line(cbind(rnorm(5),rnorm(5)))
# L1
# 
# Ls1 = Lines(list(L1),ID="a")
# Ls2 = Lines(list(L2,L3),ID="b")
# Ls2
# 
# SL12 = SpatialLines(list(Ls1,Ls2))
# plot(SL12)
# 
# SLDF = SpatialLinesDataFrame(
#   SL12,
#   data.frame(
#     Z=c("road","river"),
#     row.names=c("a","b")
#   ))
# str(SLDF)

