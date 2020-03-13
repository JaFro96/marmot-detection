# 1

# Create ggmaps
# landuse no 1 arable cropland NUMBUR = 55 
loc_arab_crop_1 <- c(lon = 65.86244, lat = 51.76227) # UTM 41
map_arab_crop_1 <- get_map(location = loc_arab_crop_1, zoom = 16, maptype="satellite")

# landuse no 2 abandoned cropland
loc_aban_crop_1 <- c(lon = 61.09529, lat = 51.69636) # UTM 41 NUMBUR = 32  
map_aban_crop_1 <- get_map(location = loc_aban_crop_1, zoom = 16, maptype="satellite")

# landuse no 3 grazed steppe NUMBUR 63
loc_gra_step_1 <- c(lon = 68.71,  lat = 50.965) # UTM 42
map_gra_step_1 <- get_map(location = loc_gra_step_1, zoom = 16, maptype="satellite")

# landuse no 4 moderately or ungrazed steppe # UTM 42
# NUMBUR = 37
loc_un_step_1 <- c(lon = 68.67678, lat = 50.535323)
map_un_step_1 <- get_map(location = loc_un_step_1, zoom = 16, maptype="satellite")

# Create raster stacks
# Pre-processing
stack_gra_step_1 <- my_preProcess(map_gra_step_1)
stack_un_step_1   <- my_preProcess(map_un_step_1)
stack_aban_crop_1 <- my_preProcess(map_aban_crop_1)
stack_arab_crop_1 <- my_preProcess(map_arab_crop_1)
