# 2

# Set the working directory
setwd("~/Bachelorarbeit")

# Prepare traindata using shapefiles and related satellite image
# arable cropland
prep_train_dat(stack_arab_crop_1,"Polygone/arable_cropland.shp","trainDat.RData")
# abandoned cropland
prep_train_dat(stack_aban_crop_1,"Polygone/abandoned_cropland.shp","trainDat.RData")
# grazed steppe 
prep_train_dat(stack_gra_step_1,"Polygone/grazed_steppe.shp","trainDat.RData")
# ungrazed steppe
prep_train_dat(stack_un_step_1,"Polygone/ungrazed_steppe.shp","trainDat.RData")

# Supervised classification
model_1 <- sup_class("trainDat.RData")
save(model_1, file="Model_1.RData")
