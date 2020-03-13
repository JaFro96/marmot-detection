# Figures of bachelor thesis

# Fig. 1
plotRGB(stack_gra_step_1)

# Acquiring training data polygons created by QGIS and merge them and convert
setwd("~/Bachelorarbeit/Polygone")
trainData1 = read_sf("arable_cropland.shp")
trainData2 = read_sf("abandoned_cropland.shp")
trainData3 = read_sf("grazed_steppe.shp")
trainData4 = read_sf("ungrazed_steppe.shp")
setwd("~/Bachelorarbeit")

# Set new column values to view appropriate colours
myColours = c( "yellow","black")
# set transparency
myColoursAlpha <- add.alpha(myColours, alpha=0.3)
# black indicating no burrow
trainData1$Colour=myColoursAlpha[2]
# yello indicating burrow
trainData1$Colour[trainData1$id==1]=myColoursAlpha[1]

# Visualizing these polygons for bachelor thesis
# Arable cropland fig. 2
plotRGB(stack_arab_crop_1)
td_arc = st_transform(trainData1, crs(stack_arab_crop_1))
plot(trainData1, add = TRUE,col=trainData1$Colour) 

# Abandoned cropland fig. 3
plotRGB(stack_aban_crop_1)
td_abc = st_transform(trainData2, crs(stack_aban_crop_1))
plot(td_abc,  add = TRUE,col=trainData1$Colour) 

# Grazed steppe fig. 4
plotRGB(stack_gra_step_1)
td_gs = st_transform(trainData3, crs(stack_gra_step_1))
plot(td_gs, add = TRUE, col = trainData1$Colour)

# Ungrazed steppe fig. 5
plotRGB(stack_un_step_1)
td_us = st_transform(trainData4, crs(stack_un_step_1))
plot(td_us, add = TRUE,col=trainData1$Colour)

# Fig. 6
spplot(list_gV[[1]]$Ar)

# Fig. 7
spplot(list_masks[[1]], colorkey = FALSE)

# Fig. 8 created by QGIS (QGIS Development Team, 2016)

# Fig. 9
plotRGB(ldf[[6]])

# Fig. 10
confuse(6, TRUE)

# Fig. 11
plot(varImp(model_1, scale = FALSE))

# Fig. 12
plot(varImp(model_2, scale = FALSE))

# Fig. 13
dev.off()
boxplot(result_b$AUC1,result_b$AUC2, names=c("Vorhersage 1","Vorhersage 2"), ylab = "AUC")

# Fig. 14
dev.off()
makePlotSD(result_b)

# Fig. 15
makePlotB(result_b)

# Fig. 16
plotRGB(ldf[[26]])

# Fig. 17
confuse(26, TRUE)

# Fig. 18
plotRGB(ldf[[44]])

# Fig. 19
confuse(44, TRUE)

# Fig. 20
plotRGB(ldf[[18]])

# Mean AUC
mean(result_b$AUC1)  
mean(result_b$AUC2)  