# Script containing lots of functions and defining libraries
# Set the working directory
setwd("~/Bachelorarbeit")

# -------------------------------------------------------------------- #
# Libraries
# -------------------------------------------------------------------- #

# Package for downloading google images
library(ggmap)
# to work with raster objects
library(raster)
# to upload shapefiles
library(sf)
# used for the classification
library(caret)
# for the validation of the classification (spatial)
library(CAST)
# Converting ggmap into raster stack
# @see: https://rdrr.io/github/poissonconsulting/poisspatial/man/ps_ggmap_to_raster.html
library(poisspatial)
# plot multiple spplots
library(gridExtra)
# for computation of geometry variables
library(Rsenal)
# Compute AUC
library(SDMTools)
# Interactive view of raster objects
library(mapview)
# for principal component analysis of a raster image
library(RStoolbox)
# for extracting tables
library(xlsx)

# -------------------------------------------------------------------- #
# Functions
# -------------------------------------------------------------------- #

# train the classification model
# @param x string pointing to a file with a dataframe containing training data
# @return model (for classification)
sup_class <- function(x){
  # load training data 
  dat <- get(load(x))
  # select predictors
  predictors_names <- c('PC1','sd3','sd5','sd7','sd9','sd11',
                        'mean3','mean5','mean7','mean9','mean11',
                        'sobelxsd','sobelysd','laplace','highpass')
  # select response
  outcome_name <- 'id'
  # for reproducibility
  set.seed(123)
  # split the data into test and training data
  selection <- createDataPartition(dat[,outcome_name], p = .75, list = FALSE, times = 1)
  trainDat <- dat[ selection,]
  testDat  <- dat[-selection,]
  # 10 fold spatial cross validation
  ind <- CreateSpacetimeFolds(trainDat,spacevar="ID",k=10)
  ctrl <- trainControl(method="cv",index=ind$index,savePredictions = TRUE)
  # train the model
  model <- train(trainDat[,predictors_names], 
                 trainDat[,outcome_name],
                 method = "rf", 
                 importance = TRUE,
                 trControl = ctrl, 
                 ntree = 500,
                 na.action = na.omit)
  return(model)
}

# train the second random forest classification model with geometry variables as predictors
# @param x string pointing to a dataframe containing training data
# @return model (for classification)
sup_class_rf2 <- function(x){
  dat <- x
  dat$distEdges = NULL
  dat = dat[!duplicated(dat),]
  # select predictors
  predictors_names <- names(list_gV[[1]])[-1]
  predictors_names <- predictors_names[predictors_names != "distEdges"] 
  # select response
  outcome_name <- 'id'
  # for a better comparison
  set.seed(123)
  # split the data into test and trainings data
  selection <- createDataPartition(dat[,outcome_name], p = .75, list = FALSE, times = 1)
  trainDat <- dat[ selection,]
  testDat  <- dat[-selection,]
  # spatial cross validation
  ind <- CreateSpacetimeFolds(trainDat,spacevar="ID",k=10)
  ctrl <- trainControl(method="cv",index=ind$index,savePredictions = TRUE)
  # train the model
  model <- train(trainDat[,predictors_names], trainDat[,outcome_name],
                 method = "rf", importance = TRUE,
                 trControl = ctrl, ntree = 500, na.action = na.omit)
  return(model)
}

# Function for preparing train data for the second random forest
# @param x RasterStack
# @param y sf or shapefile
# @param z dataframe containing traindata
rf2_prep_train_dat = function (x,y,z){
  x = dropLayer(x, "distEdges") 
  old = z
  if(typeof(y)=="S4"){
    sampleDat = y
  }
  else{
    sampleDat = read_sf(y)
  }
  rf2_extr <- extract(x, sampleDat, df = TRUE)
  rf2_extr = rf2_extr[!duplicated(rf2_extr),]
  rf2_extr = na.omit(rf2_extr)
  if(nrow(z)==0){
    sampleDat$PolyID <- 1:nrow(sampleDat)
  }
  else{
    sampleDat$PolyID <- max(z$ID) + 1:nrow(sampleDat)
    rf2_extr$ID = rf2_extr$ID+max(z$ID)
  }
  
  rf2_merged <- merge(rf2_extr, sampleDat, by.x = "ID", by.y="PolyID")
  rf2_merged$id <- as.factor(rf2_merged$id)
  rf2_merged$geometry = NULL
  new = rbind(old, rf2_merged) 
  return(new)
}

# Function for converting mask information into training data
# @param id integer identifier of mask no
# @return dataframe containing train data
mask_to_train_dat = function(id, tDat = NULL){
  # extract values for no burrow/marmot
  my_filename = sub("id",id, "trainD_RF2/gV_id_nomarmot.shp")
  train_no_marmot = rf2_prep_train_dat(list_gV[[id]],my_filename, tDat)
  
  # extract values for burrow/marmot
  my_shape = rasterToPolygons(list_masks[[id]],fun=function(x){x==1},dissolve=TRUE)
  names(my_shape) = "id"
  train_marmot = rf2_prep_train_dat(list_gV[[id]],my_shape, train_no_marmot)
  return(train_marmot)
}

# Validation of the model
# Compare the predictions with the observations
# @param x a model
# @return table of confusion
validate = function(x){
  cvPredictions <- x$pred[x$pred$mtry==x$finalModel$mtry,]
  # confusionMatrix(cvPredictions$pred,cvPredictions$obs)
  return(c(table(cvPredictions$pred,cvPredictions$obs),
           auc(cvPredictions$pred,cvPredictions$obs)))
}

# Add some predictor variables to a raster object
# @param x raster image containing a first principal component
# @return rasterStack with a minimum of 15 layers
focal_func <- function(x) {
  # edge detecting moving windows
  sobel_x <- matrix(c(-1,0,1,-2,0,2,-1,0,1),nrow = 3)
  sobel_y <- matrix(c(-1,-2,-1,0,0,0,1,2,1),nrow = 3)
  laplace <- matrix(c(0,1,0,1,-4,1,0,1,0),nrow = 3)
  high_pass <- matrix(c(-1,-1,-1,-1,16,-1,-1,-1,-1))
  if (class(x)=="RasterStack")
  {
    # add layers containing standard deviation, mean of moving window with
    # sizes (3x3),(5x5),(7x7),(9x9)
    if(!('sd3' %in% names(x))){
      x$sd3 <- focal(x$PC1, w = matrix(1/9,3,3), fun = sd)
    }
    if(!('sd5' %in% names(x))){
      x$sd5 <- focal(x$PC1, w = matrix(1/25,5,5),fun = sd)
    }
    if(!('sd7' %in% names(x))){
      x$sd7 <- focal(x$PC1, w = matrix(1/49,7,7),fun = sd)
    }
    if(!('sd9' %in% names(x))){
      x$sd9 <- focal(x$PC1, w = matrix(1/81,9,9),fun = sd)
    }
    if(!('sd11' %in% names(x))){
      x$sd11 <- focal(x$PC1, w = matrix(1/121,11,11),fun = sd)
    }
    if(!('mean3' %in% names(x))){
      x$mean3  <- focal(x$PC1, w = matrix(1/9,3,3),fun = mean)
    }
    if(!('mean5' %in% names(x))){
      x$mean5  <- focal(x$PC1, w = matrix(1/25,5,5),fun = mean)
    }
    if(!('mean7' %in% names(x))){
      x$mean7 <- focal(x$PC1, w = matrix(1/49,7,7),fun = mean)
    }
    if(!('mean9' %in% names(x))){
      x$mean9 <- focal(x$PC1, w = matrix(1/81,9,9),fun = mean)
    }
    if(!('mean11' %in% names(x))){
      x$mean11 <- focal(x$PC1, w = matrix(1/121,11,11),fun = mean)
    }
    if(!('sobelxsd' %in% names(x))){
      x$sobelxsd  <- focal(x$PC1, w = sobel_x, fun = sd)
    }
    if(!('sobelysd' %in% names(x))){
      x$sobelysd  <- focal(x$PC1, w = sobel_y, fun = sd)
    }
    if(!('laplace' %in% names(x))){
      x$laplace <- focal(x$PC1, w = laplace, fun = sd)
    }
    if(!('highpass' %in% names(x))){
      x$highpass <- focal(x$PC1, w = high_pass, fun = sd)
    }
  }
  return(x)
}

# https://stackoverflow.com/questions/19866009/pca-using-raster-datasets-in-r
# Calculate first principal component of the visible spectra:
# @param x RasterStack containing layers of the visible spectra (RGB) at the first three elements
# @return Rasterstack containing a layer of the first principal component
pc_calc <- function (x) {
  # extract layers of visible spectra 
  myRaster <- x[[1:3]]
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  x$PC1 <-rasterPCA(na.omit(myRaster), nComp = 1, spca = TRUE)$map
  return(x)
}

# Pre processing the raster or ggmap object: Conversion to a raster stack, 
# calculating principal component and adding texture layers
# @param x Raster object
# @return Raster stack prepared for extracting training data
my_preProcess <- function (x) {
  if(class(x)[1]=="ggmap"){
    x <- ggmap_to_rstack(x)
  }
  if(!('PC1' %in% names(x))){
    x <- pc_calc(x)
  }
  x <- focal_func(x)
  return (x)
}

# Converting ggmap into raster stack
# Project raster to utm 
# cut raster from Google signature
# @param object of the class ggmap
# @return raster stack 
ggmap_to_rstack <- function(x) {
  x <- ps_ggmap_to_raster(x)
  # projection got lost during the conversion
  crs(x) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # extract longitude to determine the utmzone
  my_lon = (extent(x)[2]+extent(x)[1])/2
  utm_zone_nr = floor((my_lon + 180) / 6) + 1
  # transformation to projected utm crs
  utm_zone <- "+proj=utm +zone=zid +datum=WGS84"
  utm_zone <- sub("zid",utm_zone_nr, utm_zone)
  x <- projectRaster(x,crs = utm_zone)
  # crop the extent to get a square image without google signs at the bottom
  newExtent <- extent(x)
  newExtent[1] <- newExtent[1]+(newExtent[2]-newExtent[1])/8
  newExtent[2] <- newExtent[2]-(newExtent[2]-newExtent[1])/8
  newExtent[3] <- newExtent[3]+(newExtent[2]-newExtent[1])/8
  newExtent[4] <- newExtent[4]-(newExtent[2]-newExtent[1])/8
  x <- crop(x, newExtent)
  return (x)
}

# Create train data of shapefiles
# @param x RasterStack
# @param y shapefile
# @param z file
prep_train_dat = function (x,y,z){
  # check if z already exists
  if(file.exists(z)&&file.info(z)$size!=0){
    ex_data = get(load(z))
    add_sth = TRUE
  }
  else{
    file.create(z)
    add_sth = FALSE
  }
  # transfrom crs of polygon to crs of RasterStack
  sampleDat <- read_sf(y)
  utm_zone = transform_to_utm(x)
  sampleDat <- st_transform(sampleDat, crs = utm_zone)
  # extract values from the rasterstack at the locations of the training data
  extr <- extract(x, sampleDat, df = TRUE)
  # add information of the polygon ID for better validation
  if(exists("ex_data")){
    sampleDat$PolyID <- max(ex_data$ID)+1:nrow(sampleDat)
    extr$ID = extr$ID+max(ex_data$ID)
  }
  else{
    sampleDat$PolyID <- 1:nrow(sampleDat)
  }
  # add geometry to the extraction
  merged <- merge(extr,sampleDat,by.x="ID",by.y="PolyID")
  # factorize the outcome values for the binary classification
  merged$id <- as.factor(merged$id)
  # remove NA values
  merged <- na.omit(merged)
  if(add_sth){
    new_train_data = rbind(ex_data, merged) 
  }
  else{
    new_train_data = merged
  }
  save(new_train_data,file=z)
}

# Transform crs from wgs84 to a Universal Transverse Mercator coordinate system
# @param x Raster object
# @return crs
transform_to_utm = function (x)
{
  crs_stack = crs(x)
  wgs84 = crs("+proj=longlat +datum=WGS84 +no_defs")
  if(compareCRS(crs(x),wgs84)){
    my_lon = (extent(x)[2]+extent(x)[1])/2
    utm_zone_nr = floor((my_lon + 180) / 6) + 1
    # transformation to projected utm crs
    utm_zone <- "+proj=utm +zone=zid +datum=WGS84"
    sub("zid",utm_zone_nr, utm_zone)
  }
  else{
    utm_zone = crs_stack
  }
  return(utm_zone)
}

require(RCurl)
# source(textConnection(getURL("https://gist.github.com/mages/5339689/raw/576263b8f0550125b61f4ddba127f5aa00fa2014/add.alpha.R")))
# Add an alpha value to a colour to make it transparent
# @param col Colour which should get transparent
# @param alpha Transparency level
# @return transparent colour
add.alpha <- function(col=NULL, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# Function for visual compare different layers of geometry variables with the test data
# @param x integer indicating which of the 48 images to compare
compare_mask <- function(x){
  grid.arrange(
    spplot(compare_same(res_pred[[x]])),
    spplot(list_masks[[x]]),colorkey= FALSE)
}

# Function for categorizing values to 0 and 1, gives na 0 and rest 1
# @param x RasterLayer
# @return RasterLayer
compare_same <- function(x){
  x[is.na(x)] = 0
  x[x>0] <- 1
  return(x)
}

###################
# Function for validating the prediction based on just one rf model and and two rf models
# @param x integer indicating which of 48 images to choose
# @return auc values for different predictions 
###################
confuse = function(x, visual = FALSE){
  # r represents simple model
  r = res_pred[[x]]
  # unify representation of burrow y/n
  #r[is.na(r)]=0
  # rf2 represents prediction based on two rf models
  rf2 = pp_res_pred[[x]]
  # unify representation of burrow y/n
  rf2[is.na(rf2)]=0
  # perfect result
  m = list_masks[[x]]
  testData = as.factor(m)
  # apply level change to fit to simple model
  levels(r)[[1]]$ID = c(0,1)
  levels(rf2)[[1]]$ID = c(0,1)
  levels(testData) = levels(r)
  
  pred = as.factor(values(r))
  pred2 = as.factor(values(rf2))
  levels(pred2) = c(0,1)
  truth = as.factor(values(testData))

  # visualization
  if(visual){
    grid.arrange(spplot(r, main="Vorhersage 1", colorkey = FALSE),
                 spplot(rf2, main = "Vorhersage 2", colorkey = FALSE),
                 spplot(testData, main = "Testdaten", colorkey = FALSE),
                 nrow=1)
  }
  # validation with AUCx
  auc1 = auc(truth,pred)
  auc2 = auc(truth,pred2)
  cM = confusionMatrix(table(pred,truth), positive = "1")
  cM2 = confusionMatrix(table(pred2,truth), positive = "1")
  sens1 = cM$byClass[1]
  spec1 = cM$byClass[2]
  sens2 = cM2$byClass[1]
  spec2 = cM2$byClass[2]
  # auc equals NaN if values for predictors or observers are all the same
  # considering this, they get default value of 0.5
  if(is.na(auc2)){
    auc2 = 0.5
  }
  return(c(auc1,auc2,sens1,spec1, sens2, spec2))
}

# Function for summarizing the resulting AUC values for all images
# @return Dataframe containing AUC values for prediction one and prediction two
my_summary = function(){
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  for (i in c(5:48)){
    df = rbind(df, data.frame(i,  confuse(i)[1],confuse(i)[2],
                                  confuse(i)[3],confuse(i)[4],
                                  confuse(i)[5],confuse(i)[6]))
  }
  colnames(df)=c("Nr","AUC1","AUC2", "Sens1","Spec1","Sens2","Spec2")
  return(df)
}

# Function for adding the standard deviation of the images to the auc comparing df
# @param x Dataframe containing AUC values for prediction 1 and 2
# @return Dataframe with Standard deviation of the images
add_imageVariance = function(x){
  df <- data.frame(matrix(ncol = 1, nrow = 0))
  for (i in c(5:48)){
    rgb_sd = cellStats(ldf[[i]], stat = sd)
    df = rbind(df, mean(rgb_sd))
  }
  res = cbind(x,df)
  colnames(res)[length(res)]="Sd"
  return(res)
}

# Function for adding the brightness of the images to the auc comparing df
# @param x Dataframe containing AUC values for prediction 1 and 2
# @return Dataframe with brightness of the images
add_image_brightness = function(x){
  df <- data.frame(matrix(ncol = 1, nrow = 0))
  for (i in c(5:48)){
    brightness = cellStats(ldf[[i]], stat = mean)
    df = rbind(df, mean(brightness))
  }
  res = cbind(x,df)
  colnames(res)[length(res)]="Brightness"
  return(res)
}

# Function for adding number of burrows df
# @param x Dataframe containing AUC values for prediction 1 and 2
# @return Dataframe with number of burrows
add_nr_burrows = function(x = data.frame(Pred1=numeric(),
                                         Pred2=numeric(), 
                                         Truth=numeric())){
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  for (i in c(5:48)){
    c_pred1 = clump(res_pred[[i]])
    c_pred2 = clump(pp_res_pred[[i]])
    c_truth = clump(list_masks[[i]])
    nr_b_p1 = max(na.omit(values(c_pred1)))
    if(max(na.omit(values(c_pred2)))==-Inf){
      nr_b_p2 = 0
    }
    else{
      nr_b_p2 = max(na.omit(values(c_pred2)))
    }
    nr_b_t = max(na.omit(values(c_truth)))
    df = rbind(df, c(nr_b_p1,nr_b_p2, nr_b_t))
  }
  colnames(df)=c("Nr_burrows_1","Nr_burrows_2","Truth")
  if(nrow(x) == 0){
    return(df)
  }
  else{
    res = cbind(x,df)
    return(res)
  }
}

# Function for plotting the auc standard deviation dependency
# @param x dataframe containing standard deviation of the image
makePlotSD = function(x){
  plot(x$AUC2~x$Sd,
       xlab = "Standardabweichung",ylab = "AUC",
       main = "Standardabweichung ~ AUC", pch = 2)
  points(x$AUC1~x$Sd, col = "blue", pch = 6)
  abline(lm(x$AUC2~x$Sd),lwd = 2)
  abline(lm(x$AUC1~x$Sd),lwd = 2,
         col = "blue")
   legend("topright", legend=c("Vorhersage 1", "Vorhersage 2"),
          col=c("blue","black"), lty = 1, lwd = 1, cex=0.6)
}

# Function for plotting the auc brightness dependency
# @param x dataframe containing brightness of the image
makePlotB = function(x){
  plot(x$AUC1~x$Brightness,
       xlab = "Helligkeit",ylab = "AUC",
       main = "Helligkeit ~ AUC", pch = 2)
  points(x$AUC2~x$Brightness, col = "blue", pch = 6)
  abline(lm(x$AUC2~x$Brightness),lwd = 2)
  abline(lm(x$AUC1~x$Brightness),lwd = 2,col = "blue")
  legend("topright", legend=c("Vorhersage 1", "Vorhersage 2"),
         col=c("blue","black"), lty = 1, lwd = 1, cex=0.8)
}
