# 3

# Add train data rgb images of Deep Learning folder
filenames <- list.files("data-raw-marmot/train", pattern="*.jpg", full.names=TRUE)
# create stacks
ldf <- lapply(filenames, stack)
# Pre process these stacks to apply model on it
res <- lapply(ldf, my_preProcess)
names(res) <- regmatches(filenames, regexpr("image_[0-9]-[0-9]*", filenames))

# apply model on list of preprocessed rasterstack
res_pred <- lapply(res, function (x,y) predict(x,y), y=model_1)

# Add train data masks of Deep Learning folder
filenames_mask <- list.files("data-raw-marmot/train_masks", pattern="*.png", full.names = TRUE)
# create raster layers
list_masks <- lapply(filenames_mask, raster)
names(list_masks) <- regmatches(filenames_mask, regexpr("Image_[0-9]_label-[0-9]*", filenames_mask))
# compute geometry variables
# http://www.umass.edu/landeco/research/fragstats/documents/fragstats.help.4.2.pdf
list_gV <- lapply(res_pred, geometryVariables)

# Prepare train data of geometry variables for second rf model
# create empty dataframe
df <- data.frame(ID=numeric(),Patches = numeric(),Ar = numeric(),SI = numeric(),
                 CA=numeric(),Up = numeric(), CAI = numeric(), PAR = numeric(),
                 Re=numeric(), Ru = numeric(), OIC = numeric(), CI1=numeric(),
                 CO1 = numeric(), CI2 = numeric(), CCI1 = numeric(), CCI2 = numeric(),
                 CO = numeric(), SHD = numeric(), C1 = numeric(), E = numeric(),
                 TR = numeric(), CR = numeric(), C2 = numeric(), FR = numeric(),
                 EI = numeric(), SF1 = numeric(), GSI = numeric(), SF2 = numeric(),
                 C3 = numeric(), SF3 = numeric(), id = factor()) 

# Add train data for first four images
train_data_rf2 = mask_to_train_dat(1, df)
train_data_rf2 = mask_to_train_dat(2, train_data_rf2)
train_data_rf2 = mask_to_train_dat(3, train_data_rf2)
train_data_rf2 = mask_to_train_dat(4, train_data_rf2)

save(train_data_rf2, file = "Train_data_2.RData")
# train second random forest on predictions
model_2 = sup_class_rf2(train_data_rf2)
save(model_2, file="Model_2.RData")

# predict raster stacks with second model
pp_res_pred <- lapply(list_gV, function (x,y) predict(x,y), y=model_2)

# Create a dataframe with validation results
result=my_summary()
result_rgb = add_imageVariance(result)
result_b = add_image_brightness(result_rgb)
result_nr_b = add_nr_burrows(result_b)
test = add_nr_burrows()
write.xlsx(test, "Nr_Burrows.xlsx")
