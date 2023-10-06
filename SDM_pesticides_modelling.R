
###############################################
####         Modelling Distributions       ####
####           bird species for the        ####
####         SDMs-Pesticides project       ####
###############################################

if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] == "L2100739RI") {
  wd <- "C:/Users/rotllxa/D5_FFGRCC_FarmlandBirds/"
  gbif_creds <- "C:/Users/rotllxa/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03", "jeodpp-terminal-dev-12", "jeodpp-terminal-jd002-03")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/Birds_SDM_Pesticides/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/Birds_SDM_Pesticides/")
  wd <- "/eos/jeodpp/home/users/rotllxa/Birds_SDM_Pesticides/"
  gbif_creds <- "/home/rotllxa/Documents/"
}else{
  wd <- "/Users/xavi_rp/Documents/D5_FFGRCC/"
  gbif_creds <- "/Users/xavi_rp/Dropbox/GBIF_credentials/"
}

setwd(wd)

library(tidyr)
library(ENMeval)
library(raster)
library(dplyr)
library(dismo)
library(data.table)
library(virtualspecies)
library(terra)
library(sf)

#library(keras)
# https://tensorflow.rstudio.com/reference/keras/install_keras/
# https://annie-wangliu.medium.com/how-to-run-keras-and-tensorflow-rstudio-46645b0d87d7
##install_keras(tensorflow = "cpu", method = "conda", envname = "CONDA_R_ENVIRONMENT_NAME")
#install_keras(tensorflow = "cpu", method = "conda", envname = "conda_Renv_2", version = "2.7.0")
#install_keras(tensorflow = "cpu", method = "conda", envname = "conda_R_env", version = "2.7.0")
#Sys.setenv(RETICULATE_PYTHON = "/home/rotllxa/.conda/envs/CONDA_R_ENVIRONMENT_NAME/bin/python")
#library(reticulate)
##use_condaenv('CONDA_R_ENVIRONMENT_NAME')
#use_condaenv('conda_Renv_2')
#use_condaenv('conda_R_env')
#library(tensorflow)
#library("keras")
#keras_model_sequential()



## Species ####

# Candidates for modelling:
sps <- read.csv("/home/rotllxa/Documents/Birds_SDM_Pesticides/sp_list_FBI_3.csv", header = FALSE)
sps
sps <- as.vector(sps$V2)
sps


## Predictors ####

### Worldclim and elevation ####

preds_dir <- "/eos/jeodpp/home/users/rotllxa/European_butterflies_SDMs_data/"

worldclim_all <- stack(paste0(preds_dir, "worldclim_all.tif"))
worldclim_all <- terra::rast(paste0(preds_dir, "worldclim_all.tif"))
worldclim_all
#plot(worldclim_all[[3]])
worldclim_all_names <- c("wc2.1_30s_bio_1", "wc2.1_30s_bio_10", "wc2.1_30s_bio_11", "wc2.1_30s_bio_12", "wc2.1_30s_bio_13", "wc2.1_30s_bio_14",
                         "wc2.1_30s_bio_15", "wc2.1_30s_bio_16", "wc2.1_30s_bio_17", "wc2.1_30s_bio_18", "wc2.1_30s_bio_19", "wc2.1_30s_bio_2",
                         "wc2.1_30s_bio_3", "wc2.1_30s_bio_4", "wc2.1_30s_bio_5", "wc2.1_30s_bio_6", "wc2.1_30s_bio_7", "wc2.1_30s_bio_8",
                         "wc2.1_30s_bio_9",  "wc2.1_30s_elev") 
worldclim_all_names <- gsub("wc2.1_30s_", "", worldclim_all_names)
names(worldclim_all) <- worldclim_all_names
#worldclim_all <- stack(worldclim_all)
worldclim_all

plot(worldclim_all[[1]])



### Creating a mask for the area od Interest: France ####
create_mask <- "yes"
create_mask <- "no"

if(create_mask == "yes"){
  library(giscoR)  # https://ropengov.github.io/giscoR/
  fr_gisco <- gisco_get_nuts(country = "France", nuts_level = 0)
  plot(fr_gisco[1])
  
  fr_cont <- st_crop(fr_gisco[1], xmin = -5.5, xmax = 8.5, ymin = 42, ymax = 51)
  plot(fr_cont)
  fr_cont
  
  msk_FR <- terra::mask(worldclim_all[[1]], 
                        mask = fr_cont
                        #filename = ""
  )
  plot(msk_FR)
  
  fr_ext <- terra::ext(-5.5, 8.5, 42, 51)
  FR_mask <- terra::crop(msk_FR, fr_ext)
  
  FR_mask <- terra::ifel(!is.na(FR_mask), 1, NA)
  plot(FR_mask, col = "green")
  names(FR_mask) <- "mask"
  FR_mask
  
  writeRaster(FR_mask, filename = "mask_FR.tif", overwrite = TRUE)
}


run_masking <- "yes"
run_masking <- "no"
if(run_masking == "yes"){
  worldclim_all_FR <- terra::crop(worldclim_all, 
                                  FR_mask)
  worldclim_all_FR <- terra::mask(worldclim_all_FR, 
                                  FR_mask,
                                  filename = "worldclim_all_FR.tif",
                                  overwrite = TRUE)
  plot(worldclim_all_FR[[1]])
  plot(worldclim_all_FR[[10]])
}

worldclim_all_FR <- terra::rast("worldclim_all_FR.tif")
worldclim_all_FR
plot(worldclim_all_FR[[19]])
head(values(worldclim_all_FR[[19]], mat = FALSE), 100)
sum(!is.na(values(worldclim_all_FR[[19]], mat = FALSE)))
names(worldclim_all_FR)


### Land cover ####

lc_dir <- "/Users/xavi_rp/Documents/D5_FFGRCC/land_cover/"
lc_dir <- "/eos/jeodpp/home/users/rotllxa/land_cover/"
lc1km_files <- list.files(lc_dir, full.names = TRUE)[grepl("lc1km_", list.files(lc_dir))]
lc1km_files
lc1km_all <- rast(lc1km_files)
lc1km_all
names(lc1km_all) <- c("Bare_SparseVeg", "ClosedForest", "Cropland",
                      "Herbaceous", "OpenForest", "Shrubland", "WetlandVeg")
lc1km_all


run_masking <- "yes"
run_masking <- "no"
if(run_masking == "yes"){
  lc1km_all_FR <- terra::crop(lc1km_all, 
                              FR_mask)
  lc1km_all_FR <- terra::mask(lc1km_all_FR, 
                              FR_mask,
                              filename = "lc1km_all_FR.tif",
                              overwrite = TRUE)
  plot(lc1km_all_FR[[3]])
}
  
lc1km_all_FR <- rast("lc1km_all_FR.tif")
lc1km_all_FR


worldclim_all_FR <- c(worldclim_all_FR, lc1km_all_FR)
names(worldclim_all_FR)
plot(worldclim_all_FR[[19]])
plot(worldclim_all_FR[[23]])
worldclim_all_FR

sum(!is.na(values(worldclim_all_FR[[24]], mat = FALSE)))  # cells with data


#worldclim_all_data <- fread(paste0(preds_dir, "worldclim_all_data.csv"), header = TRUE)
#names(worldclim_all_data) <- names(worldclim_all)
#worldclim_all_data


## Background points ####
bckgr <- read.csv(paste0(preds_dir, "background_points.csv"), header = TRUE)
nrow(bckgr)



## FBI birds Occurrences ####

getwd()
occs_all <- fread(paste0("sp_records_20220308", ".csv"), header = TRUE)
length(unique(occs_all$species)) == length(unique(occs_all$sp2))  # just to confirm that sp2 is correct (if TRUE)


## species for modelling
sps
occs_all <- occs_all[occs_all$species %in% sps, ]
occs_all
table(occs_all$species)

#occs all years (1990:2021)
#Anthus pratensis   Emberiza citrinella     Serinus serinus         Upupa epops 
#           232105              432934               59965               33395 


## Years for modelling 
#EBBA2_years <- 2013:2017
yrs <- 2013:2017

occs_all_yrs <- occs_all[year %in% yrs, ]
occs_all_yrs

head(occs_all_yrs)
table(occs_all_yrs$species)
#occs years 2013:2017
#Anthus pratensis   Emberiza citrinella     Serinus serinus         Upupa epops 
#           75533              142100               23876               14852


occs_all <- occs_all_yrs


tbl <- table(occs_all_yrs$species)
ordr <- names(sort(tbl))
spcies <- data.frame(taxons = unique(occs_all_yrs$sp2), sps = unique(occs_all_yrs$species))
spcies <- spcies[match(ordr, spcies$sps),]
spcies
taxons <- spcies$taxons
taxons


unique(occs_all$year)


## Modelling with MaxEnt ####

info_models_maxent <- c()
info_models_mlp <- c()     # for MultiLayer Perceptrons
info_models_cnn <- c()     # for Convolutional NN
info_models_cnnt <- c()     # for CNN with tensors


# Threshold to use for converting to presence/absence
# Options: kappa,  spec_sens, no_omission, prevalence, equal_sens_spec, sensitivity

threshold2use <- "sensitivity"    # deffault 0.9
#threshold2use <- "no_omission"    # keeping all presences
Sys.time()



for (t in taxons){
  #print(t)
  #t <- taxons[1]
  t0 <- Sys.time()
  print(t0)
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running... ", sps))
  
  dir2save_maxent <- paste0("models_maxent_", t, "/")
  if(!dir.exists(paste0("models_maxent_", t))) {
    dir.create(dir2save_maxent)
  }
  
  dir2save_presences <- paste0("pres4modelling", "/")
  if(!dir.exists(paste0("pres4modelling"))) {
    dir.create(dir2save_presences)
  }
  
  occs_i <- occs_all[occs_all$sp2 %in% t, c("decimalLongitude", "decimalLatitude")]
  occurrences_raw <- nrow(occs_i)
  
  occs_i_shp <- SpatialPointsDataFrame(coords = occs_i[, c("decimalLongitude", "decimalLatitude")],
                                       data = data.frame(sp = rep(1, nrow(occs_i))),
                                       proj4string = CRS("+init=EPSG:4326"))
  #names(occs_i_shp) <- t
  occs_i_rstr <- rasterize(occs_i_shp, worldclim_all[[1]], field = "sp", background = 0)
  #names(occs_i_rstr) <- t
  #occs_i_rstr <- occs_i_rstr[[2]]
  occs_i_rstr <- mask(occs_i_rstr, worldclim_all[[1]])
  
  #assign(paste0(t, "_rstr"), occs_i_rstr)
  #print(sum(getValues(occs_i_rstr) == 1, na.rm = T))
  
  
  ## occurrences for training/testing
  sps_data <- stack(occs_i_rstr, worldclim_all) 
  sps_data <- as.data.table(as.data.frame(sps_data))
  sps_data[, raster_position := 1:nrow(sps_data)]
  
  # data set for presences
  sps_data_presences <- sps_data[layer == 1, ]
  sps_data_presences <- sps_data_presences[complete.cases(sps_data_presences), ]
  occurrences_1km <- nrow(sps_data_presences)
  rm(sps_data); gc()
  
  # data set for pseudo-absences
  sps_data_absences <- as.data.table(as.data.frame(raster::extract(worldclim_all, bckgr, cellnumbers = TRUE)))
  sps_data_absences <- sps_data_absences[!sps_data_absences$cells %in% sps_data_presences$raster_position, ]
  names(sps_data_absences)
  
  nrow(sps_data_presences)
  nrow(sps_data_absences)
  
  prop4test <- 0.3
  prop4train <- 1 - prop4test
  
  sps_data_presences_train <- sample_n(sps_data_presences, ceiling(nrow(sps_data_presences) * prop4train))
  sps_data_presences_test <- sps_data_presences[!sps_data_presences$raster_position %in% sps_data_presences_train$raster_position, ]
  
  write.csv(sps_data_presences_train, paste0(dir2save_presences, "/sps_data_presences_train_", t, ".csv"), row.names = FALSE)
  write.csv(sps_data_presences_test, paste0(dir2save_presences, "/sps_data_presences_test_", t, ".csv"), row.names = FALSE)
  write.csv(sps_data_absences, paste0(dir2save_presences, "/sps_data_absences_", t, ".csv"), row.names = FALSE)
  
  #sps_data_presences_train <- fread(paste0(dir2save_presences, "/sps_data_presences_train_", t, ".csv"), header = TRUE)
  #sps_data_presences_test <- fread(paste0(dir2save_presences, "/sps_data_presences_test_", t, ".csv"), header = TRUE)
  #sps_data_absences <- fread(paste0(dir2save_presences, "/sps_data_absences_", t, ".csv"), header = TRUE)  
  
  
  ## Running ENMeval (https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0.0-vignette.html)
  ## Including a tryCatch to avoid stop process if there's an error because of a bug with "H" transformation or something
  
  library(dismo)
  library(ENMeval)
  
  dir_func <- function(sps_data_presences, worldclim_all, sps_data_absences, fc){ # to avoid stop modelling if low number of background points or other errors
    res <- tryCatch(
      {
        library(dismo)
        library(ENMeval)
        #modl1 <- ENMevaluate(occs = sps_data_presences[1:3000, .SD, .SDcols = names(worldclim_all)], 
        modl1 <- ENMevaluate(occs = sps_data_presences_train[, .SD, .SDcols = names(worldclim_all)], 
                             envs = NULL, 
                             bg = sps_data_absences[, .SD, .SDcols = names(worldclim_all)], 
                             algorithm = 'maxnet', 
                             #partitions = 'block', 
                             partitions = "testing",
                             #occs.testing = sps_data_presences[3001:3750, .SD, .SDcols = names(worldclim_all)],  # occurrences for testing; only when partitions = 'testing'
                             occs.testing = sps_data_presences_test[, .SD, .SDcols = names(worldclim_all)],  # occurrences for testing; only when partitions = 'testing'
                             tune.args = list(
                               #fc = c("L","LQ","LQH","H"),
                               #fc = c("L","LQ","LQH"),  # removed "H" because there is some bug in maxnet that gives error for some species
                               #fc = c("L","LQ"),  # removed "H" and "LQH" because there is some bug in maxnet that gives error for some species
                               #fc = c("L","LQ"),
                               fc = fc,
                               rm = c(1, 2, 5)
                               #rm = 1:2
                             ),
                             quiet = TRUE,
                             parallel = TRUE,
                             #parallel = FALSE,
                             numCores = 12
                             #numCores = 4
                             )
        
      },
      error = function(con){
        message(con)
        return(NULL)
      }
    )
    if(exists("modl1")){ return(modl1) }else{ return(NULL) }
  } #end of dir_func
  
  
  fc_opts <- list(c("L","LQ","LQH","H"), c("L","LQ","LQH"), c("L","LQ"), "L")
  
  for(fc in fc_opts){
    modl <- dir_func(sps_data_presences, worldclim_all, sps_data_absences, fc)
    if(!is.null(modl)) break
  }
  
  
  #rm(sps_data_absences); gc()
  modl
  modl@results
  #View(modl@results)
  write.csv(modl@results, file = paste0(dir2save_maxent, "ENMeval_results_", t, ".csv"))
  save(modl, file = paste0(dir2save_maxent, "models_", t, ".RData"))
  #evalplot.stats(e = modl, stats = "or.mtp", color = "fc", x.var = "rm")
  #load(paste0(dir2save_maxent, "models_", t, ".RData"), verbose = TRUE)
  
  occurrences_train <- nrow(modl@occs)
  occurrences_test <- nrow(modl@occs.testing)  # none because cross-validation
  background_points <- nrow(modl@bg)
  
  
  # selecting optimal model
  results <- eval.results(modl)
  results
  #View(results)
  optimal <- results %>% filter(delta.AICc == 0)
  optimal
  if(nrow(optimal) > 1) optimal <- optimal[1, ]
  
  modl_args <- eval.models(modl)[[optimal$tune.args]]
  modl_args$betas
  #str(modl_args)
  
  #dev.off()
  pdf(paste0(dir2save_maxent, "opt_model_RespCurves_", t, ".pdf"))
  plot(modl_args, type = "cloglog")
  # And these are the marginal response curves for the predictor variables wit non-zero 
  # coefficients in our model. We define the y-axis to be the cloglog transformation, which
  # is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
  # (Phillips et al. 2017).
  dev.off()
  
  modl <- modl@models[[optimal$tune.args]]
  gc()
  
  #save(modl, file = paste0(dir2save_maxent, "opt_model_", t, ".RData"))
  
  # making predictions
  #worldclim_all_data <- fread("worldclim_all_data_NoCor.csv", header = TRUE)
  #worldclim_all_data <- fread("worldclim_all_data_NoCor_070.csv", header = TRUE)
  #worldclim_all_data <- fread("worldclim_all_data_NoCor_040.csv", header = TRUE)
  #worldclim_all_data <- fread(paste0(preds_dir, "worldclim_all_data.csv"), header = TRUE)
  #names(worldclim_all_data) <- names(worldclim_all)
  #names(worldclim_all_data) <- gsub("wc2.1_30s_bio_", "worldclim_all.", names(worldclim_all_data))
  #names(worldclim_all_data) <- gsub("wc2.1_30s_elev", "worldclim_all.20", names(worldclim_all_data))
  #names(worldclim_all_data) <- gsub("worldclim_all", "worldclim_all", names(worldclim_all_data))
  #worldclim_all_data <- worldclim_all_data[complete.cases(worldclim_all_data), ]
  sps_predictions_maxent <- predict(object = modl, 
                                    newdata = worldclim_all_data, 
                                    clamp = TRUE,
                                    type = c("cloglog")
  )
  #rm(worldclim_all_data); gc()
  sps_predictions_maxent <- as.data.table(sps_predictions_maxent)
  head(sps_predictions_maxent)
  range(sps_predictions_maxent)
  nrow(sps_predictions_maxent)
  
  worldclim_all_data0 <- as.data.table(as.data.frame(worldclim_all[[1]]))
  worldclim_all_data0$raster_position <- 1:nrow(worldclim_all_data0)
  
  worldclim_all_data1 <- worldclim_all_data0
  worldclim_all_data1 <- worldclim_all_data1[complete.cases(worldclim_all_data1), ]
  
  worldclim_all_data0 <- worldclim_all_data0[, .SD, .SDcols = "raster_position"]
  
  worldclim_all_data1[, predictions := sps_predictions_maxent$V1]
  
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions")], 
                               by = "raster_position", all.x = TRUE)
  
  #rm(worldclim_all_data1); gc()
  
  sps_preds_rstr <- worldclim_all[[1]]
  sps_preds_rstr <- setValues(sps_preds_rstr, worldclim_all_data0$predictions)
  names(sps_preds_rstr) <- "predictions_maxent"
  
  #rm(worldclim_all_data0); gc()
  
  
  #pdf("sps_predictions_maxent_kk.pdf", width = 20, height = 15)
  #par(mfrow = c(1, 2))
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #plot(occs_i_shp, add = TRUE, col = "black")
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #dev.off()
  
  
  #BI_mxnt <- ecospat::ecospat.boyce(fit = sps_preds_rstr,
  #                                  obs = linaria_pres_test_coords, 
  #                                  nclass = 0, 
  #                                  window.w = "default", 
  #                                  res = 100, 
  #                                  PEplot = TRUE)
  
  ## Creating presence/absence map
  # Threshold: minimum presence
  
  #info_models_maxent
  #threshold1 <- min(extract(sps_preds_rstr, occs_i_shp))
  #threshold1 <- quantile(extract(sps_preds_rstr, occs_i_shp), 0.1)#, na.rm = TRUE) # sensitivity = 0.9
  #threshold1
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr, occs_i_shp), 
                                                 extract(sps_preds_rstr, bckgr))) # sensitibity default 0.9
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr, occs_i_shp), 
                                                 extract(sps_preds_rstr, bckgr)),
                                 stat = "", sensitivity = 0.99) 
  thresholds
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2 <- 0.09993547
  #threshold2use <- "no_omission"
  #threshold2use <- "sensitivity"
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr[["predictions_maxent"]], rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  sps_preds_rstr_pres_abs_all <- brick(sps_preds_rstr_pres_abs)
  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt")
  
  #plot(sps_preds_rstr_pres_abs)
  
  pdf(paste0(dir2save_maxent, "sps_predictions_maxent_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr[["predictions_maxent"]], zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr[["predictions_maxent"]], zlim = c(0, 1), main = "MaxEnt predictions (cloglog)", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, " (0.99)'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  
  running_time <- as.vector(Sys.time() - t0)
  if(exists("data2save")) rm(data2save)
  data2save <- data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
                          occurrences_test, background_points, optimal,
                          thresholds, threshold_used)
  rownames(data2save) <- t
  
  info_models_maxent <- rbind(info_models_maxent, data2save)
  #write.csv(info_models_maxent, "info_models_all_species.csv", row.names = FALSE)
  #write.csv(info_models_maxent, "info_models_all_species_085.csv", row.names = FALSE)
  write.csv(info_models_maxent, "info_models_maxent_all.csv", row.names = FALSE)
  write.csv(info_models_maxent, paste0(dir2save_maxent, "info_models_maxent_all.csv"), row.names = FALSE)
  #info_models_maxent <- fread("info_models_maxent_all.csv")
  #info_models_maxent <- info_models_maxent[-3, ]
  
  print(paste0(t, " run in: ", running_time))
  
  
  
  ## Modelling with Multilayer perceptrons ####
  
  # Multilayer Perceptrons: The simplest deep networks, they consist of multiple layers of neurons each 
  # fully connected to those in the layer below (from which receive input) and those above (which they influence).
  # 
  
  #library(reticulate)
  #use_condaenv('conda_R_env')
  #library(tensorflow)
  library("keras")
  
  #print(t)
  t0 <- Sys.time()
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running Multilayer Perceptrons for... ", sps))
  
  dir2save_mlp <- paste0("models_MLP_", t, "/")
  if(!dir.exists(paste0("models_MLP_", t))) {
    dir.create(dir2save_mlp)
  }
  
  # For now, each point is associated to a one (the same) point of the explanatory variables.
  # Later, following Botella's approach (?), each point (presence and absence) can be associated to a 
  # matrix of pixels surrounding the point (e.g. 3x3 or 5x5). This is to capture the influence of 
  # the "micro-habitat" of the plant.
  
  
  nrow(sps_data_absences)
  #nrow(sps_data_presences)
  nrow(sps_data_presences_train)
  nrow(sps_data_presences_test)  
  
  
  # build a model
  if(exists("model")) rm(model); gc()
  
  
  
  model <- keras_model_sequential()
  #inpt_shpe <- length(sps_data_presences_train[, names(worldclim_all)]) - 1
  inpt_shpe <- length(sps_data_presences_train[, names(worldclim_all)])      # number of variables
  
  reg_L2 <- 0.001  # L2 regularizer (penalty to limit overfitting)
  
  
  model %>%
    #layer_flatten(input_shape = c(1174, 3)) %>%            # transforms the format of the images from a 2d-array (of 28 by 28 pixels), to a 1d-array of 28 * 28 = 784 pixels. This layer has no parameters to learn; it only reformats the data.
    # this is probably needed if tensors are used instead of punctual environmental vector
    layer_dense(units = 128, activation = 'relu', input_shape = c(inpt_shpe),
                kernel_regularizer = regularizer_l2(reg_L2)
                ) %>%     #
    #layer_dropout(rate = 0.1) %>%   
    layer_dense(units = 128, activation = 'relu',
                kernel_regularizer = regularizer_l2(reg_L2)
                ) %>%     #
    layer_dense(units = 128, activation = 'relu',
                kernel_regularizer = regularizer_l2(reg_L2)
                ) %>%     #
    layer_dense(units = 128, activation = 'relu',
                kernel_regularizer = regularizer_l2(reg_L2)
                ) %>%     #
    layer_dense(units = 128, activation = 'relu',
                kernel_regularizer = regularizer_l2(reg_L2)
                ) %>%     #
    layer_dense(units = 128, activation = 'relu',
                kernel_regularizer = regularizer_l2(reg_L2)
                ) %>%     #
    #layer_dense(units = 10, activation = 'softmax')
    layer_dense(units = 1, activation = 'sigmoid')
  
  model
  # The batch size defines the number of samples that will be propagated through the network
  # e.g. we have 1050 training samples and you want to set up a batch_size equal to 100. The algorithm takes the first 100 
  # samples (from 1st to 100th) from the training dataset and trains the network. Next, it takes the second 100 samples 
  # (from 101st to 200th) and trains the network again.
  # Advantage: mmore emory efficient and faster 
  # Disadvantage: the smaller the batch the less accurate the estimate of the gradient will be
  
  # Neural network terminology:
  # one epoch = one forward pass and one backward pass of all the training examples
  # batch size = the number of training examples in one forward/backward pass. The higher the batch size, the more memory space you'll need.
  # number of iterations = number of passes, each pass using [batch size] number of examples. To be clear, one pass = one forward pass + one backward pass (we do not count the forward pass and backward pass as two different passes).
  # Example: if you have 1000 training examples, and your batch size is 500, then it will take 2 iterations to complete 1 epoch.
  
  # If you pass both batch_size=32 and input_shape=c(6, 8) to a layer, it will then expect every 
  # batch of inputs to have the batch shape (32, 6, 8)
  
  # Capinha et al (2021) generate 20 random models and chose the one with the best architecture by training them with a small balanced
  # data set. To determine the best, the models are validated with another data set.
  # Then, when model with best architecture is chosen, they train it with a bigger training data set.
  # To determine the best number of epochs, they check how the performance (AUC over testing data set) of the final model trained 
  # with the full training data set increases with the number of epochs, until the increase stabilizes during 25 epochs.
  
  
  # Activation function (aka Transfer Function; https://towardsdatascience.com/activation-functions-neural-networks-1cbd9f8d91d6):
  # used to determine the output of neural network like yes or no. It maps the resulting values in 
  # between 0 to 1 or -1 to 1 etc. (depending upon the function).
  # 1) Linear or Identity Activation Function: the output of the functions will not be confined between 
  #    any range.
  # 2) Nonlinear Activation Functions are the most used activation functions.
  #    a) Sigmoid Function curve looks like a S-shape: especially used for models where we have to predict 
  #       the probability as an output. Since probability of anything exists only between the range of 0 and 1, sigmoid is the right choice.
  #    b) The softmax function is a more generalized logistic activation function which is used for multiclass classification.
  #       maps our output to a [0,1] range but also maps each output in such a way that the total sum is 1. 
  #       The output of Softmax is therefore a probability distribution.
  #    a-b) In conclusion, Softmax is used for multi-classification in logistic regression model (multivariate) 
  #         whereas Sigmoid is used for binary classification in logistic regression model.
  #    c) ReLU is the most used activation function right now (used in almost all the convolutional neural networks or deep learning)
  #       Range: [ 0 to infinity)
  #       But the issue is that all the negative values become zero immediately which decreases the ability of the 
  #       model to fit or train from the data properly. That means any negative input given to the ReLU activation 
  #       function turns the value into zero immediately in the graph, which in turns affects the resulting graph 
  #       by not mapping the negative values appropriately.
  
  
  # Dropout involves injecting noise while computing each internal layer during forward propagation, 
  # and it has become a standard technique for training neural networks.
  # Typically, we disable dropout at test time. Given a trained model and a new example, we do not drop out
  # any nodes and thus do not need to normalize. However, there are some exceptions: some researchers use 
  # dropout at test time as a heuristic for estimating the uncertainty of neural network predictions: 
  # if the predictions agree across many different dropout masks, then we might say that the network is 
  # more confident.
  
  
  
  ## Compile the model
  # Optimizers are algorithms or methods used to change the attributes of your neural network such as weights 
  # and learning rate in order to reduce the losses and to provide the most accurate results possible.
  # 1) Gradient Descent is the most basic but most used optimization algorithm. It’s used heavily in linear regression and classification algorithms. Backpropagation in neural networks also uses a gradient descent algorithm.
  #    It requires a lot of memory and long time to converge for big data sets. 
  # 2) Stochastic Gradient Descent. e.g. optimizer = optimizer_rmsprop(lr = 0.002), where 'lr' is initial learning rate. 
  #    A too big lr may lead to training loss divergence; too small, learning can be very slow. Botella, 2018, used lr = 1e-8.
  #    It converges in less time and requires less memory.
  # 3) Mini-Batch Gradient Descent. Difficult to choose an optimum value of the learning rate. If the learning rate is too small than gradient descent may take ages to converge.   
  # 4) Momentum was invented for reducing high variance in SGD and softens the convergence. It accelerates the convergence towards the relevant direction and reduces the fluctuation to the irrelevant direction.
  # 5) Adam. Fast and converges rapidly, but computationally costly.
  
  
  
  model %>% compile(
    #optimizer = 'adam',    # This is how the model is updated based on the data it sees and its loss function.
    #optimizer = "rmsprop",
    #optimizer = optimizer_rmsprop(lr = 1e-8),  # for a binary classification problem
    #optimizer = optimizer_rmsprop(),  # for a binary classification problem
    optimizer = optimizer_sgd(#lr = 1e-8, 
      #lr = 1e-6,
      #lr = 1e-4,
      learning_rate = 1e-5,
      momentum = 0.9),
    #loss = 'sparse_categorical_crossentropy',   # This measures how accurate the model is during training. We want to minimize this function to “steer” the model in the right direction.
    loss = "binary_crossentropy",  # for a binary classification proble
    #metrics = c('accuracy')   # Used to monitor the training and testing steps. e.g. "accuracy", the fraction of the images that are correctly classified.
    #metrics = metric_binary_accuracy  # for a binary classification problems
    metrics = c(tensorflow::tf$keras$metrics$AUC(), metric_binary_accuracy)   # AUC from tensorflow + binary accuracy
  )
  model
  
  
  ## Training the model
  
  sps_train_data_0 <- sps_data_absences[, .SD, .SDcols = names(worldclim_all)]
  sps_train_data_0$layer <- 0
  sps_train_data_0 <- sps_train_data_0[, .SD, .SDcols = c("layer", names(worldclim_all))]
  
  sps_train_data <- rbind(sps_data_presences_train[, .SD, .SDcols = c("layer", names(worldclim_all))], sps_train_data_0)
  unique(sps_train_data$layer)
  nrow(sps_train_data)
  
  sps_test_data <- rbind(sps_data_presences_test[, .SD, .SDcols = c("layer", names(worldclim_all))], sps_train_data_0)
  nrow(sps_test_data)
  head(sps_test_data)
  unique(sps_test_data$layer)
  sum(is.na(sps_test_data))
  
  sps_train_data_1 <- as.matrix(sps_train_data[, .SD, .SDcols = c(names(worldclim_all))])
  head(sps_train_data_1)
  dim(sps_train_data_1)
  nrow(sps_train_data_1)
  
  
  sps_train_labels <- as.matrix(sps_train_data[, "layer", drop = FALSE])
  head(sps_train_labels)
  unique(sps_train_labels)
  nrow(sps_train_labels)
  dim(sps_train_labels)
  
  
  #t0 <- Sys.time()
  model %>% fit(sps_train_data_1,
                sps_train_labels, 
                epochs = 200,  # The number of "epochs" is a hyperparameter that defines the number times that the learning algorithm will work through the ENTIRE training dataset.
                # The "batch" size is a hyperparameter that defines the NUMBER OF SAMPLES to work through before updating the internal model parameters.
                batch_size = 128,
                verbose = 0
  )
  #Sys.time() - t0
  
  # https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_save_and_restore/
  save_model_hdf5(model, 
                  filepath = paste0(dir2save_mlp, "model_MLP.h5"), 
                  overwrite = TRUE, 
                  include_optimizer = TRUE)
  
  
  
  ## Test the model
  
  score_mlp_test <- model %>% evaluate(as.matrix(sps_test_data[, .SD, .SDcols = names(worldclim_all)]), 
                                       as.matrix(sps_test_data[, "layer", drop = FALSE]), 
                                       batch_size = 128
                                       )
  lst2safe <- c("score_mlp_test")
  save(list = lst2safe, file = paste0(dir2save_mlp, "/evaluations.RData"))
  #load("evaluations.RData", verbose = TRUE)
  score_mlp_test #for test data
  #       loss           auc    binary_accuracy 
  # 0.01116585      0.99933493      0.99636084
  
  
  # for training data (last epoch)
  # loss: 0.0958 - auc: 0.9564 - binary_accuracy: 0.9651
  # The gap between training accuracy and test accuracy is an example of "overfitting" 
  # train_auc (0.9564) < test_auc (0.97014868)  --> No overfitting
  # train_accuracy (0.9651) < test_accuracy (0.98521733)  --> No overfitting
  # Overfitting is when a machine learning model performs worse on new data than on their training data
  
  
  ## Make predictions over the whole extent
  nrow(worldclim_all_data)
  sum(complete.cases(worldclim_all_data))
  
  #predictions <- model %>% predict_proba(as.matrix(worldclim_all_data), batch_size = 128)
  predictions_mlp <- model %>% predict(as.matrix(worldclim_all_data), batch_size = 128)
  head(predictions_mlp)
  nrow(predictions_mlp)
  sum(predictions_mlp[, 1])
  
  length(unique(predictions_mlp[, 1]))
  range(predictions_mlp[, 1])
  max(predictions_mlp[, 1])
  summary(predictions_mlp[, 1])
  
  ## Mapping predictions
  worldclim_all_data1[, predictions_mlp := as.vector(predictions_mlp[, 1])]
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions_mlp")], 
                               by = "raster_position", all.x = TRUE)
  
  sps_preds_rstr <- brick(sps_preds_rstr)
  sps_preds_rstr <- setValues(sps_preds_rstr, 
                              values = worldclim_all_data0$predictions_mlp,
                              layer = 2)
  names(sps_preds_rstr) <- c("predictions_maxent", "predictions_MLP")
  sps_preds_rstr
  
  #pdf("sps_predictions_maxent_kk.pdf", width = 20, height = 15)
  #par(mfrow = c(1, 2))
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #plot(occs_i_shp, add = TRUE, col = "black")
  #plot(sps_preds_rstr, zlim = c(0, 1))
  #dev.off()
  
  
  ## Boyce Index
  
  sps_data_presences_test_coords <- xyFromCell(object = sps_preds_rstr, cell = sps_data_presences_test$raster_position)
  
  BI_mlp <- ecospat::ecospat.boyce(fit = sps_preds_rstr[["predictions_MLP"]],
                                   obs = sps_data_presences_test_coords, 
                                   nclass = 0, 
                                   window.w = "default", 
                                   res = 100, 
                                   PEplot = TRUE)
  
  as.vector(unlist(BI_mlp[grepl("cor", names(BI_mlp))]))
  #BI_mlp$cor  # 0.962 (DL)
  
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr[["predictions_MLP"]], occs_i_shp), 
                                                 extract(sps_preds_rstr[["predictions_MLP"]], bckgr))) # sensitibity default 0.9
  thresholds
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr[["predictions_MLP"]], rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  sps_preds_rstr_pres_abs_all <- stack(sps_preds_rstr_pres_abs_all, sps_preds_rstr_pres_abs)
  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP")
  #plot(sps_preds_rstr_pres_abs)
  
  pdf(paste0(dir2save_mlp, "sps_predictions_MLP_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr[["predictions_MLP"]], zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr[["predictions_MLP"]], zlim = c(0, 1), main = "MLP predictions", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, "'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  
  
  running_time <- as.vector(Sys.time() - t0)
  
  if(exists("data2save")) rm(data2save)
  data2save <- data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
                          occurrences_test, background_points, 
                          auc.val = as.vector(score_mlp_test[grepl("auc", names(score_mlp_test))]),
                          cbi.val = as.vector(unlist(BI_mlp[grepl("cor", names(BI_mlp))])),
                          #cbi.val = BI_mlp$cor,
                          thresholds, threshold_used)
  
  rownames(data2save) <- t
  
  info_models_mlp <- rbind(info_models_mlp, data2save)
  write.csv(info_models_mlp, "info_models_MLP_all.csv", row.names = FALSE)
  write.csv(info_models_mlp, paste0(dir2save_mlp, "info_models_MLP_all.csv"), row.names = FALSE)
  #info_models_mlp <- fread("info_models_MLP_all.csv")
  #info_models_mlp <- info_models_mlp[-3, ]
  
  
  print(paste0(t, " run in: ", running_time))
  
  
  
  ## Convolutional Neural Networks ####
  # https://d2l.ai/
  
  # CNN: A particular case of NN
  # 1D-CNN means that the convolution kernel is convolved with the layer input 
  # over a single spatial (or temporal) dimension to produce a tensor of outputs
  # In other words, the input layer has only one dimension, in our case one pixel
  # and its punctual values of environmental variables
  
  t0 <- Sys.time()
  sps <- spcies[spcies$taxons == t, "sps"]
  
  print(paste0("running Convolutional neural Networks for... ", sps))
  
  dir2save_cnn <- paste0("models_CNN_", t, "/")
  if(!dir.exists(paste0("models_CNN_", t))) {
    dir.create(dir2save_cnn)
  }
  
  nrow(sps_data_absences)
  nrow(sps_data_presences_train)
  nrow(sps_data_presences_test)  
  
  sps_train_data
  table(sps_train_data$layer)
  sps_test_data
  table(sps_test_data$layer)
  sps_train_data_1
  dim(sps_train_data_1)
  sps_train_labels
  dim(sps_train_labels)
  
  
  sps_test_data_1 <- sps_test_data[, -c("layer")]
  dim(sps_test_data_1)
  
  
  # Creating arrays
  sps_train_array_x <- array(as.matrix(sps_train_data_1), dim = c(nrow(sps_train_data_1), ncol(sps_train_data_1), 1))
  dim(sps_train_array_x)
  
  #linaria_train_array_y <- to_categorical(as.matrix(linaria_train[, 1]))
  sps_train_array_y <- as.matrix(sps_train_labels)
  dim(sps_train_array_y)
  head(sps_train_array_y)
  tail(sps_train_array_y)
  
  
  sps_test_array_x <- array(as.matrix(sps_test_data_1), dim = c(nrow(sps_test_data_1), ncol(sps_test_data_1), 1))
  dim(sps_test_array_x)
  #linaria_test_array_y <- to_categorical(as.matrix(linaria_test[, 1]))
  sps_test_array_y <- as.matrix(sps_test_data[, "layer", drop = FALSE])
  dim(sps_test_array_y)
  
  
  # We have to create an array where for each pixel (pres and abs) I have a tensor of, say, 3x3 pixels around it
  # Then, dim of this array will be 'number of pixels', 3, 3, 'number of variables'
  # This array will be 'x', which will be divided into 'x_train' and 'x_test' (e.g. 70/30)
  # Finally, I will have another array for labels with dim equal to 'number of pixels' and 1 (one column per species with 1s and 0s for pres and absences, respectivelly)
  
  # build a model
  if(exists("model_c")) rm(model); gc()
  
  inpt_shpe <- length(sps_test_data_1)  # input_shape = c(ncol(dataTrain_x), nrow(dataTrain_x))) 
  
  model_c <- keras_model_sequential()
  
  model_c %>% 
    layer_conv_1d(filters = 9, kernel_size = 3, padding = "valid", input_shape = c(inpt_shpe, 1), activation = "relu",
                  kernel_regularizer = regularizer_l2(reg_L2)
                  ) %>%
    layer_max_pooling_1d() %>%
    layer_conv_1d(filters = 18, kernel_size = 3, padding = "valid", activation = "relu",
                  kernel_regularizer = regularizer_l2(reg_L2)
                  ) %>%
    layer_max_pooling_1d() %>%
    layer_conv_1d(filters = 64, kernel_size = 5, padding = "causal", activation = "relu",
                  kernel_regularizer = regularizer_l2(reg_L2)
                  ) %>%
    layer_max_pooling_1d() %>%
    layer_batch_normalization() %>%
    layer_flatten() %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  #model_c %>% 
  #  layer_conv_1d(filters = 64, kernel_size = 3, padding = "valid", input_shape = c(inpt_shpe, 1), activation = "relu") %>%
  #  #layer_max_pooling_1d() %>%
  #  layer_average_pooling_1d() %>%
  #  layer_conv_1d(filters = 128, kernel_size = 5, padding = "valid", input_shape = c(inpt_shpe, 1), activation = "relu") %>%
  #  layer_average_pooling_1d() %>%
  #  layer_flatten() %>%
  #  layer_dense(units = 1, activation = "sigmoid")
  
  
  # Why to use Pooling Layers?
  # Pooling layers are used to reduce the dimensions of the feature maps. Thus, it reduces the 
  # number of parameters to learn and the amount of computation performed in the network. The 
  # pooling layer summarises the features present in a region of the feature map generated by 
  # a convolution layer. So, further operations are performed on summarised features instead 
  # of precisely positioned features generated by the convolution layer. This makes the model 
  # more robust to variations in the position of the features in the input image. 
  
  # padding = "valid" means no padding (seems to give slightly better results than "same" and "causal")
  #           "same" results in padding the input such that the output has the same length as the original input. 
  #           "causal" results in causal (dilated) convolutions, e.g. output[t] does not depend on input[t+1:]. Useful when modeling temporal data where the model should not violate the temporal order
  
  # filters: the dimensionality of the output space (i.e. the number of output filters in the convolution).
  #          https://towardsdatascience.com/a-beginners-guide-to-convolutional-neural-networks-cnns-14649dbddce8
  #          Usually the smaller the better, and prefearible odd numbers. 3x3 convolution filters work in 
  #          general, and is often the popular choice!
  
  # https://smltar.com/dlcnn.html
  # Having a small kernel size in the first layer will let the model detect low-level features locally.
  # Larger kernels learn larger and less frequent patterns, while smaller kernels will find fine-grained features.
  
  # layer_max_pooling_1d() between the convolutional layers. This layer performs a pooling operation that 
  # calculates the maximum values in its pooling window; in this model, that is set to 2. This is done in the 
  # hope that the pooled features will be able to perform better by weeding out the small weights.
  
  # stride: The step size as the filter slides across the image
  
  model_c %>% 
    compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_sgd(learning_rate = 1e-5, momentum = 0.9),
      metrics = c(tensorflow::tf$keras$metrics$AUC(), metric_binary_accuracy)
    )
  
  
  model_c %>% fit(sps_train_array_x, sps_train_array_y, 
                  batch_size = 128, 
                  epochs = 100, 
                  verbose = 0)
  
  save_model_hdf5(model_c, 
                  filepath = paste0(dir2save_cnn, "model_CNN.h5"), 
                  overwrite = TRUE, 
                  include_optimizer = TRUE)
  
  
  score_cnn_train <- model_c %>% evaluate(sps_train_array_x, sps_train_array_y, batch_size = 128)
  score_cnn_train
  
  score_cnn_test <- model_c %>% evaluate(sps_test_array_x, sps_test_array_y, batch_size = 128)
  score_cnn_test
  
  
  score_cnn_test #for test data
  #       loss           auc    binary_accuracy 
  # 0.01756423      0.99896616      0.99355167 
  
  
  # for training data (score_train)
  #       loss            auc    binary_accuracy 
  # 0.02106769      0.99900067      0.99371600
  
  
  ## Making predictions over the whole area
  nrow(worldclim_all_data)
  
  worldclim_all_data_array_x <- array(as.matrix(worldclim_all_data), dim = c(nrow(worldclim_all_data), ncol(worldclim_all_data), 1))
  dim(worldclim_all_data_array_x)
  
  
  predictions_cnn <- model_c %>% predict(worldclim_all_data_array_x, batch_size = 128)
  head(predictions_cnn)
  nrow(predictions_cnn)
  sum(predictions_cnn[, 1])
  
  length(unique(predictions_cnn[, 1]))
  range(predictions_cnn[, 1])
  max(predictions_cnn[, 1])
  summary(predictions_cnn[, 1])
  
  
  ## Mapping predictions
  worldclim_all_data1[, predictions_cnn := as.vector(predictions_cnn[, 1])]
  
  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions_cnn")], 
                               by = "raster_position", all.x = TRUE)
  
  #sps_preds_rstr <- brick(sps_preds_rstr)
  sps_preds_rstr <- setValues(sps_preds_rstr, 
                              values = worldclim_all_data0$predictions_cnn,
                              layer = 3)
  names(sps_preds_rstr) <- c("predictions_maxent", "predictions_MLP", "predictions_CNN")
  sps_preds_rstr
  
  
  ## Boyce Index
  
  sps_data_presences_test_coords
  
  BI_cnn <- ecospat::ecospat.boyce(fit = sps_preds_rstr[["predictions_CNN"]],
                                   obs = sps_data_presences_test_coords, 
                                   nclass = 0, 
                                   window.w = "default", 
                                   res = 100, 
                                   PEplot = TRUE)
  
  
  as.vector(unlist(BI_cnn[grepl("cor", names(BI_cnn))]))
  #BI_cnn$cor  # 0.89 (CNN)
  
  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr[["predictions_CNN"]], occs_i_shp), 
                                                 extract(sps_preds_rstr[["predictions_CNN"]], bckgr))) # sensitibity default 0.9
  thresholds
  #threshold2 <- as.numeric(thresholds$sensitivity)
  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
  threshold_used <- threshold2
  
  a <- c(0, threshold2, 0)
  b <- c(threshold2, 1, 1)
  thr <- rbind(a, b)
  
  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr[["predictions_CNN"]], rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
  sps_preds_rstr_pres_abs_all <- stack(sps_preds_rstr_pres_abs_all, sps_preds_rstr_pres_abs)
  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP", "Pres_Abs_CNN")
  
  
  #plot(sps_preds_rstr_pres_abs)
  
  pdf(paste0(dir2save_cnn, "sps_predictions_CNN_", t, ".pdf"), width = 18, height = 15)
  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
  par(mfrow = c(2, 2))
  plot(sps_preds_rstr[["predictions_CNN"]], zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr[["predictions_CNN"]], zlim = c(0, 1), main = "CNN predictions", cex.main = 2, cex.sub = 1.5)
  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", 
       sub = paste0("Threshold: '", threshold2use, "'"), 
       cex.main = 2, cex.sub = 1.5, legend = FALSE)
  title(list(paste0(sps),
             cex = 4), 
        line = 1, outer = TRUE)
  
  dev.off()
  
  ## saving results
  running_time <- as.vector(Sys.time() - t0)
  if(exists("data2save")) rm(data2save)
  data2save <- (data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
                           occurrences_test, background_points,
                           auc.train = as.vector(score_cnn_train[grepl("auc", names(score_cnn_train))]),
                           auc.val = as.vector(score_cnn_test[grepl("auc", names(score_cnn_test))]),
                           #cbi.val = BI_cnn$cor,
                           cbi.val = as.vector(unlist(BI_cnn[grepl("cor", names(BI_cnn))])),
                           thresholds, threshold_used))
  rownames(data2save) <- t
  
  info_models_cnn <- rbind(info_models_cnn, data2save)
  write.csv(info_models_cnn, "info_models_CNN_all.csv", row.names = FALSE)
  write.csv(info_models_cnn, paste0(dir2save_cnn, "info_models_CNN_all.csv"), row.names = FALSE)
  #info_models_cnn <- fread("info_models_CNN_all.csv")
  #info_models_cnn <- info_models_cnn[-3, ]
  
  
  writeRaster(sps_preds_rstr_pres_abs_all, paste0(dir2save_cnn, "sps_preds_rstr_pres_abs_all.tif"), overwrite = TRUE)
  writeRaster(sps_preds_rstr, paste0(dir2save_cnn, "sps_preds_rstr.tif"), overwrite = TRUE)
  
  print(paste0(t, " run in: ", running_time))
  print(Sys.time())
  
  
  
  
  ## CNN tensors ####
  
  ## Extracting "tensors" from variables around the presences and absences
  # Following Botella's approach (?), each point (presence and absence) can be associated to a 
  # matrix of pixels surrounding the point (e.g. 3x3 or 5x5). This is to capture the influence of 
  # the "micro-habitat" of the plant.
  # I have to create an array where for each pixel (pres and abs) I have a tensor of, say, 3x3 pixels around it
  # Then, dim of this array will be 'number of pixels', 3, 3, 'number of variables'
  
#  library("keras")
#  
#  #print(t)
#  t0 <- Sys.time()
#  sps <- spcies[spcies$taxons == t, "sps"]
#  
#  print(paste0("running CNN with tensors for... ", sps))
#  
#  dir2save_cnnt <- paste0("models_CNNt_", t, "/")
#  if(!dir.exists(paste0("models_CNNt_", t))) {
#    dir.create(dir2save_cnnt)
#  }
#  
#  ## for presences:
#  #Which(sps_pres_rstr == 1, cells = TRUE)
#  matrix_dim <- 3
#  #matrix_dim <- 5
#  adj_cells <- matrix(1, nrow = matrix_dim, ncol = matrix_dim) # 49 pixels
#  adj_cells[ceiling(matrix_dim / 2), ceiling(matrix_dim / 2)] <- 0
#  #View(adj_cells)
#  
#  x <- adjacent(occs_i_rstr, 
#                cells = Which(occs_i_rstr == 1, cells = TRUE),
#                #directions = 8, # 8 for a matrix 3x3; 16 for a matrix 5x5
#                directions = adj_cells, # 7x7
#                pairs = TRUE, 
#                #target = NULL, 
#                sorted = TRUE, 
#                include = TRUE, 
#                id = FALSE)
#  
#  head(as.data.frame(x), 20)
#  length(unique(as.data.frame(x)$from))
#  
#  sps_pres_neighb <- as.data.frame(x)
#  head(sps_pres_neighb)
#  
#  sps_pres_tensor <- worldclim_all[sps_pres_neighb$to]
#  sps_pres_tensor <- cbind(sps_pres_neighb, sps_pres_tensor)
#  head(sps_pres_tensor, 20)
#  #View(sps_pres_tensor)
#  nrow(sps_pres_tensor)
#  
#  
#  # Creating an array (number_presences x 3 x 3 x number_variables)
#  length(unique(sps_pres_tensor$from))
#  
#  sps_pres_tensor_grouped <- sps_pres_tensor %>% group_by(from) %>% group_split()
#  #sps_pres_tensor_grouped
#  length(sps_pres_tensor_grouped)
#  
#  #sps_pres_tensor_grouped[[2]]
#  sps_pres_tensor_grouped[[1]]
#  
#  
#  
#  sps_pres_tensor_array <- aperm(array(unlist(sps_pres_tensor_grouped[[1]][, 3:length(sps_pres_tensor)]), 
#                                       dim = c(matrix_dim, matrix_dim, (length(sps_pres_tensor) - 2))),
#                                 c(2, 1, 3))
#  
#  
#  for(i in 2:length(sps_pres_tensor_grouped)){
#    sps_pres_tensor_array2 <- aperm(array(unlist(sps_pres_tensor_grouped[[i]][, 3:length(sps_pres_tensor)]), 
#                                          dim = c(matrix_dim, matrix_dim, (length(sps_pres_tensor) - 2))), 
#                                    c(2, 1, 3))
#    sps_pres_tensor_array <- array(c(sps_pres_tensor_array, sps_pres_tensor_array2), 
#                                   dim = c(matrix_dim, matrix_dim, dim(sps_pres_tensor_array)[3], i))
#  }
#  
#  
#  sps_pres_tensor_array <- aperm(sps_pres_tensor_array, c(4, 1, 2, 3))
#  dim(sps_pres_tensor_array)
#  sps_pres_tensor_array[1, , ,1]
#  #sps_pres_tensor_array[734, , ,1]
#  sps_pres_tensor_array[5, , ,1]
#  sps_pres_tensor_array2[,,1]
#  
#  
#  
#  ## for absences:
#  #Which(sps_pres_rstr == 0, cells = TRUE)
#  #length(Which(sps_pres_rstr == 0, cells = TRUE))
#  #ncell(sps_pres_rstr)
#  
#  # Selecting a subset of pseudo-absences for training
#  
#  pseudoAbs_train_cells <- sps_data_absences$cells
#  head(sort(pseudoAbs_train_cells))
#  
#  y <- adjacent(occs_i_rstr, 
#                cells = pseudoAbs_train_cells,
#                directions = adj_cells, # 8 for a matrix 3x3; 16 for a matrix 5x5
#                pairs = TRUE, 
#                #target = NULL, 
#                sorted = TRUE, 
#                include = TRUE, 
#                id = FALSE)
#  
#  head(as.data.frame(y), 20)
#  length(unique(as.data.frame(y)$from))
#  
#  sps_abs_neighb <- as.data.frame(y)
#  head(sps_abs_neighb, 30)
#  
#  sps_abs_tensor <- worldclim_all[sps_abs_neighb$to]
#  sps_abs_tensor <- cbind(sps_abs_neighb, sps_abs_tensor)
#  head(sps_abs_tensor, 20)
#  
#  
#  # Creating an array (number_absences x 3 x 3 x number_variables)
#  length(unique(sps_abs_tensor$from))
#  
#  sps_abs_tensor_grouped <- sps_abs_tensor %>% group_by(from) %>% group_split()
#  #sps_abs_tensor_grouped
#  length(sps_abs_tensor_grouped)
#  
#  #sps_abs_tensor_grouped[[700]]
#  sps_abs_tensor_grouped[[1]]
#  
#  
#  
#  sps_abs_tensor_array <- aperm(array(unlist(sps_abs_tensor_grouped[[1]][, 3:length(sps_abs_tensor)]), 
#                                      dim = c(matrix_dim, matrix_dim, (length(sps_abs_tensor) - 2))),
#                                c(2, 1, 3))
#  
#  for(i in 2:length(sps_abs_tensor_grouped)){
#    sps_abs_tensor_array2 <- aperm(array(unlist(sps_abs_tensor_grouped[[i]][, 3:length(sps_abs_tensor)]), 
#                                         dim = c(matrix_dim, matrix_dim, (length(sps_abs_tensor) - 2))), 
#                                   c(2, 1, 3))
#    sps_abs_tensor_array <- array(c(sps_abs_tensor_array, sps_abs_tensor_array2), 
#                                  dim = c(matrix_dim, matrix_dim, dim(sps_abs_tensor_array)[3], i))
#  }
#  
#  
#  sps_abs_tensor_array <- aperm(sps_abs_tensor_array, c(4, 1, 2, 3))
#  dim(sps_abs_tensor_array)
#  sps_abs_tensor_array[1, , ,]
#  sps_abs_tensor_array[1, , ,1]
#  
#  
#  ## Selecting a subset of pseudo-absences for testing
#  #pseudoAbs_test_cells <- sample(Which(sps_pres_rstr == 0, cells = TRUE), num_PeudoAbs)
#  #head(sort(pseudoAbs_test_cells))
#  #head(sort(pseudoAbs_train_cells))
#  #sum(pseudoAbs_test_cells %in% pseudoAbs_train_cells) # number of pseudo-abs which are selected both for train and test
#  ## it should tend to 0
#  
#  #y1 <- adjacent(sps_pres_rstr, 
#  #               cells = pseudoAbs_test_cells,
#  #               directions = adj_cells, # 8 for a matrix 3x3; 16 for a matrix 5x5
#  #               pairs = TRUE, 
#  #               #target = NULL, 
#  #               sorted = TRUE, 
#  #               include = TRUE, 
#  #               id = FALSE)
#  #
#  #head(as.data.frame(y1), 20)
#  #length(unique(as.data.frame(y1)$from))
#  #
#  #sps_abs_neighb_test <- as.data.frame(y1)
#  #head(sps_abs_neighb_test, 30)
#  #
#  #sps_abs_tensor_test <- worldclim_all[sps_abs_neighb_test$to]
#  #sps_abs_tensor_test <- cbind(sps_abs_neighb_test, sps_abs_tensor_test)
#  #head(sps_abs_tensor_test, 20)
#  #
#  #
#  ## Creating an array (number_presences x 3 x 3 x number_variables)
#  #length(unique(sps_abs_tensor_test$from))
#  #
#  #sps_abs_tensor_test_grouped <- sps_abs_tensor_test %>% group_by(from) %>% group_split()
#  #sps_abs_tensor_test_grouped
#  #length(sps_abs_tensor_test_grouped)
#  #
#  #sps_abs_tensor_test_grouped[[700]]
#  #sps_abs_tensor_test_grouped[[1]]
#  #
#  #
#  #
#  #sps_abs_tensor_array_test <- aperm(array(unlist(sps_abs_tensor_test_grouped[[1]][, 3:length(sps_abs_tensor_test)]), 
#  #                                             dim = c(matrix_dim, matrix_dim, (length(sps_abs_tensor_test) - 2))),
#  #                                       c(2, 1, 3))
#  #
#  #for(i in 2:length(sps_abs_tensor_test_grouped)){
#  #  sps_abs_tensor_array_test2 <- aperm(array(unlist(sps_abs_tensor_test_grouped[[i]][, 3:length(sps_abs_tensor_test)]), 
#  #                                                dim = c(matrix_dim, matrix_dim, (length(sps_abs_tensor_test) - 2))), 
#  #                                          c(2, 1, 3))
#  #  sps_abs_tensor_array_test <- array(c(sps_abs_tensor_array_test, sps_abs_tensor_array_test2), 
#  #                                         dim = c(matrix_dim, matrix_dim, dim(sps_abs_tensor_array_test)[3], i))
#  #}
#  #
#  #
#  #sps_abs_tensor_array_test <- aperm(sps_abs_tensor_array_test, c(4, 1, 2, 3))
#  #dim(sps_abs_tensor_array_test)
#  ##sps_abs_tensor_array_test[1, , ,]
#  #sps_abs_tensor_array_test[1, , ,1]
#  
#  
#  ## Let's subset data set for training the model (we set aside 20% of presences for validation)
#  
#  pres_pos_1 <- data.table(sps_data_presences_train$raster_position, rep("train", length(sps_data_presences_train$raster_position)))
#  pres_pos <- data.table(Which(occs_i_rstr == 1, cells = TRUE), 1:length(Which(occs_i_rstr == 1, cells = TRUE)))
#  pres_pos <- merge(pres_pos, pres_pos_1, by = "V1", all.x = TRUE)
#  pres_pos$V2.y[is.na(pres_pos$V2.y)] <- "test"
#  table(pres_pos$V2.y)
#  pres_pos
#  
#  pres_train <- as.vector(unlist(pres_pos[pres_pos$V2.y == "train", "V2.x"]))
#  pres_test <- as.vector(unlist(pres_pos[pres_pos$V2.y == "test", "V2.x"]))
#  
#  sps_train_array_x <- sps_pres_tensor_array[pres_train, , , , drop = FALSE]
#  dim(sps_train_array_x)
#  sps_train_array_x[2,,,]
#  
#  sps_test_array_x <- sps_pres_tensor_array[pres_test, , , , drop = FALSE]
#  dim(sps_test_array_x)
#  sps_test_array_x[2,,,]
#  sum(is.na(sps_test_array_x[2,,,]))
#  mean(sps_test_array_x[2,,,20])
#  
#  
#  # labels
#  sps_train_array_y <- matrix(1, nrow = dim(sps_train_array_x)[1], ncol = 1)
#  dim(sps_train_array_y)
#  head(sps_train_array_y)
#  
#  sps_abs_tensor_array_y <- matrix(0, nrow = dim(sps_abs_tensor_array)[1], ncol = 1)
#  dim(sps_abs_tensor_array_y)
#  head(sps_abs_tensor_array_y)
#  
#  sps_test_array_y <- matrix(1, nrow = dim(sps_test_array_x)[1], ncol = 1)
#  dim(sps_test_array_y)
#  head(sps_test_array_y)
#  
#  #sps_abs_tensor_array_test_y <- matrix(0, nrow = dim(sps_abs_tensor_array_test)[1], ncol = 1)
#  #dim(sps_abs_tensor_array_test_y)
#  #head(sps_abs_tensor_array_test_y)
#  
#  library(abind)
#  dim(sps_train_array_x)
#  dim(sps_abs_tensor_array)
#  sps_train_array_all_x <- abind(sps_train_array_x, sps_abs_tensor_array, along = 1)
#  dim(sps_train_array_all_x)
#  
#  head(sps_train_array_y)
#  dim(sps_train_array_y)
#  head(sps_abs_tensor_array_y)
#  dim(sps_abs_tensor_array_y)
#  sps_train_array_all_y <- abind(sps_train_array_y, sps_abs_tensor_array_y, along = 1)
#  dim(sps_train_array_all_y)
#  
#  
#  dim(sps_test_array_x)
#  #dim(sps_abs_tensor_array_test)
#  #sps_test_array_all_x <- abind(sps_test_array_x, sps_abs_tensor_array_test, along = 1)
#  sps_test_array_all_x <- abind(sps_test_array_x, sps_abs_tensor_array, along = 1)
#  dim(sps_test_array_all_x)
#  sum(is.na(sps_test_array_all_x))
#  
#  dim(sps_test_array_y)
#  #dim(sps_abs_tensor_array_test_y)
#  #sps_test_array_all_y <- abind(sps_test_array_y, sps_abs_tensor_array_test_y, along = 1)
#  sps_test_array_all_y <- abind(sps_test_array_y, sps_abs_tensor_array_y, along = 1)
#  dim(sps_test_array_all_y)
#  
#  
#  ## Building the model
#  inpt_shpe <- dim(sps_pres_tensor_array)[2:4]
#  #inpt_shpe <- c(5,5,27)
#  if(exists("model_ct")) {rm(model_ct); gc()}
#  
#  model_ct <- keras_model_sequential()
#  
#  model_ct %>% 
#    layer_conv_2d(filters = 32, kernel_size = c(3, 3), data_format = "channels_last", padding = "valid", 
#                  input_shape = inpt_shpe, activation = "relu",
#                  kernel_regularizer = regularizer_l2(reg_L2)
#                  ) %>%
#    layer_conv_2d(filters = 9, kernel_size = c(1, 1), data_format = "channels_last", padding = "valid", activation = "relu",
#                  kernel_regularizer = regularizer_l2(reg_L2)
#                  ) %>%
#    #layer_average_pooling_2d(pool_size = c(2, 2)) %>%
#    #layer_batch_normalization() %>%
#    #
#    #
#    #layer_dropout(0.25) %>%
#    layer_flatten() %>%
#    layer_dense(units = 9, activation = "relu") %>%
#    layer_batch_normalization() %>%
#    layer_dense(units = 1, activation = "sigmoid")
#  
#  
#  
#  # Batch normalization helps models learn and generalize better on new data. The most common methods for batch normalization 
#  # consists of centering the data on zero by subtracting the mean from the data and changing standard deviation of data 
#  # to 1 by dif=viding the data by ots standard deviation.
#  # Batch normalization should be implemented after every transofmation operated by the network. 
#  # It is commonly used after a convolution or densly connected layers
#  
#  # (to reduce overfitting) Dropout consists on randomly setting to zero a number of output features of a layer during the training. The idea behind 
#  # dropout technique is to introduce noise in the output values of a layer in order to remove non significant features. The 
#  # dropout rate is the fraction of the features that are set to zero. It is usually between 0.2 and 0.5. During the test phase, 
#  # no units are dopped out but the layer’s output values are scaled down by a factor equal to the dropout rate to have same number of units active.
#  
#  # (to reduce overfitting) L2 regularization (weight decay): The cost added is proportional to the square of the value of the weight coefficients.
#  # regularizer_l2(0.001) means that every coefficient in the weight matrix of the layer will add 0.001*weight_coefficient_value to the total loss 
#  # of the network. Since the penalty is only added at training phase, the loss at the training phase will be much higher than the loss in the test phase.
#  
#  # Why to use Pooling Layers?
#  # Pooling layers are used to reduce the dimensions of the feature maps. Thus, it reduces the 
#  # number of parameters to learn and the amount of computation performed in the network. The 
#  # pooling layer summarises the features present in a region of the feature map generated by 
#  # a convolution layer. So, further operations are performed on summarised features instead 
#  # of precisely positioned features generated by the convolution layer. This makes the model 
#  # more robust to variations in the position of the features in the input image. 
#  
#  # padding = "valid" means no padding (seems to give slightly better results than "same" and "causal")
#  
#  # filters: the dimensionality of the output space (i.e. the number of output filters in the convolution).
#  #          https://towardsdatascience.com/a-beginners-guide-to-convolutional-neural-networks-cnns-14649dbddce8
#  #          Usually the smaller the better, and prefearible odd numbers. 3x3 convolution filters work in 
#  #          general, and is often the popular choice!
#  
#  # https://smltar.com/dlcnn.html
#  # Having a small kernel size in the first layer will let the model detect low-level features locally.
#  # Larger kernels learn larger and less frequent patterns, while smaller kernels will find fine-grained features.
#  
#  # layer_max_pooling_1d() between the convolutional layers. This layer performs a pooling operation that 
#  # calculates the maximum values in its pooling window; in this model, that is set to 2. This is done in the 
#  # hope that the pooled features will be able to perform better by weeding out the small weights.
#  
#  # stride: The step size as the filter slides across the image
#  
#  
#  model_ct %>% 
#    compile(
#      loss = 'binary_crossentropy',
#      optimizer = optimizer_sgd(learning_rate = 1e-5, momentum = 0.9),
#      metrics = c(tensorflow::tf$keras$metrics$AUC(), metric_binary_accuracy) # AUC calculation doesn't work if there are NAs in the tensors
#    )
#  
#  # Removing NAs from tensors (otherwise, AUC gives error and the model can't be fitted)
#  # Not only AUC, but the model itself is not fitted, so nothing works afterwards (i.e. predictions...) if there are NAs in the tensors
#  sps_train_array_all_x[is.na(sps_train_array_all_x)] <- 0 
#  sum(is.na(sps_train_array_all_x))
#  
#  model_ct %>% fit(sps_train_array_all_x, sps_train_array_all_y,
#                   batch_size = 128, 
#                   epochs = 200, 
#                   verbose = 2)
#  
#  save_model_hdf5(model_ct, 
#                  filepath = paste0(dir2save_cnnt, "model_CNN_T.h5"), 
#                  overwrite = TRUE, 
#                  include_optimizer = TRUE)
#  
#  
#  score_cnnt_train <- model_ct %>% evaluate(sps_train_array_all_x, sps_train_array_all_y, batch_size = 128)
#  score_cnnt_train
#  #          loss           auc   binary_accuracy 
#  #    0.00704888      0.99965131      0.99851102
#  
#  sps_test_array_all_x[is.na(sps_test_array_all_x)] <- 0 
#  sum(is.na(sps_test_array_all_x))
#  score_cnnt_test <- model_ct %>% evaluate(sps_test_array_all_x, sps_test_array_all_y, batch_size = 128)
#  score_cnnt_test #for test data
#  #         loss             auc binary_accuracy 
#  #   0.006295961     0.984028041     0.998903394 
#  
#  
#  # The gap between training accuracy and test accuracy is an example of "overfitting" 
#  # train_auc () < test_auc ()  --> No overfitting
#  # train_accuracy () < test_accuracy ()  --> No overfitting
#  # Overfitting is when a machine learning model performs worse on new data than on their training data
#  
#  
#  ## Make predictions over the whole extent
#  
#  #worldclim_all_data <- as.data.frame(worldclim_all)
#  #worldclim_all_data <- worldclim_all_data[complete.cases(worldclim_all_data), ]
#  #head(worldclim_all_data)
#  #nrow(worldclim_all_data)
#  
#  #dim(sps_train_array_x)
#  #dim(as.matrix(worldclim_all_data))
#  
#  worldclim_all
#  if(exists("sps_data")) rm(sps_data); gc()
#  if(exists("sps_abs_tensor_grouped")) rm(sps_abs_tensor_grouped); gc()
#  
#  if(!file.exists(paste0(preds_dir, "worldclim_all_tensor_array_3x3.rds"))){
#    x1 <- adjacent(worldclim_all, 
#                   cells = Which(!is.na(occs_i_rstr), cells = TRUE),
#                   directions = adj_cells, # 8 for a matrix 3x3; 16 for a matrix 5x5
#                   pairs = TRUE, 
#                   #target = NULL, 
#                   sorted = TRUE, 
#                   include = TRUE, 
#                   id = FALSE)
#    
#    head(as.data.frame(x1), 20)
#    length(unique(as.data.frame(x1)$from))
#    length(unique(as.data.frame(x1)$to))
#    
#    worldclim_all_neighb <- as.data.frame(x1)
#    head(worldclim_all_neighb, 10)
#    
#    worldclim_all_tensor <- worldclim_all[worldclim_all_neighb$to]
#    worldclim_all_tensor <- cbind(worldclim_all_neighb, worldclim_all_tensor)
#    
#    head(worldclim_all_tensor, 20)
#    #View(worldclim_all_tensor)
#    
#    
#    # Creating an array (number_presences x 3 x 3 x number_variables)
#    length(unique(worldclim_all_tensor$from))
#    
#    worldclim_all_tensor_grouped <- worldclim_all_tensor %>% group_by(from) %>% group_split()
#    #worldclim_all_tensor_grouped
#    length(worldclim_all_tensor_grouped)
#    
#    worldclim_all_tensor_grouped[[700]]
#    worldclim_all_tensor_grouped[[1]]
#    
#    #worldclim_all_tensor_array <- aperm(array(unlist(worldclim_all_tensor_grouped[[1]][, 3:length(worldclim_all_tensor)]), 
#    #                                          dim = c(matrix_dim, matrix_dim, (length(worldclim_all_tensor) - 2))),
#    #                                    c(2, 1, 3))
#    
#    
#    
#    worldclim_all_tensor_array <- array(NA, dim = c(matrix_dim, matrix_dim, (length(worldclim_all_tensor) - 2), length(worldclim_all_tensor_grouped)))
#    dim(worldclim_all_tensor_array)
#    
#    for(i in 1:length(worldclim_all_tensor_grouped)){
#      cat("\r", paste0(i, "/", length(worldclim_all_tensor_grouped)))
#      worldclim_all_tensor_array2 <- aperm(array(unlist(worldclim_all_tensor_grouped[[i]][, 3:length(worldclim_all_tensor)]), 
#                                                 dim = c(matrix_dim, matrix_dim, (length(worldclim_all_tensor) - 2))), 
#                                           c(2, 1, 3))
#      worldclim_all_tensor_array[, , , i] <- worldclim_all_tensor_array2
#    }
#    dim(worldclim_all_tensor_array)
#    
#    #for(i in 2:length(worldclim_all_tensor_grouped)){
#    #  cat("\r", paste0(i, "/", length(worldclim_all_tensor_grouped)))
#    #  worldclim_all_tensor_array2 <- aperm(array(unlist(worldclim_all_tensor_grouped[[i]][, 3:length(worldclim_all_tensor)]), 
#    #                                             dim = c(matrix_dim, matrix_dim, (length(worldclim_all_tensor) - 2))), 
#    #                                       c(2, 1, 3))
#    #  worldclim_all_tensor_array <- array(c(worldclim_all_tensor_array, worldclim_all_tensor_array2), 
#    #                                      dim = c(matrix_dim, matrix_dim, dim(worldclim_all_tensor_array)[3], i))
#    #}
#    
#    worldclim_all_tensor_array <- aperm(worldclim_all_tensor_array, c(4, 1, 2, 3))
#    dim(worldclim_all_tensor_array)
#    worldclim_all_tensor_array[1, , ,1]
#    #worldclim_all_tensor_array[720399, , ,1]
#    #worldclim_all_tensor_array[734, , ,1]
#    worldclim_all_tensor_array2[,,1]
#    saveRDS(worldclim_all_tensor_array, file = paste0(preds_dir, "worldclim_all_tensor_array_3x3.rds"))
#    
#  } else {
#    worldclim_all_tensor_array <- readRDS(paste0(preds_dir, "worldclim_all_tensor_array_3x3.rds"))
#  }
#  
#  worldclim_all_tensor_array[is.na(worldclim_all_tensor_array)] <- 0   # to remove NAs, otherwise predictions = NaN
#  #sum(is.na(worldclim_all_tensor_array))
#  dim(worldclim_all_tensor_array)
#  
#  
#  predictions_cnnt <- model_ct %>% predict(worldclim_all_tensor_array, batch_size = 128)
#  head(predictions_cnnt)
#  nrow(predictions_cnnt)
#  sum(predictions_cnnt[, 1])
#  sum(is.na(predictions_cnnt[, 1]))
#  
#  length(unique(predictions_cnnt[, 1]))
#  range(predictions_cnnt[, 1], na.rm = TRUE)
#  max(predictions_cnnt[, 1])
#  summary(predictions_cnnt[, 1])
#  
#  sum(!is.na(predictions_cnnt[, 1]))
#  sum(is.na(predictions_cnnt[, 1])) + sum(!is.na(predictions_cnnt[, 1])) == nrow(predictions_cnnt)
#  
#  
#  ## Mapping predictions
#  worldclim_all_data1[, predictions_cnnt := as.vector(predictions_cnnt[, 1])]
#  #worldclim_all_data1$predictions_cnnt <- as.vector(predictions_cnnt[, 1])
#  sum(is.na(worldclim_all_data1$predictions_cnnt))
#  
#  worldclim_all_data0 <- merge(worldclim_all_data0[, "raster_position", with = FALSE], 
#                               worldclim_all_data1[, .SD, .SDcols = c("raster_position", "predictions_cnnt")], 
#                               by = "raster_position", all.x = TRUE)
#  
#  #sps_preds_rstr <- brick(sps_preds_rstr)
#  sps_preds_rstr <- setValues(sps_preds_rstr, 
#                              values = worldclim_all_data0$predictions_cnnt,
#                              layer = 4)
#  
#  #names(sps_preds_rstr) <- c("predictions_CNN_T")
#  names(sps_preds_rstr) <- c("predictions_maxent", "predictions_MLP", "predictions_CNN", "predictions_CNN_T")
#  sps_preds_rstr
#  
#  
#  ## Boyce Index
#  sps_data_presences_test_coords
#  
#  BI_cnnt <- ecospat::ecospat.boyce(fit = sps_preds_rstr[["predictions_CNN_T"]],
#                                    obs = sps_data_presences_test_coords, 
#                                    rm.duplicate = TRUE,
#                                    nclass = 0, 
#                                    window.w = "default", 
#                                    res = 100, 
#                                    PEplot = TRUE)
#  
#  
#  as.vector(unlist(BI_cnnt[grepl("cor", names(BI_cnnt))]))
#  #BI_cnnt$cor  # 0.89 (CNN)
#  
#  thresholds <- dismo::threshold(dismo::evaluate(extract(sps_preds_rstr[["predictions_CNN_T"]], occs_i_shp), 
#                                                 extract(sps_preds_rstr[["predictions_CNN_T"]], bckgr))
#                                 #, stat = "", sensitivity = 0.999) 
#  )   # sensitibity default 0.9
#  thresholds
#  #threshold2 <- as.numeric(thresholds$sensitivity)
#  #threshold2 <- as.numeric(thresholds$no_omission) # keeping all presences
#  threshold2 <- as.numeric(thresholds[names(thresholds) %in% threshold2use])
#  if(threshold2 > 1) threshold2 <- 1
#  threshold_used <- threshold2
#  
#  a <- c(0, threshold2, 0)
#  b <- c(threshold2, 1, 1)
#  thr <- rbind(a, b)
#  
#  sps_preds_rstr_pres_abs <- reclassify(sps_preds_rstr[["predictions_CNN_T"]], rcl = thr, filename = '', include.lowest = FALSE, right = TRUE)
#  sps_preds_rstr_pres_abs_all <- stack(sps_preds_rstr_pres_abs_all, sps_preds_rstr_pres_abs)
#  #names(sps_preds_rstr_pres_abs_all)[2] <- c("Pres_Abs_CNN_T")
#  #sps_preds_rstr_pres_abs_all <- sps_preds_rstr_pres_abs_all[[c(1:3, 5)]]
#  names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP", "Pres_Abs_CNN", "Pres_Abs_CNN_T")
#  
#  
#  ## Plotting predictions
#  pdf(paste0(dir2save_cnnt, "sps_predictions_CNNtensors_", t, ".pdf"), width = 18, height = 15)
#  par(mar = c(6, 8, 6, 8), oma = c(4,0,8,0))
#  par(mfrow = c(2, 2))
#  plot(sps_preds_rstr[["predictions_CNN_T"]], zlim = c(0, 1), main = "Occurrences (1km)", cex.main = 2, cex.sub = 1.5, legend = FALSE)
#  plot(occs_i_shp, add = TRUE, col = "black")
#  plot(sps_preds_rstr[["predictions_CNN_T"]], zlim = c(0, 1), main = "CNN-tensors predictions", cex.main = 2, cex.sub = 1.5)
#  plot(sps_preds_rstr_pres_abs, main = "Presence-Absence", #col = c("green", "grey"), 
#       sub = paste0("Threshold: '", threshold2use, "'"), 
#       cex.main = 2, cex.sub = 1.5, legend = FALSE)
#  title(list(paste0(sps),
#             cex = 4), 
#        line = 1, outer = TRUE)
#  
#  dev.off()
#  
#  
#  
#  ## saving results
#  running_time <- as.vector(Sys.time() - t0)
#  if(exists("data2save")) rm(data2save)
#  data2save <- (data.frame(species = t, occurrences_raw, occurrences_1km, occurrences_train,
#                           occurrences_test, background_points,
#                           auc.train = as.vector(score_cnnt_train[grepl("auc", names(score_cnnt_train))]),
#                           auc.val = as.vector(score_cnnt_test[grepl("auc", names(score_cnnt_test))]),
#                           #cbi.val = BI_cnnt$cor,
#                           cbi.val = as.vector(unlist(BI_cnnt[grepl("cor", names(BI_cnnt))])),
#                           thresholds, threshold_used))
#  rownames(data2save) <- t
#  
#  info_models_cnnt <- rbind(info_models_cnnt, data2save)
#  write.csv(info_models_cnnt, "info_models_CNNtensors_all.csv", row.names = FALSE)
#  write.csv(info_models_cnnt, paste0(dir2save_cnnt, "info_models_CNNtensors_all.csv"), row.names = FALSE)
#  #info_models_cnnt <- fread("info_models_CNNtensors_all.csv")
#  #info_models_cnnt <- info_models_cnnt[1:8, ]
#  
#  
#  writeRaster(sps_preds_rstr_pres_abs_all, paste0(dir2save_cnnt, "sps_preds_rstr_pres_abs_all.tif"), overwrite = TRUE)
#  writeRaster(sps_preds_rstr, paste0(dir2save_cnnt, "sps_preds_rstr.tif"), overwrite = TRUE)
#  
#  print(paste0(t, " run in: ", running_time))
#  print(Sys.time())
  
  #
  
  
  ## Plotting all predictions together ####
  
  sps_preds_rstr_pres_abs_all
  #names(sps_preds_rstr_pres_abs_all) <- c("Pres_Abs_MaxEnt", "Pres_Abs_MLP", "Pres_Abs_CNN")
  
  
  pdf(paste0(dir2save_cnn, "Pres-Abs_MaxEnt_MLP_CNN_CNNt_", t, ".pdf"), width = 30, height = 20)
  par(mfrow = c(2, 3))
  par(mar = c(8, 4, 4, 5))
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_MaxEnt"]], col = "grey", 
       main = paste0("Simulated species: ", t), cex.main = 3.5, legend = FALSE)
  plot(occs_i_shp, add = TRUE, col = "black")
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_MaxEnt"]], zlim = c(0, 1), main = "MaxEnt", cex.main = 3.5, cex.sub = 2.5, legend = FALSE,
       sub = paste0("MaxEnt: Boyce Index = ", round(as.vector(optimal$cbi.val), 3), "; AUC = ", round(as.vector(optimal$auc.val), 3)))
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_MLP"]], zlim = c(0, 1), main = "MultiLayer Perceptrons (MLP)", cex.main = 3.5, cex.sub = 2.5, legend = FALSE,
       sub = paste0("MLP: Boyce Index = ", round(as.vector(unlist(BI_mlp[grepl("cor", names(BI_mlp))])), 3), "; AUC = ", round(score_mlp_test[grepl("auc", names(score_mlp_test))], 3)))
  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_CNN"]], zlim = c(0, 1), main = "Convolutional Neural Network (CNN)", cex.main = 3.5, cex.sub = 2.5, legend = FALSE,
       sub = paste0("CNN: Boyce Index = ", round(as.vector(unlist(BI_cnn[grepl("cor", names(BI_cnn))])), 3), "; AUC = ", round(score_cnn_test[grepl("auc", names(score_cnn_test))], 3)))
#  plot(sps_preds_rstr_pres_abs_all[["Pres_Abs_CNN_T"]], zlim = c(0, 1), main = "Convolutional Neural Network with tensors (CNN-T)", cex.main = 3.5, cex.sub = 2.5, legend = FALSE,
#       sub = paste0("CNN-T: Boyce Index = ", round(as.vector(unlist(BI_cnnt[grepl("cor", names(BI_cnnt))])), 3), "; AUC = ", round(score_cnnt_test[grepl("auc", names(score_cnnt_test))], 3)))
  dev.off()
  
  
  
  if(exists("model")) rm(model)
  if(exists("model_c")) rm(model_c)
  if(exists("model_ct")) rm(model_ct)
  
  
  #files <- list.files(tempdir(), full.names = TRUE)
  #unlink(files, recursive = TRUE); rm(files)
  #
  #files <- list.files("/scratch/rotllxa", full.names = TRUE)
  #files <- files[grepl("py", files)]
  #unlink(files, recursive = TRUE); rm(files)
  #
  #
  #detach(package:reticulate,unload=TRUE)
  #detach(package:keras,unload=TRUE)
  
  #
  
}



## Checking results ####

#info_models_maxent <- read.csv("info_models_all_species.csv", header = TRUE)
info_models_maxent <- read.csv("info_models_maxent_all.csv", header = TRUE)
View(info_models_maxent)

paste0("MaxEnt: AUC-train = ", round(mean(as.numeric(info_models_maxent$auc.train)), 3), 
       "; AUC-test = ", round(mean(as.numeric(info_models_maxent$auc.val)), 3),
       "; CBI-train = ",round(mean(as.numeric(info_models_maxent$cbi.train)), 3),
       "; CBI-test = ", round(mean(as.numeric(info_models_maxent$cbi.val)), 3))



info_models_mlp
paste0("MLP: AUC-train = ", round(mean(as.numeric(info_models_mlp$auc.train)), 3), 
       "; AUC-test = ", round(mean(as.numeric(info_models_mlp$auc.val)), 3),
       "; CBI-train = ",round(mean(as.numeric(info_models_mlp$cbi.train)), 3),
       "; CBI-test = ", round(mean(as.numeric(info_models_mlp$cbi.val)), 3))

info_models_maxent$cbi.val
info_models_mlp$cbi.val


info_models_cnn
paste0("CNN: AUC-train = ", round(mean(as.numeric(info_models_cnn$auc.train)), 3), 
       "; AUC-test = ", round(mean(as.numeric(info_models_cnn$auc.val)), 3),
       "; CBI-train = ",round(mean(as.numeric(info_models_cnn$cbi.train)), 3),
       "; CBI-test = ", round(mean(as.numeric(info_models_cnn$cbi.val)), 3))


info_models_maxent$cbi.val
info_models_mlp$cbi.val
info_models_cnn$cbi.val





info_models_all <- c()

for (t in taxons[length(taxons)]){
  dir2save <- paste0(getwd(), "/")
  
  if(dir.exists(paste0(dir2save, "models_maxent_", t))){
    dir2save 
    
    print(paste0(t, ": maxent"))
    info_models_maxent <- fread(paste0(dir2save, "models_maxent_", t, "/info_models_maxent_all.csv"), header = TRUE)
    names(info_models_maxent)
    info_maxent <- info_models_maxent[, c("species", "auc.val", "cbi.val")]
    info_maxent$algorithm <- "maxent"
    #info_maxent$species <- t
    
    print(paste0(t, ": MLP"))
    info_models_MLP <- fread(paste0(dir2save, "models_MLP_", t, "/info_models_MLP_all.csv"), header = TRUE)
    names(info_models_MLP)
    info_mlp <- info_models_MLP[, c("species", "auc.val", "cbi.val")]
    info_mlp$algorithm <- "MLP"
    #info_mlp$species <- t
    
    print(paste0(t, ": CNN"))
    info_models_CNN <- fread(paste0(dir2save, "models_CNN_", t, "/info_models_CNN_all.csv"), header = TRUE)
    names(info_models_CNN)
    info_cnn <- info_models_CNN[, c("species", "auc.val", "cbi.val")]
    #names(info_cnn)[2] <- "cbi.val"
    info_cnn$algorithm <- "CNN"
    #info_cnn$species <- t
    
    info_models_all <- rbind(info_models_all, info_maxent, info_mlp, info_cnn)
    #
    
  }
}


## Some averages

statistics <- info_models_all %>%
  group_by(algorithm) %>%
  summarise(mean_cbi = mean(cbi.val, na.rm = TRUE), mean_auc = mean(auc.val, na.rm = TRUE)) %>%
  as.data.frame()
statistics

statistics$mean_cbi <- round(statistics$mean_cbi, 3)
statistics$mean_auc <- round(statistics$mean_auc, 3)


statistics <- statistics %>% slice(match(c("maxent", "MLP", "CNN"), algorithm))
statistics

#  algorithm mean_cbi mean_auc
#     maxent    1.000    0.926
#        MLP    1.000    0.927      # MLP and MaxEnt, virtually the same performance
#        CNN    0.996    0.882


statistics_auc <- statistics[order(statistics$mean_auc, decreasing = TRUE), ]
statistics_cbi <- statistics[order(statistics$mean_cbi, decreasing = TRUE), ]


## Some plots

info_models_all

info_models_all$species <- as.factor(info_models_all$species)
str(info_models_all)
info_models_all$species <- factor(info_models_all$species, levels = as.vector(unique(info_models_all$species)))

#info_models_all <- info_models_all[with(info_models_all, order(species, algorithm)), ]
#info_models_all <- info_models_all[with(info_models_all, order(algorithm)), ]


library(lattice)
library(latticeExtra)
library(viridis)

colrs <- viridis(4)

pdf("auc_boyce_all.pdf")
xyplot(auc.val + cbi.val ~ species,
       layout=c(1, 2),
       type = "b",
       #type = "l",
       group = algorithm,
       data = info_models_all,
       ylim = c(0.5, 1.05),
       scales = list(x = list(rot = 45)),
       col = colrs,
       xlab = "Species (sorted by number of occurrences)", 
       ylab = "AUC and CBI for validation",
       par.settings = list(superpose.line = list(col = colrs)),         
       auto.key=list(space="top", 
                     points = FALSE, lines = TRUE,
                     columns = 4)) 
#graphics::text(paste0(paste(statistics$algorithm, statistics$mean_cbi, sep = " = "), collapse = "; "))
panel.text(260, 285, paste0("Averages: ", paste0(paste(statistics_cbi$algorithm, statistics_cbi$mean_cbi, sep = " = "), collapse = "; ")), cex = 0.8, font = 1)
panel.text(260, 100, paste0("Averages: ", paste0(paste(statistics_auc$algorithm, statistics_auc$mean_auc, sep = " = "), collapse = "; ")), cex = 0.8, font = 1)

dev.off()



