

###############################################
####        Downloading occurrences        ####
####        of bird species for the        ####
####        SDMs-Pesticides project        ####
###############################################

if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] == "L2100739RI") {
  wd <- "C:/Users/rotllxa/D5_FFGRCC_FarmlandBirds/"
  gbif_creds <- "C:/Users/rotllxa/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03", "jeodpp-terminal-jd002-03",
                              "jeodpp-terminal-jd004-03.cidsn.jrc.it")) {
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
library(data.table)
library(ggplot2)
library(dplyr)
library(devtools)
#install_github("xavi-rp/PreSPickR", 
#               ref = "v2", 
#               INSTALL_opts = c("--no-multiarch"))  # https://github.com/rstudio/renv/issues/162
library(PreSPickR)
library(sp)
#library(spdplyr)



list.files()
sps2dwnld_3 <- read.csv("/home/rotllxa/Documents/SDMs_Pesti/sp_list_FBI_3.csv", header = FALSE)
sps2dwnld <- read.csv("/home/rotllxa/Documents/SDMs_Pesti/sp_list_FBI_all.csv", header = FALSE)
head(sps2dwnld)
length(sps2dwnld$V2)



## Checking number of occs in GBIF (EU-27) ####

sp_1_key <- as.data.frame(name_backbone(name='Iphiclides podalirius'))$speciesKey

countr <- c("BE", "EL", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL")
countr <- sort(countr)
countr <- "FR"
length(countr)


num_eu_occs_df <- c()
count <- 1
#sp <- sps2dwnld$V2[1]

for(sp in sps2dwnld$V2){
  sp_key <- as.data.frame(name_backbone(name = sp))$usageKey
  num_eu_occs <- 0
  for(c in countr){
    num_occs <- occ_count(taxonKey = sp_key,
                          country = c,
                          #from = 1990,
                          from = 2013,
                          #to = 2022)
                          to = 2017)
    num_eu_occs <- num_eu_occs + num_occs
  }
  num_eu_occs_df <- rbind(num_eu_occs_df, data.frame(sp, sp_key, num_eu_occs))
  print(paste0(sp, " - sp ", count, ": ", num_eu_occs))
  count <- count + 1
}

as.data.table(num_eu_occs_df)
num_eu_occs_df[num_eu_occs_df$num_eu_occs == min(num_eu_occs_df$num_eu_occs), ]

write.csv(num_eu_occs_df, "Number_occs_sp_FR.csv", row.names = FALSE)
num_eu_occs_df <- fread("Number_occs_sp_FR.csv", header = TRUE)
num_eu_occs_df <- num_eu_occs_df[order(num_eu_occs_df$sp), ]

library(viridis)

pdf("num_occs_GBIF_FR.pdf", width = 7, height = 9)
num_eu_occs_df %>%
  ggplot(aes(x = reorder(sp, desc(sp)), y = num_eu_occs)) + 
  geom_bar(stat = "identity", fill = viridis(39)) +
  ggtitle("Farmland Birds") +
  labs(x = "Species", y = "Number of Occurrences GBIF (1990-2022)") +
  #theme(plot.title = element_text(color="red", size=14, face="bold.italic")) +
  theme(plot.title = element_text(hjust = 0.3, size = 14, face = "bold")) +
  coord_flip()
dev.off()



#library(lattice)
#barchart(sp ~ num_eu_occs, data = num_eu_occs_df, 
#         main = "Farmland Birds",
#         xlab = "Number of Occurrences GBIF (1990-2022)",
#         ylab = "Species",
#         col = viridis(2))

num_eu_occs_df[order(num_eu_occs_df$num_eu_occs), ]




## Downloading the data ####
library(PreSPickR)

taxons <- sps2dwnld$V2

t0 <- Sys.time()
GetBIF(credentials = paste0(gbif_creds, "/gbif_credentials.RData"),
       taxon_list = taxons,
       download_format = "SIMPLE_CSV",
       download_years = c(2013, 2017),
       #download_coords = c(-13, 48, 35, 72), #order: xmin, xmax, ymin, ymax
       download_coords = c(-6, 8, 42, 52), #order: xmin, xmax, ymin, ymax
       download_coords_accuracy = c(0, 250),
       rm_dupl = TRUE,
       cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                     "gbifID",
                     "coordinateUncertaintyInMeters",
                     "countryCode", "year", 
                     #"institutionCode",	"collectionCode",
                     #"ownerInstitutionCode",
                     "datasetKey"),
       out_name = paste0("sp_records_FR", format(Sys.Date(), "%Y%m%d")))

Sys.time() - t0


## if GetBIF didn't manage to create/write out the data frame with presences:
taxon_dir <- getwd()
#taxons <- taxons$sp

data1 <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                  taxons = taxons,
                  cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                                "gbifID",
                                "coordinateUncertaintyInMeters",
                                "countryCode", 
                                "eventDate", "day", "month",
                                "year", 
                                #"institutionCode",	"collectionCode",
                                #"ownerInstitutionCode",
                                "datasetKey"
                  ),
                  #cols2keep = "all",
                  rm_dupl = TRUE)

head(data1)
nrow(data1)
unique(data1$species)
sort(unique(data1$year))
if(length(unique(data1$species)) != length(unique(data1$sp2))) print("Check the error in 'sp2'!!!")

table(data1$species)

data_sp_year <- data1[, .SD, .SDcols = c("species", "year")] %>% group_by(species) %>% table
data_sp_year
apply(data_sp_year, 2, sum)  # 2017 is the year with more occurrences

data1_2018 <- data1[year == 2018, ]
data1_2018[, .SD, .SDcols = c("species")] %>% group_by(species) %>% table


print(paste0("Saving GBIF data as ", "/sp_records_20220308", ".csv"))
write.csv(data1, file = paste0("sp_records_20220308", ".csv"),
          quote = FALSE, row.names = FALSE)


data <- fread(paste0("sp_records_20220308", ".csv"), header = TRUE)
data



## Falco tinnunculus
load("download_info_Falco tinnunculus.RData", verbose = TRUE)
rqst_02_meta




## Citing information ####
load("download_info_Alauda arvensis.RData", verbose = TRUE)
citation_02








## Mapping occurrences ####

data
sps <- sps2dwnld$V2
sps

# countries map
library(rworldmap)
library(rgdal)
wrld_map <- getMap()
crs(wrld_map)
CRS("+init=EPSG:4326")
#
#wrld_map <- spTransform(wrld_map, crs(occs_all_shp))



#sp <- sps[1]
#sps <- sps[24:length(sps)] 
#sps <- sps[1] 
for (sp in sps){
  print(paste0("running... ", sp, " (", which(sps %in% sp), "/", length(sps), ")"))
  
  if (sp == "Miliaria calandra"){
    print("using the synonym 'Emberiza calandra', as in GBIF data set")
    sp <- "Emberiza calandra"    # There is a problem with this sp, too few occurrences 
                                 # (see https://ebba2.info/maps/species/Emberiza-calandra/ebba2/occurrence/)
  } 
  
  data_sp <- data1[species %in% sp, ]
  
  sum(is.na(data_sp$month))  # 2110 out of 259841 for Alauda arvensis
  sum(is.na(data_sp$month)) / nrow(data_sp) * 100  # 0.8% for Alauda arvensis
  
  #removing occs with no available data for month
  data_sp <- data_sp[!is.na(data_sp$month), ]  
  
  
  occs_i_shp <- SpatialPointsDataFrame(coords = data_sp[, c("decimalLongitude", "decimalLatitude")],
                                       data = data_sp[, .SD, .SDcols = c("species", "month", "year")],
                                       proj4string = CRS("+init=EPSG:4326"))
  
  occs_i_shp
  #sp:::spplot.points(occs_i_shp, zcol = "month",
  #                   xlab = NULL, ylab = NULL, 
  #                   colorkey = TRUE)#, col.regions = get_col_regions())
  
  jpeg(paste0("maps_monthly_", gsub(" ", "_", sp), ".jpg"),
       height = 20, width = 17, units = "cm", res = 150)
  par(mfrow = c(4, 3),
      oma = c(5,4,2,2),
      mar = c(2,1,3,1))
  #par(mar = c(6, 2, 4, 2))
  for (m in 1:12){
    par(xpd = FALSE)
    occs_i_shp_m <- occs_i_shp[occs_i_shp$month %in% m, ]
    plot(occs_i_shp["species"], main = paste0("Month = ", m), pch = 3)
    if(length(occs_i_shp_m) > 0) plot(occs_i_shp_m["species"], col = "red", add = TRUE, pch = 3)
    plot(wrld_map, add = TRUE)
    par(xpd = TRUE)
  }
  legend(-190, 31,
         legend = c(paste0("All occurrences (years = ", paste(range(occs_i_shp$year), collapse = "-"), ")"), 
                    paste0("Monthly occurrences (years = ", paste(range(occs_i_shp$year), collapse = "-"), ")")),
         #fill = c("black","red"),
         pch = 3,
         col = c("black","red"),
         cex = 1.4,
         #lty = c(2,3,4,5),
         title = as.expression(bquote(bolditalic(.(sp)))),
         xpd = NA)
  #legend("bottom", sp_list[sp], fill = cols[sp], ncol = 1, inset = -0.2)
  dev.off()

}




## Mapping occurrences w/ ggplot ####

library(ggplot2)
library(ggridges)
library(patchwork)
library(giscoR)
library(sf)
library(viridis)
library(tidyverse)
library(ggExtra)


data
sps <- sps2dwnld$V2
sps

## Gisco maps
# https://ropengov.github.io/giscoR/
#eur_gisco <- gisco_get_countries(region = "Europe")
#eur_gisco <- gisco_get_countries(region = c("Europe", "Africa", "Asia"))
eur_gisco_eur <- gisco_get_countries(region = "Europe")
eur_gisco_tur <- gisco_get_countries(country = c("Turkey", "Georgia", "Armenia", "Azerbaijan"))
eur_gisco <- st_union(eur_gisco_eur, eur_gisco_tur)
                                     
eur_gisco <- st_crop(eur_gisco, xmin = -10.5, xmax = 50, ymin = 33, ymax = 72)

#ggplot() + geom_sf(data = eur_gisco_tur)
#ggplot() + geom_sf(data = eur_gisco)



### Combined map ####

for (sp in sps){
  print(paste0("running... ", sp, " (", which(sps %in% sp), "/", length(sps), ")"))
  
  if (sp == "Miliaria calandra"){
    print("using the synonym 'Emberiza calandra', as in GBIF data set")
    sp <- "Emberiza calandra"    # There is a problem with this sp, too few occurrences 
    # (see https://ebba2.info/maps/species/Emberiza-calandra/ebba2/occurrence/)
  } 
  
  data_sp <- data[species %in% sp, ]
  
  sum(is.na(data_sp$month))  # 2110 out of 259841 for Alauda arvensis
  sum(is.na(data_sp$month)) / nrow(data_sp) * 100  # 0.8% for Alauda arvensis
  
  #removing occs with no available data for month
  data_sp <- data_sp[!is.na(data_sp$month), ]  
  
  #data_sp <- data_sp[year > 1999, ]
  sort(unique(data_sp$year))
  

  data_sp <- arrange(data_sp, month)
  data_sp$month <- as.factor(data_sp$month)
  #str(data_sp$month)
  unique(data_sp$month)
  
  
  p <- ggplot() +
    geom_sf(data = eur_gisco) +
    geom_point(
      data = data_sp, 
      aes(x = decimalLongitude, y = decimalLatitude,
          color = month
          ),
      show.legend = FALSE,
      size = 0.1
    ) +
    
    theme_light() +
    scale_color_viridis(option = "viridis", discrete = TRUE) #+
    #labs(title = paste0("GBIF monthly occurrences ", paste0(range(unique(data_sp$year)), collapse = "-"), ": ", sp)) + #, x = "TY [°C]", y = "Txxx") +
    #theme(plot.title = element_text(hjust = 0.5),
    #      legend.position = "bottom",
    #      legend.title = element_text(size = 16)) +
    #guides(color = guide_legend("Month", override.aes = list(size = 2)))
  
  #p
  
  plot_p3 <- 1
  plot_p3 <- 0
  if(plot_p3 == 1){
    p3 <- ggMarginal(p,
                     aes(colour = month),
                     type = "density", 
                     #type = "histogram", 
                     #type = "densigram", 
                     groupColour = TRUE, groupFill = TRUE)
    
    p3
  }
  
  
  p1 <- data_sp %>% 
    #arrange(desc(month)) %>%
    ggplot(aes(x = decimalLongitude, y = fct_rev(month), group = month, fill = month)) + 
    geom_density_ridges() +
    labs(y = "Months (Dec-Jan)") +
    xlim(-10, 50) +
    scale_fill_viridis(option = "viridis", discrete = TRUE) +
    guides(fill="none") 
  
  #p1
  
  
  
  p2 <- ggplot(data_sp, aes(x = decimalLatitude, y = month, group = month, fill = month)) + 
    geom_density_ridges(panel_scaling = TRUE) +  
    labs(y = "Months (Jan-Dec)") +
    #scale_y_discrete(expand = c(1, 1)) +
    #scale_x_continuous(expand = c(0.025, 0.02)) +
    #xlim(range(data_sp$decimalLatitude)) +
    xlim(34.4, 71.7) +
    scale_fill_viridis(option = "viridis", discrete = TRUE) +
    guides(fill="none")  +
    #coord_fixed() +
    coord_flip()
  
  #p2
  
  
  p4 <- list(p, p2, p1) %>%         # https://stackoverflow.com/questions/72442442/properly-size-multiple-ggextraplot-objects-after-calling-ggmarginal-in-r
    wrap_plots(nrow = 2, ncol = 2, widths = c(2, 1), heights = c(2, 1)) 

  
  
  ## Plotting a legend
  plot_legend <- 1
  plot_legend <- 0
  
  if(plot_legend == 1){
    p22 <- ggplot(data_sp, aes(x = decimalLatitude, y = month, group = month, fill = month)) +
      geom_density_ridges(panel_scaling = TRUE) + 
      scale_fill_viridis(option = "viridis", discrete = TRUE) +
      guides(fill = guide_legend(ncol = 4, title = "Months (Jan-Dec)", title.hjust = 0.5))
    
    p_legend <- cowplot::get_legend(p22)
    
    
    p4 + 
      plot_annotation(title = paste0("GBIF monthly occurrences ", paste0(range(unique(data_sp$year)), collapse = "-"), ": ", sp),
                      theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) +
      p_legend
    
  }
  
  
  
  ## Plotting barplot with total monthly occurrences instead
  
  if(plot_legend == 0){
    p5 <- ggplot(data_sp, aes(x = month, fill = month)) +
      geom_bar() + 
      labs(x = "Months (Jan-Dec)") +
      labs(y = "Number of occurrences") +
      scale_fill_viridis(option = "viridis", discrete = TRUE) +
      guides(fill="none")  
    #guides(fill = guide_legend(ncol = 4, title = "Months (Jan-Dec)", title.hjust = 0.5, title.position = "top")) +
    #theme(legend.position = "bottom")
    
    #p5
    
    p4 + p5 +
      plot_annotation(title = paste0("GBIF monthly occurrences ", paste0(range(unique(data_sp$year)), collapse = "-"), ": ", sp),
                      theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) 
    
  }
  
  
  

  ggsave(paste0("GBIF_monthly_occurrences_", gsub(" ", "_", sp), ".png"))#, width = 20, height = 20, units = "cm")
  
  
}
  



plot_kk <- 1
plot_kk <- 0

if (plot_kk == 1){
  
  ggplot() +
    geom_sf(data = eur_gisco) +
    geom_point(
      data = data_sp, 
      aes(x = decimalLongitude, y = decimalLatitude,
          #color = coordinateUncertaintyInMeters
          color = month
          #color = year
      ),
      show.legend = TRUE,
      size = 1
    ) +
    
    theme_light() +
    scale_color_viridis(option = "viridis", 
                        #discrete = FALSE) 
                        discrete = TRUE) 
  
  
}







### Monthly occurrences maps ####

for (sp in sps){
  print(paste0("running... ", sp, " (", which(sps %in% sp), "/", length(sps), ")"))
  
  if (sp == "Miliaria calandra"){
    print("using the synonym 'Emberiza calandra', as in GBIF data set")
    sp <- "Emberiza calandra"    # There is a problem with this sp, too few occurrences 
    # (see https://ebba2.info/maps/species/Emberiza-calandra/ebba2/occurrence/)
  } 
  
  data_sp <- data[species %in% sp, ]
  

  #removing occs with no available data for month
  data_sp <- data_sp[!is.na(data_sp$month), ]  
  
  #data_sp <- data_sp[year > 1999, ]
  sort(unique(data_sp$year))
  
  
  data_sp <- arrange(data_sp, month)
  data_sp$month <- as.factor(data_sp$month)
  #str(data_sp$month)
  unique(data_sp$month)
  

  # to rename months
  month_names <- month.name
  names(month_names) <- 1:12
  month_names <- as_labeller(month_names)
  
  

  q <- ggplot() +
    geom_sf(data = eur_gisco) +
    geom_point(data = data_sp[, -"month", with = FALSE], aes(x = decimalLongitude, y = decimalLatitude),
      show.legend = FALSE, size = 0.1, colour = "darkgrey") +
    geom_point(
      data = data_sp, 
      aes(x = decimalLongitude, y = decimalLatitude,
          color = month),
      show.legend = FALSE,
      size = 0.1
    ) +
    scale_color_viridis(option = "viridis", discrete = TRUE) +
    #theme(plot.title = element_text(hjust = 0.5),
    #      legend.position = "bottom",
    #      legend.title = element_text(size = 16)) +
    #guides(color = guide_legend("Month", override.aes = list(size = 2))) +
    facet_wrap(~ month, nrow = 4, labeller = month_names) +
    theme_light() +
    labs(title = paste0("GBIF monthly occurrences ", paste0(range(unique(data_sp$year)), collapse = "-"), ": ", sp)) +
    theme(plot.title = element_text(hjust = 0.5))
    
  #q
  
  
  q +  geom_point(data = data_sp, aes(x = decimalLongitude, y = decimalLatitude, size = "All occurrences", shape = NA), 
               colour = "darkgrey") + guides(size = guide_legend(paste0(range(unique(data_sp$year)), collapse = "-"), override.aes = list(size = 2)))
  
  
  ggsave(paste0("GBIF_monthly_occurrences_", gsub(" ", "_", sp), "_MonthlyMaps.png"), width = 21, height = 30, units = "cm")

  
}  









## plotting climatic rasters ####
library(raster)
library(rasterVis)
library(viridis)
library(ggplot2)
library(ggpubr)

worldclim_all

getwd()
wd <- "/eos/jeodpp/home/users/rotllxa/FarmlandBirds/"

jpeg("/eos/jeodpp/home/users/rotllxa/FarmlandBirds/worldclim.jpg")
gplot(worldclim_all[[c(1:3, 1)]]) + 
  geom_raster(aes(fill = value), show.legend = FALSE) +
  scale_fill_gradientn(colours = rev(magma(30)), na.value = "transparent") +
  facet_wrap(~ variable, ncol = 2) 
dev.off()

