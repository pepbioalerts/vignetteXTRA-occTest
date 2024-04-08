#### Figure 2 ####
library(occTest)
library(spocc)
library(sf)
library(ggplot2)


#Load data
sp_file <- system.file('ext/exampleOccData.csv',package='occTest')
occ_species_raw <- read.table(file = sp_file,header = T,sep = ',',as.is = T)
#Load the raster of the environmental variables to the specific region
ras_env_file <- system.file('ext/AllEnv.tif',package='occTest')
ras_env <- raster::stack(ras_env_file)
#Load a high resolution raster of elevation
ras_dem_file <- system.file('ext/DEM.tif',package='occTest')
ras_dem <- raster::raster(ras_dem_file)
#Load a high resolution coastlines
pol_ctry_file <- system.file('ext/countries.rds',package='occTest')
pol_ctry <- readRDS(pol_ctry_file)


#show default settings
show_table_Names()
#modify settings
my_table_Settings <- occTest::set_tableNames(x.field = 'MAPX',
                                             y.field = 'MAPY',
                                             t.field = 'DATE',
                                             l.field = 'LOCALITYNAME',
                                             c.field = 'CONTRYRECORD',
                                             e.field = 'ELEVATION',
                                             a.field =c('UNCERTAINTY_X_M','UNCERTAINTY_Y_M'))

my_analysis_settings <- set_testTypes(geoenvLowAccuracy = F)
my_writeout_settings <- set_writeout(output.dir = getwd(),writeAllOutput = T)


my_analysis_settings

library(terra)
ras_env<-rast(ras_env)
ras_dem<-rast(ras_dem)
#run workflow
occ_species_test <- occTest::occTest(sp.name = 'Genus_species',
                                     sp.table = occ_species_raw,
                                     r.env =ras_env,
                                     tableSettings = my_table_Settings,
                                     analysisSettings = my_analysis_settings,
                                     writeoutSettings = my_writeout_settings,
                                     r.dem =   ras_dem,
                                     ntv.ctry = 'ESP',
                                     inv.ctry = 'FRA',
                                     verbose = T)

#modify some analysis table parameters
my_analysis_settings <- defaultSettings()$analysisSettings
my_analysis_settings$humanAnalysis$th.human.influence <-50
my_analysis_settings$rangeAnalysis$countries.shapefile <-pol_ctry
my_analysis_settings$rangeAnalysis$countryfield.shapefile <- 'ISO'
my_analysis_settings$geoenvLowAccuracy$doGeoEnvAccuracy <- F
my_analysis_settings$envOutliers$methodEnvOutliers <- "grubbs"



occ_species_raw_shorten<-occ_species_raw[occ_species_raw$MAPY>10,]


occ_species_test <- occTest::occTest(sp.name = 'Genus_species',
                                     sp.table = occ_species_raw,
                                     r.env =ras_env,
                                     tableSettings = my_table_Settings,
                                     analysisSettings = my_analysis_settings,
                                     writeoutSettings = my_writeout_settings,
                                     r.dem =   ras_dem,
                                     ntv.ctry = 'ESP',
                                     inv.ctry = 'FRA',
                                     verbose = T)

custom_rules<-readRDS(system.file('ext/fieldMetadata.rds',package='occTest'))
custom_rules$errorThreshold<-rep(0.5,nrow(custom_rules))
custom_rules[custom_rules$testBlock=="lu","errorThreshold"]<-0.7
custom_rules[custom_rules$testBlock=="geo","errorThreshold"]<-0.2
custom_rules[custom_rules$testBlock=="env","errorThreshold"]<-0.5
custom_rules[custom_rules$testBlock=="time","errorThreshold"]<-1



occ_species_test_filter<-occFilter(occ_species_test,custom = custom_rules)

occ_species_test_sf<-st_as_sf(occ_species_test[!is.na(occ_species_test$MAPX)&occ_species_test$MAPY>10,],coords = c("MAPX","MAPY"),crs=st_crs(4326))



pol_ctry_sf<-st_as_sf(pol_ctry)


library(raster)

ras_env <- raster::stack(ras_env_file)
ras_env_dt<-data.frame(xyFromCell(ras_env,1:ncell(ras_env)))
ras_env_dt$Env<-getValues(ras_env$AllEnv_1)

hii<-my_analysis_settings$humanDetection$ras.hii
hii<-crop(hii,ras_env)
hii_dt<-data.frame(xyFromCell(hii,1:ncell(hii)))
hii_dt$Hii<-readValues(hii)


size_big_point<-1.7
size_small_point<-1.45

plot_1<-ggplot(pol_ctry_sf)+
  geom_sf(fill="grey90")+
  theme_bw()+
  geom_sf(data=occ_species_test_sf,size=size_big_point)+
  #  geom_sf(data=occ_species_test_sf,aes(color=geoenvLowAccuracy_score),size=size_small_point)+  # no geo accuracy test done, but coul be showed too ? 
  labs(x="",y="",title="Original dataset")+
  coord_sf(expand = F)

occ_species_test_sf$Env_score<-as.factor(round(occ_species_test_sf$envOutliers_score,2))

plot_env<-ggplot(pol_ctry_sf)+
  geom_raster(data=ras_env_dt,aes(x=x,y=y,fill=Env))+
  geom_sf(fill=NA)+
  theme_bw()+
  geom_sf(data=occ_species_test_sf,color="black",size=size_big_point)+
  geom_sf(data=occ_species_test_sf,aes(color=Env_score),size=size_small_point)+
  scale_fill_viridis_c(na.value = "transparent")+
  scale_color_manual(values = c("skyblue2","tan1","tomato4"))+
  labs(x="",y="",title="Environmental outlier tests")+
  theme(legend.position = "bottom")+
  coord_sf(expand = F)+
  guides(fill=guide_colorbar(title.position = "top", title.hjust = 0.5),color = guide_legend(title.position = "top", title.hjust = 0.5,label.position = "bottom",override.aes=list(size=3)))
#scale_fill_gradient2(na.value = "transparent",low="blanchedalmond",mid="lightsteelblue",high = "lightsalmon",midpoint = 7,limits=c(0,15),oob=scales::squish)

occ_species_test_sf$Hii_score<-as.factor(occ_species_test_sf$humanDetection_score)

library(data.table)
hii_dt<-data.table(hii_dt)
missing_y<-hii_dt[,sum(is.na(Hii)),by=y][order(-V1),][1,y]
one_pixel_north<-hii_dt[,sum(is.na(Hii)),by=y][order(abs(y-40.72903)),][2,y]


hii_dt[y==missing_y,Hii:=hii_dt[y==one_pixel_north,Hii]]
hii_dt[y==missing_y,]

hii_dt[y==one_pixel_north,]

#plotly::ggplotly(plot_hii)

plot_hii<-ggplot(pol_ctry_sf)+
  geom_tile(data=hii_dt,aes(x=x,y=y,fill=Hii))+
  geom_sf(fill=NA)+
  theme_bw()+
  geom_sf(data=occ_species_test_sf,color="black",size=size_big_point)+
  geom_sf(data=occ_species_test_sf,aes(color=Hii_score),size=size_small_point)+
  scale_fill_gradient2(na.value = "transparent",low="mintcream",mid="seashell3",high = "orangered3",midpoint = 20)+
  scale_color_manual(values = c("skyblue2","tan1","tomato4"))+
  labs(x="",y="",title="Human influence tests")+
  theme(legend.position = "bottom")+
  coord_sf(expand = F)+ guides(fill=guide_colorbar(title.position = "top", title.hjust = 0.5,order=1),color = guide_legend(title.position = "top", title.hjust = 0.5,label.position = "bottom",override.aes=list(size=3)))



occ_species_test_sf$Filtered<-!occ_species_test_sf$ID_GRAFIC%in%occ_species_test_filter$filteredDataset$ID_GRAFIC

plot_filter<-ggplot(pol_ctry_sf)+
  geom_sf(fill="grey90")+
  theme_bw()+
  #geom_sf(data=occ_species_test_sf,color="black",size=2)+
  geom_sf(data=occ_species_test_sf[occ_species_test_sf$Filtered==T,],aes(color="Filtered"),size=size_small_point,shape=4, stroke = 2)+
  geom_sf(data=occ_species_test_sf[occ_species_test_sf$Filtered==F,],aes(color="Kept"),size=size_big_point,shape=16)+
  scale_color_manual(values=c("red3","royalblue1"))+
  guides(colour =guide_legend(override.aes = list(shape=16) ))+
  labs(x="",y="",color="Occurrences",title="Filtered occurrences")+
  theme(legend.position = "bottom")+scale_x_continuous()+
  coord_sf(expand = F)

occ_species_test_sf$Geo_score<-as.factor(occ_species_test_sf$geoOutliers_score)

plot_geo<-ggplot(pol_ctry_sf)+
  geom_sf(fill="grey90")+
  theme_bw()+
  geom_sf(data=occ_species_test_sf,color="black",size=size_big_point)+
  geom_sf(data=occ_species_test_sf,aes(color=Geo_score),size=size_small_point)+
  scale_color_manual(values = c("skyblue2","tan1","tomato3","grey20"))+
  guides(fill = guide_colourbar(order = 1),color = guide_legend(order = 2,override.aes=list(size=3)))+
  labs(x="",y="",title="Geographic outlier tests")+
  theme(legend.position = "bottom")+
  coord_sf(expand = F)+guides(color = guide_legend(title.position = "top", title.hjust = 0.5,label.position = "bottom",override.aes=list(size=3))) 

plot_geo

library(patchwork)

results_arrange<-plot_1+plot_geo+plot_hii+plot_env+ plot_annotation(tag_levels = 'a',tag_suffix = ')')& theme(plot.tag = element_text(face="bold"))

ggsave("Figure_manuscript_ex_2.png",results_arrange,units = "mm",width = 180,height = 190,dpi=700,scale=1.4, type = "cairo") 
ggsave("Figure_manuscript_ex_2.pdf",results_arrange,units = "mm",width = 180,height = 180,dpi=700,scale=1.4) 

plot(occ_species_test,occ_species_test_filter)
