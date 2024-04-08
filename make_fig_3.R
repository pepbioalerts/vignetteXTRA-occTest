
#### figure 3 ####

library(occTest)
library(spocc)
# Getting some tree occurrences
df_qupe <- occ(query ='Quercus petraea',limit = 1000)
occ_data_qupe <-occ2df(df_qupe)
names (occ_data_qupe)[2] <- 'decimalLongitude'
names (occ_data_qupe)[3] <- 'decimalLatitude'


occ_data_qupe<-occ_data_qupe[occ_data_qupe$decimalLatitude>10,]


# downloading environental data
library (raster)
renv = raster::getData(name='worldclim',var='bio', res=10,path = tempdir())
renv <- rast(renv)
# running occTest
out_qupe = occTest(sp.name='Quercus petraea',sp.table = occ_data_qupe ,r.env = renv,verbose = F)
# running a strict filtering process
out_filtered_qupe= occFilter (df = out_qupe , errorAcceptance = 'strict')

custom_rules<-readRDS(system.file('ext/fieldMetadata.rds',package='occTest'))
custom_rules$errorThreshold<-rep(0.5,nrow(custom_rules))
custom_rules[custom_rules$testBlock=="lu","errorThreshold"]<-0.5
custom_rules[custom_rules$testBlock=="geo","errorThreshold"]<-0.2
custom_rules[custom_rules$testBlock=="env","errorThreshold"]<-0.1


out_filtered_qupe = occFilter(df = out_qupe,custom=custom_rules)


list_of_plots<-plot(x = out_qupe , occFilter_list = out_filtered_qupe, show_plot = F)
list_of_plots
list_of_plots[[2]]

library(ggpubr)

export_2_2<-ggarrange(plotlist = list_of_plots[1:4],nrow=2,ncol=2,align = "hv",labels=c("a)","b)","c)","d)"))
export_2_2

ggsave("Figure_manuscript_3.jpg",export_2_2,units = "mm",width = 180,height = 160,dpi=450,scale=1.5) # psme
ggsave("Figure_manuscript_3.pdf",export_2_2,units = "mm",width = 180,height = 160,dpi=600,scale=1.5) # psme

# 
# 
# results_arrange<-ggarrange(plotlist = list(plot_1,plot_env,plot_hii,plot_filter),ncol=2,nrow=2,align = "v",labels=c("a)","b)","c)","d)"))
# results_arrange<-ggarrange(plotlist = list(plot_1,plot_env),ncol=1,nrow=2,align = "v",labels=c("a)","b)"))
# 
# occ_species_test$tot_score<-rowSums( occ_species_test[,grep("_score",colnames(occ_species_test),value=T)],na.rm = T)
# occ_species_test_sf$tot_score<-rowSums( occ_species_test[,grep("_score",colnames(occ_species_test),value=T)],na.rm = T)
# 


library(ggpubr)
#load packages
library(occTest)
library(spocc)
library(sf)

sp_to_test<-"Abies alba"
sp_to_test<-"Fagus sylvatica"
sp_to_test<-"Quercus petraea"


df <- occ(query = sp_to_test,limit = 1000)
occ.data <-occ2df(df)

#we change the names here but we could just change it in the tableSettings
names (occ.data)[2] <- 'decimalLongitude'
names (occ.data)[3] <- 'decimalLatitude'

#load needed environmental data
library (raster)
renv = raster::getData(name='worldclim',var='bio', res=10,path = tempdir())

#test occurrence


outDougFir = occTest(sp.name=sp_to_test,sp.table = occ.data,r.env = renv,verbose = F)



#filter data
outDougFirSelec = occFilter(df = outDougFir,errorAcceptance = 'relaxed')

custom_rules<-readRDS(system.file('ext/fieldMetadata.rds',package='occTest'))

custom_rules$errorThreshold<-rep(0.5,nrow(custom_rules))
custom_rules[custom_rules$testBlock=="lu","errorThreshold"]<-0.6
custom_rules[custom_rules$testBlock=="geo","errorThreshold"]<-0.1
custom_rules[custom_rules$testBlock=="env","errorThreshold"]<-0.2
custom_rules[custom_rules$testBlock=="time","errorThreshold"]<-1


outDougFirSelec = occFilter(df = outDougFir,custom=custom_rules)

plot(outDougFir,outDougFirSelec)
#show filtered dataset
nrow (outDougFir)
nrow (outDougFirSelec[["filteredDataset"]])


outDougFirSelecstrict = occFilter (df = outDougFir, errorAcceptance = 'strict')


list_of_plots<-plot(outDougFir , occFilter_list = outDougFirSelec, show_plot = F)



sf_doug<-st_as_sf(outDougFir,coords=c("decimalLongitude",'decimalLatitude'),crs=st_crs(4326))

list_of_plots

crop_hii<-sets$analysisSettings$humanDetection$ras.hii
crop_hii<-crop(crop_hii,bbox(as_Spatial( sf_doug)))
crop_hii_dt<-data.frame(xyFromCell(crop_hii,1:ncell(crop_hii)))
crop_hii_dt$hii<-getValues(crop_hii$hii3)

list_of_plots[[3]]+geom_raster(data=crop_hii_dt,aes(x=x,y=y,fill=hii),alpha=0.7)+scale_fill_gradientn(breaks=c(0,20,40,60),colors=c("grey90","grey70","violetred3"),na.value =NA )


first_filter<-list_of_plots[[1]]
filtertype_3<-ggarrange(plotlist = list_of_plots[2:4],nrow=3,ncol=1,common.legend = T,legend="bottom")

export_4_1<-ggarrange(first_filter,filtertype_3,nrow=2,heights = c(1.32,3),align = "v")

ggsave("Figure_manuscript.jpg",export_4_1,units = "mm",width = 180,height = 360,dpi=300)


export_2_2<-ggarrange(plotlist = list_of_plots[1:4],nrow=2,ncol=2,align = "hv",labels=c("a)","b)","c)","d)"))


ggsave("Figure_manuscript_2.jpg",export_2_2,units = "mm",width = 180,height = 130,dpi=300,scale=1.5) # psme

ggsave("Figure_manuscript_2.jpg",export_2_2,units = "mm",width = 180,height = 160,dpi=300,scale=1.5) # qupe
ggsave("Figure_manuscript_3.jpg",export_2_2,units = "mm",width = 180,height = 170,dpi=300,scale=1.5) # qupe
ggsave("Figure_manuscript_4.jpg",export_2_2,units = "mm",width = 180,height = 145,dpi=300,scale=1.5) # qupe
