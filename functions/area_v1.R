

area_v1 <- function(Data_SB_model, Data_Parcels.SC, years=c(2012, 2013, 2014, 2015,2016,2017), buffer, cultivation, previous=FALSE){
  buf_range=set_units(buffer, m)
  interglob <- data.frame()
  for(i in years){
    if(previous){
      RPG_i=get(paste0("RPG_v1_", i-1))
      crs_rpg=st_crs(RPG_i)
      area <- 'area_n1_'
    }else{
      RPG_i=get(paste0("RPG_v1_", i))
      crs_rpg=st_crs(RPG_i)
      area <- 'area_'
    }
    
    #calculer aire ilots
    RPG_i$area_ilot <- st_area(RPG_i)
    
    #Create points layer
    coords <- data.frame(ID=Data_Parcels.SC[Data_Parcels.SC$ANNEE_OBS==i,]$ID,
                         lat = Data_Parcels.SC[Data_Parcels.SC$ANNEE_OBS==i,]$LATITUDE,
                         long = Data_Parcels.SC[Data_Parcels.SC$ANNEE_OBS==i,]$LONGITUDE,
                         type = Data_Parcels.SC[Data_Parcels.SC$ANNEE_OBS==i,]$TYPEPARCELLELIB)
    coords=na.omit(coords)
    if(cultivation=="All"){
      points <- st_as_sf(coords,coords=c("long","lat"), crs="+proj=longlat")
    }else{
      points <- st_as_sf(coords[coords$type==cultivation,],coords=c("long","lat"), crs="+proj=longlat")
    }
    
    points <- st_transform(points,  crs_rpg)
    
    #Create buffer layer
    buf <- st_buffer(points, dist=buf_range)
    
    
    
    #intersection buffer
    inters <- st_intersection(RPG_i, buf)
    
    inters$area <- st_area(inters)
    inters$prop <- inters$area/inters$area_ilot
    
    inters <- data.frame(inters)
    inters <- subset(inters, select = -c(geometry))
    inters <- unique(inters)
    
    if(previous){
      inters_summary <- inters %>% 
        dplyr::group_by(ID) %>% 
        dplyr::mutate(area_n1_corn=as.numeric(sum(as.numeric(X02)*prop)), area_n1_rapeseed=as.numeric(sum(as.numeric(X05)*prop)), 
               area_n1_sunflower=as.numeric(sum(as.numeric(X06)*prop)), area_n1_PM=as.numeric(sum(as.numeric(X18)*prop)),
               area_n1_orchard=as.numeric(sum(as.numeric(X20)*prop)), area_n1_TMF=as.numeric(sum(as.numeric(X19)*prop)),
               area_n1_vineyard=as.numeric(sum(as.numeric(X21)*prop))) %>%
        dplyr::select(ID, area_n1_corn, area_n1_rapeseed, 
                      area_n1_sunflower, area_n1_PM,
                      area_n1_orchard, area_n1_TMF,
                      area_n1_vineyard)
    }else{
      inters_summary <- inters %>% 
        dplyr::group_by(ID) %>% 
        dplyr::mutate(area_corn=as.numeric(sum(as.numeric(X02)*prop)), area_rapeseed=as.numeric(sum(as.numeric(X05)*prop)), 
               area_sunflower=as.numeric(sum(as.numeric(X06)*prop)), area_PM=as.numeric(sum(as.numeric(X18)*prop)),
               area_orchard=as.numeric(sum(as.numeric(X20)*prop)), area_TMF=as.numeric(sum(as.numeric(X19)*prop)),
               area_vineyard=as.numeric(sum(as.numeric(X21)*prop))) %>%
        dplyr::select(ID, area_corn, area_rapeseed, 
                      area_sunflower, area_PM,
                      area_orchard, area_TMF,
                      area_vineyard)
    }
    
    
    inters_summary$ID <- as.character(inters_summary$ID)
    inters_summary <- data.frame(inters_summary)
    interglob <- rbind(interglob, inters_summary)
    
    
  }
  Data_SB_model <- left_join(Data_SB_model, unique(interglob), by=c("ID"))
  return(Data_SB_model)
}
