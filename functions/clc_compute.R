
clc_compute <- function(Data_SB_model, Data_Parcels.SC, buffer, cultivation){
  buf_range=set_units(buffer, m)
  crs_rpg=st_crs(CLC)
  
  #Create points layer
  coords <- data.frame(ID=Data_Parcels.SC$ID,
                       lat = Data_Parcels.SC$LATITUDE,
                       long = Data_Parcels.SC$LONGITUDE,
                       type = Data_Parcels.SC$TYPEPARCELLELIB)
  coords=na.omit(coords)
  if(cultivation=="All"){
    points <- st_as_sf(coords,coords=c("long","lat"), crs="+proj=longlat")
  }else{
    points <- st_as_sf(coords[coords$type==cultivation,],coords=c("long","lat"), crs="+proj=longlat")
  }
  points <- st_transform(points,  crs_rpg)
  
  #Create buffer layer
  buf <- st_buffer(points, dist=buf_range)
  
  #Intersection
  inters <- st_intersection(CLC, buf)
  
  #We can group by type of forests and compute the area
  inters_summary <- group_by(inters, ID.1, CODE_18) %>% 
    summarise()
  inters_summary$area <- st_area(inters_summary)
  
  inters_dataframe <- as.data.frame(inters_summary)
  
  for(k in 1:nrow(inters_dataframe)){
    ID=as.character(inters_dataframe[k, "ID.1"])
    code=inters_dataframe[k, 'CODE_18']
    colname <- paste0("CLC_for_area_", code)
    Data_SB_model[Data_SB_model$ID==ID, colname] <- inters_dataframe[k, "area"]/10000
  }
  
  #We can also compute the perimeter
  inters_summary <- group_by(inters, ID.1) %>% 
    summarise()
  inters_summary$perimeter <- st_perimeter(inters_summary)
  
  inters_dataframe <- as.data.frame(inters_summary)
  for(k in 1:nrow(inters_dataframe)){
    ID=as.character(inters_dataframe[k, "ID.1"])
    Data_SB_model[Data_SB_model$ID==ID, "CLC_for_perim"] <- inters_dataframe[k, "perimeter"]
  }
  
  return(Data_SB_model)
}
