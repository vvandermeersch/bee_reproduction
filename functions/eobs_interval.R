#fonction pour calculer temp moyenne et precip dans l'intervalle
##faire d'abord tourner une fois sans "end"
###puis une fois avec "end" pr?cis?e (refera les calculs seulement si n?cessaire)

eobs_interval <- function(Data_SB_model, type, begin="02-20", end=NA){
  #Create points layer
  coords <- data.frame(lat = Data_SB_model$LATITUDE,
                       long = Data_SB_model$LONGITUDE)
  coords=na.omit(coords)
  points <- st_as_sf(coords,coords=c("long","lat"), crs="+proj=longlat")
  eval(parse(text=paste0("crs_eobs<-st_crs(eobs_rast_",type,")")))
  points <- st_transform(points,  crs_eobs)
  coltype <- type
  if(!is.na(end)){coltype<-paste0(type,"_mf")}
  
  Data_SB_model[, coltype]<-NA
  
  for(k in 1:nrow(Data_SB_model)){
    if(is.na(Data_SB_model[k, coltype]) & !is.na(end)){
      year <- Data_SB_model[k, 'ANNEE_OBS']
      ID <- Data_SB_model[k, 'ID']
      date <- Data_SB_model[k, 'Date.de.passage']
      dbegin <- paste0(year, "-", begin)
      dend <- paste0(year, "-", end)
      if(date > dend){date <- dend
        eval(parse(text=paste0("subeobs <- eobs_rast_",type,"[[which(as.Date(substr(names(eobs_rast_",type,"), 2, 11), format='%Y.%m.%d')>= dbegin & as.Date(substr(names(eobs_rast_",type,"), 2, 11), format='%Y.%m.%d')<= date)]]")))
        crs_eobs=st_crs(subeobs)
      
        if(type=="temp"){subeobs <- mean(subeobs)
        }else if (type=="prec"){subeobs <- sum(subeobs)}
    
        #Intersection 
        inters <- raster::extract(subeobs, points[k,], method='bilinear') #bilinear si le point n'est pas exactement dans une cellule...
        print(k)
        Data_SB_model[k, coltype]<-inters
      }else{Data_SB_model[k, coltype]<-Data_SB_model[k, type]}
    }else if(is.na(Data_SB_model[k, coltype]) & is.na(end)){
      year <- Data_SB_model[k, 'ANNEE_OBS']
      ID <- Data_SB_model[k, 'ID']
      date <- Data_SB_model[k, 'Date.de.passage']
      dbegin <- paste0(year, "-", begin)
      eval(parse(text=paste0("subeobs <- eobs_rast_",type,"[[which(as.Date(substr(names(eobs_rast_",type,"), 2, 11), format='%Y.%m.%d')>= dbegin & as.Date(substr(names(eobs_rast_",type,"), 2, 11), format='%Y.%m.%d')<= date)]]")))
      crs_eobs=st_crs(subeobs)
      if(type=="temp"){subeobs <- mean(subeobs)
      }else if (type=="prec"){subeobs <- sum(subeobs)}
      
      #Intersection 
      inters <- raster::extract(subeobs, points[k,], method='bilinear') #bilinear si le point n'est pas exactement dans une cellule...
      
      Data_SB_model[k, coltype]<-inters
      print(k)
    }
  }
  return(Data_SB_model)
}

