#fonction pour convertir RPG v2 vers RPG v1



conv_v1 <- function(RPG, ilots){
  crs_rpg <- crs(RPG)
  
  #intersection parcelles ilots
  inters <- st_intersection(ilots, RPG)
  inters$SURF_PARC <- st_area(inters)
  inters$SURF_PARC<-set_units(inters$SURF_PARC, NULL)
  inters$SURF_PARC<-inters$SURF_PARC/10000
  
  dat <- data.frame(inters)
  dat <- dat[, c("ID_ILOT", "CODE_GROUP", "SURF_PARC")]
  
  #spread
  dat2 <- dat %>% 
    dplyr::group_by_at(vars(-SURF_PARC)) %>%  # group by everything other than the value column. 
    dplyr::mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
    spread(key="CODE_GROUP", value="SURF_PARC") %>%    # spread
    dplyr::select(-row_id)  # drop the index
  
  dat2[is.na(dat2)]<-0
  
  dat3 <- dat2 %>% 
    dplyr::group_by(ID_ILOT) %>% 
    dplyr::summarise_all(funs(sum))
  
  dat4 <- merge(dat3, ilots)
  names(dat4) <- c("ID_ILOT", "X01", "X11", "X15", "X16", "X17", 
                   "X18", "X19", "X02", "X20", "X21", "X22", "X23",
                   "X24", "X25", "X28", "X03", "X04", "X05", "X06",
                   "X07", "X08", "X09", "geometry")
  
  dat4<-st_as_sf(dat4)
  
  return(dat4)
}
  