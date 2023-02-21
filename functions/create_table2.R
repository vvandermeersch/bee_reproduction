
#Version Victor, avec scripts différents et left_join plutôt que merge (pour enlever les doublons)

create_table2 <- function(species, cultivation, prep_parcels=FALSE, prep_species=FALSE){
  # Load prepare_parcels.R
  if (prep_parcels) source("scripts/prepare_parcels2.R")
  if (!exists("Data_Parcels")) stop("use prep_parcels=TRUE")
  
  # Load table depending on the cultivation
  if (cultivation=="All"){ tab <- Data_Parcels[ , ! colnames(Data_Parcels) %in% c("ANNEE_OBS", "ID_SGBD_parcelle")]
  }else{ tab <- which(Data_Parcels$TYPEPARCELLELIB==cultivation) %>% Data_Parcels[.,]
  tab <- tab[ , ! colnames(tab) %in% c("ANNEE_OBS", "ID_SGBD_parcelle")]
  }
  
  # Load prepare_species.R
  if (prep_species) source(paste0("scripts/prepare_",species,"2.R"))
  
  
  # Merge with parcels and species data
  
  ##bees
  if (!species%in%c("bees","worms","invertebrates","butterflies")) stop("wrong species")
  
  if (species=="bees"){
    tab <- left_join(Sampling_SB,tab, by="ID")
  }
  
  
  
  return(tab)
}