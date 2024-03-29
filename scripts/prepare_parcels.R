###################################
### Prepare general parcel data ###
###################################


#Setup----

library(dplyr)

library(ggplot2)

library(sf)
library(units)



source("functions/names_other_crops.R")
source("functions/unaccent.R")

#Read data----

Practices <- read.table(file = "data/raw_data/TableauPratiquesParcelles.txt",
                        header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")

Applications <- read.table(file = "data/raw_data/TableauTraitementsParcelles.txt",
                           header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")

Field <- read.table(file = "data/raw_data/TableauInfosExploitations.txt",
                    header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")

Landscape <- read.table(file = "data/raw_data/TableauPaysageParcelles.txt",
                                 header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")

#Coordinate correction----
Field$coordcor <- FALSE
for (i in 1 :nrow(Field)){
  # if (!is.na(Field$LATITUDE[i])&Field$LATITUDE[i]<41.3166667) Field$LATITUDE[i] <-  NA
  if (!is.na(Field$LONGITUDE[i])&!between(Field$LATITUDE[i],41.3166667,51.0716667)){ Field$LATITUDE[i] <-  NA}
  if (!is.na(Field$LATITUDE[i])&!between(Field$LONGITUDE[i],-5.151112,9.56)){ Field$LONGITUDE[i] <-  NA}
  # if (!is.na(Field$LATITUDE[i])&Field$LONGITUDE[i]>9.56) Field$LONGITUDE[i] <-  NA
  if (is.na(Field$LATITUDE[i])){
    Field$LATITUDE[i] = Field$LATITUDETOWNS[i]
    Field$coordcor[i] = TRUE}
  if (is.na(Field$LONGITUDE[i])){
    Field$LONGITUDE[i] = Field$LONGITUDETOWNS[i]
    Field$coordcor[i] = TRUE}
}

#Applications----
## Sommes des traitements

Applications$Traitement.Tot <-
  apply(Applications[,c("Traitement.insecticide","Traitement.herbicide",
                                "Traitement.fongicide","Traitement.mollucide","Autre.traitement")],1, sum)

Applications$Fertilisation.Tot <-
  apply(Applications[,c("Fertilisation.minerale","Fertilisation.organique")],1, sum)

Applications$Tot.passages <- Applications$Traitement.Tot + Applications$Fertilisation.Tot
#Merge table----

names(Practices)[1] <- 'ID_SGBD_parcelle'
names(Applications)[1] <- 'ID_SGBD_parcelle'
names(Landscape)[1] <- 'ID_SGBD_parcelle'

Data_Parcels <- merge(Practices[,c('ID_SGBD_parcelle','ID', 'ANNEE_OBS', 'TYPEPARCELLELIB','groupe_espcultiv','espcultiv', 'EC_CL', 'pcultn1', 'pcultn2', 'conduite', 'travailsol', 'interrang', 'TYPE_PRAIRIE', 'AGEPRAIRIE')],
                    Applications[,c('ID', 'Traitement.insecticide', 'Traitement.fongicide', 'Traitement.Tot', "Fertilisation.minerale","Fertilisation.organique")],
                    by="ID")

Data_Parcels <- merge(Data_Parcels, Field[,c('ID_SGBD_parcelle', 'LATITUDE', 'LONGITUDE', 'coordcor')], 
                    by="ID_SGBD_parcelle")

Data_Parcels <- merge(Data_Parcels, Landscape[,c('ID', 'PAYSAGE')], 
                      by="ID")

#Clean data----

#remove accent and special characters
Data_Parcels$espcultiv <- iconv(Data_Parcels$espcultiv,from="UTF-8",to="ASCII//TRANSLIT")
Data_Parcels$pcultn1 <- iconv(Data_Parcels$pcultn1,from="UTF-8",to="ASCII//TRANSLIT")
Data_Parcels$pcultn2 <- iconv(Data_Parcels$pcultn2,from="UTF-8",to="ASCII//TRANSLIT")

#practices
Data_Parcels$conduite <- factor(Data_Parcels$conduite, levels = c("AUTRE","BIOLOGIQUE","CONVENTIONNELLE"))
Data_Parcels$travailsol <- factor(Data_Parcels$travailsol, levels = c("TRAVAIL SUPERFICIEL","LABOUR PROFOND","SEMI DIRECT"))
Data_Parcels$interrang <- factor(Data_Parcels$interrang, levels = c("ENHERBE","ENHERBE PARTIELLEMENT","SOL NU","PAILLE"))

#meadows
Data_Parcels$TYPE_PRAIRIE <- factor(Data_Parcels$TYPE_PRAIRIE, levels = c("Temporaire","Permanente"))
Data_Parcels[Data_Parcels$AGEPRAIRIE>1900& !is.na(Data_Parcels$AGEPRAIRIE),"AGEPRAIRIE"] <- Data_Parcels[Data_Parcels$AGEPRAIRIE>1900& !is.na(Data_Parcels$AGEPRAIRIE),"ANNEE_OBS"]-Data_Parcels[Data_Parcels$AGEPRAIRIE>1900& !is.na(Data_Parcels$AGEPRAIRIE),"AGEPRAIRIE"]

#landscape 
Data_Parcels$PAYSAGE <- factor(Data_Parcels$PAYSAGE, levels = c("Extensif","Intensif"))

#crops
# Data_Parcels[Data_Parcels$espcultiv%in%c("chanvre","pomme de terre"),"groupe_espcultiv"] <- "autre"
# Data_Parcels[Data_Parcels$pcultn1%in%c("chanvre","pomme de terre"),"groupe_pcultn1"] <- "autre"
# Data_Parcels[Data_Parcels$pcultn2%in%c("chanvre","pomme de terre"),"groupe_pcultn2"] <- "autre"
# Data_Parcels[Data_Parcels$teterot%in%c("chanvre","pomme de terre"),"groupe_teterot"] <- "autre"
# 
# Data_Parcels[Data_Parcels$espcultiv=="lin","groupe_espcultiv"] <- "oléagineux"
# Data_Parcels[Data_Parcels$pcultn1=="lin","groupe_pcultn1"] <- "oléagineux"
# Data_Parcels[Data_Parcels$pcultn2=="lin","groupe_pcultn2"] <- "oléagineux"
# Data_Parcels[Data_Parcels$teterot=="lin","groupe_teterot"] <- "oléagineux"
# 
# noms_autres <-  names_other_crops(data = Data_Parcels[Data_Parcels$espcultiv=="autre grande culture",
#                                                               c("groupe_espcultiv","espcultiv","EC_CL","ID")])
# levels(Data_Parcels$espcultiv) <- levels(noms_autres[,2])
# for( i in 1:nrow(noms_autres)){
#   Data_Parcels[Data_Parcels$ID==noms_autres$ID[i],c("groupe_espcultiv","espcultiv")] <-
#     noms_autres[i,c("groupe_espcultiv","espcultiv")]
# }

#Floral ressources----


RF=data.frame(espcul=c('tournesol', 'soja', 'oeillette', 'luzerne', 'chanvre', 'lin', 'choux', 'luzerne', 
                'colza', "colza/navette d'hiver", "pois", "feverole", "lentilles", "haricots",
                "seigle", "triticale", "orge/escourgeon d'hiver", "ble tendre d hiver", "ble dur d'hiver", "ble hiver", "avoine d'hiver", 
                "mais", "mais grain", "orge/escourgeon de printemps", "mais semence", "ble dur de printemps", "sorgho",
                "avoine de printemps", "oignon", "echalote", "ble", "autre grande culture", "orge", "fourrages", "Cereales",
                "ble tendre", "avoine", "meteil", "proteagineux", "millet", "epeautre", "sarrasin", "mais fourrage",
                "ble tendre de printemps", "pomme de terre"), 
              RF=c('Ete', 'Ete', 'Ete', 'Ete', 'Ete', 'Ete', 'Ete', 'Ete', 
                'Printemps', "Printemps", "Printemps", "Printemps", "Printemps", "Printemps",
                "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", 
                "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible",
                "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible",
                "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible", "0Faible",
                "0Faible", "0Faible"))


Data_Parcels <- merge(x=Data_Parcels, RF, by.x="espcultiv", by.y="espcul", all.x=TRUE)
names(Data_Parcels)[length(Data_Parcels)] <-'espcultiv_RFS'
Data_Parcels <- merge(x=Data_Parcels, RF, by.x="pcultn1", by.y="espcul",all.x=TRUE)
names(Data_Parcels)[length(Data_Parcels)] <-'pcultn1_RFS'
Data_Parcels <- merge(x=Data_Parcels, RF, by.x="pcultn2", by.y="espcul",all.x=TRUE)
names(Data_Parcels)[length(Data_Parcels)] <-'pcultn2_RFS'




