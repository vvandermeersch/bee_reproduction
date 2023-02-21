###################################
### Prepare general parcel data ###
###################################

#Version Victor


#Setup----

source("functions/names_other_crops.R")
source("functions/unaccent.R")

#Read data----

Practices <- read.table(file = "data/raw_data/TableauPratiquesParcelles.txt",
                        header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")

Applications <- read.table(file = "data/raw_data/TableauTraitementsParcelles.txt",
                           header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")


Field <- read.table(file = "data/raw_data/TableauInfosExploitations.txt",
                    header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8", quote="")

Field$DEP <- Field$POSTCODE%/%1000


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
## Sommes des traitements----

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

Data_Parcels <- merge(Data_Parcels, Field[,c('ID_SGBD_parcelle', 'LATITUDE', 'LONGITUDE', 'coordcor', 'DEP')], 
                      by="ID_SGBD_parcelle")

Data_Parcels <- merge(Data_Parcels, subset(Landscape, select = -c(ANNEE_OBS)), 
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
Data_Parcels[Data_Parcels$AGEPRAIRIE>1900 & !is.na(Data_Parcels$AGEPRAIRIE),"AGEPRAIRIE"] <- Data_Parcels[Data_Parcels$AGEPRAIRIE>1900 & !is.na(Data_Parcels$AGEPRAIRIE),"ANNEE_OBS"]-Data_Parcels[Data_Parcels$AGEPRAIRIE>1900 & !is.na(Data_Parcels$AGEPRAIRIE),"AGEPRAIRIE"]

#landscape 
Data_Parcels$PAYSAGE <- factor(Data_Parcels$PAYSAGE, levels = c("Extensif","Intensif"))

