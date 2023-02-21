
names_other_crops <- function(data){
  out <- data
  levels(out[, 2]) <-  c(levels(out[, 2]),"C?r?ales","fourrages","m?teil","oeillette","proteagineux", "millet","epeautre","sarrasin")
  for (i in 1:nrow(data)){
    if (data[i,"EC_CL"]%in%c("cereales","orge bl? tendre")){
        out[i, 2] <- "C?r?ales"
        out[i, 1] <- "C?r?ales"
    }
    if (data[i,"EC_CL"]%in%c("grand epeautre")){
      out[i, 2] <- "epeautre"
      out[i, 1] <- "C?r?ales"
    }
    if (data[i,"EC_CL"]%in%c("millet")){
      out[i, 2] <- "millet"
      out[i, 1] <- "C?r?ales"
    }
    if (data[i,"EC_CL"]%in%c("Sarrazin")){
      out[i, 2] <- "sarrasin"
      out[i, 1] <- "C?r?ales"
    }
    if (data[i,"EC_CL"]%in%c("Seigle")){
      out[i, 2] <- "seigle"
      out[i, 1] <- "C?r?ales"
    }
    if (data[i,"EC_CL"]%in%c("colza")){
      out[i, 2] <- "colza"
      out[i, 1] <- "ol?agineux"
    }
    if (data[i,"EC_CL"]%in%c("escourgeon","ray grass porte graine","fetuque","moha","sainfoin","trefle d alexandrie porte graines",
                             "Triticale")){
      out[i, 2] <- "fourrages"
      out[i, 1] <- "fourrages"
    }
    if (data[i,"EC_CL"]%in%c("m?lange : triticale, avoine, f?verole","m?lange c?r?alier, tr?fle",
                             "grand m?teil (f?verolle, avoine, pois,, triticale, vesce")){
      out[i, 2] <- "m?teil"
      out[i, 1] <- "fourrages"
    }
    if (data[i,"EC_CL"]%in%c("Haricot","haricot blanc, flageolets","harricot blanc")){
      out[i, 2] <- "haricots"
      out[i, 1] <- "prot?agineux"
    }
    if (data[i,"EC_CL"]%in%c("Lentille","Lentilles AB", "lentilles")){
      out[i, 2] <- "lentilles"
      out[i, 1] <- "prot?agineux"
    }
    if (data[i,"EC_CL"]%in%c("oeillette","oeillette (que cette culture, pas de betterave, mais ne fonctionne pas dans le champs d?di?s ci-dessus))")){
      out[i, 2] <- "oeillette"
      out[i, 1] <- "ol?agineux"
    }
    if (data[i,"EC_CL"]%in%c("pois","Pois")){
      out[i, 2] <- "pois"
      out[i, 1] <- "prot?agineux"
    }
    if (data[i,"EC_CL"]%in%c("proteagineux")){
      out[i, 2] <- "proteagineux"
      out[i, 1] <- "prot?agineux"
    }
    if (data[i,"EC_CL"]%in%c("soja bth")){
      out[i, 2] <- "soja"
      out[i, 1] <- "prot?agineux"
    }
    if (data[i,"EC_CL"]%in%c("tournesol")){
      out[i, 2] <- "tournesol"
      out[i, 1] <- "ol?agineux"
    }
  }
  return(out)
}
