rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
source("./scripts/functions-tables.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2)
library(MplusAutomation)
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")


# ---- declare-globals ---------------------------------------------------------
path_newc_out   <- "./data/unshared/raw/originals/2classesage.out"
path_octo_out <- "./data/unshared/raw/originals/redo/for paper MMSe octo QTD.out"

path_stencil_newcastle <- "./data/shared/raw/table-stencil-newcastle.csv"
path_stencil_octo      <- "./data/shared/raw/table-stencil-octo.csv"

baseSize <- 12
# ---- load-data ---------------------------------------------------------------
# octo <- MplusAutomation::readModels(path_octo_out)
# newc <- MplusAutomation::readModels(path_newc_out)
ls_model <- list(
  "newcastle" = MplusAutomation::readModels(path_newc_out),
  "octo"      = MplusAutomation::readModels(path_octo_out)
)
stencil_newcastle <- readr::read_csv(path_stencil_newcastle)
stencil_octo <- readr::read_csv(path_stencil_octo)

# readr::write_csv(octo$parameters$unstandardized,"./data/shared/raw/octo-est.csv")

# ---- create-list-objects -------------------

dummy <- list(
  "study_label" = NA,
  "model"       = NA,
  "stencil"     = NA,
  "estimates"   = list("1"=NA,"2"=NA),
  "class_size" = list("1"=NA,"2"=NA),
  "class_labels" = NA
)

lst <- list(
  "newcastle" = dummy,
  "octo"      = dummy
)
lapply(lst,names)

lst$newcastle$study_label = "Newcastle 85+"
lst$newcastle$model <- MplusAutomation::readModels(path_newc_out)
lst$newcastle$stencil <- readr::read_csv(path_stencil_newcastle)
lst$newcastle$class_labels <- c("2" = "Slow decliners", "1" = "Fast decliners")

lst$octo$study_label <- "OCTO-Twin"
lst$octo$model <- MplusAutomation::readModels(path_octo_out)
lst$octo$stencil <-  readr::read_csv(path_stencil_octo)
lst$octo$class_labels <- c("1" = "Slow decliners", "2" = "Fast decliners")

# ---- functions ----------------------

# get_estimate_table(lst,"octo",1)

# ---- populate-list-object --------------------------

for(study in c("newcastle","octo")){
  for(class in c(1,2)){
    lst[[study]][["estimates"]][[paste0(class)]] <- get_estimate_table(lst,study,class)
    lst[[study]][["class_size"]][[paste0(class)]] <- get_class_size(lst,study,class)
  }#close class
}#close study
lapply(lst,names)


# ---- assemble-tables -----------------------------


one_column <- function(lst,study,class){
  # study = "octo"
  # class = 1
  # 
  class_ = as.character(class)
  l <- list(
    lst[[study]]$study_label,
    lst[[study]]$class_labels[class_] %>% as.character(),
    class_size_pretty(lst,study,class),
    get_estimate_table(lst,study,class) %>% dplyr::select(effect,label,dense)
  )
  n <- do.call(rbind,l)
  n[1:3,1:2] <- ""
  return(n)
}
one_column(lst,"octo",1)

l <- list(
  one_column(lst, "newcastle",1) %>% dplyr::select(effect,label),
  one_column(lst,"newcastle",1) %>% dplyr::select(dense),
  one_column(lst,"newcastle",2)%>% dplyr::select(dense),
  one_column(lst,"octo",2)%>% dplyr::select(dense),
  one_column(lst,"octo",1)%>% dplyr::select(dense)
)
t1 <- do.call(cbind,l) %>% as.data.frame()





