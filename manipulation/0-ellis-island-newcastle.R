rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")

# source("./scripts/graphs/graph-presets.R") # pre-sets and options for graphing
# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
requireNamespace("readr")
requireNamespace("dplyr")
requireNamespace("tidyr")


# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

path_input  <- "./data/unshared/raw/newcastle/NewcastleTDclasses.txt"
path_output <- "./data/unshared/raw/newcastle/newcastle_terminal_decline"


# ---- load-data ---------------------------------------------------------------
ds0 <- read.table(path_input, quote="\"", comment.char="",header = T,stringsAsFactors=FALSE)
ds0[ds0 == -9999] <- NA
head(ds0)
temp <- ds0

# ----- revert-tranformations ----------------------
ds_wide <- ds0 %>% 
  dplyr::mutate(
    id                = person,
    female            = mujer,
    SESgrp            = highsoccl,
    smoker            = smoker,
    Demever           = everdem,
    age_at_bl         = bage,
    age_of_death      = aged, 
    years_to_death_bl = TD1c-2.71,
    ytdead            = years_to_death_bl
  ) %>% 
  dplyr::select(
    id,
    female,
    Demever,
    SESgrp,
    yrsedu,
    ytdead,
    bage,
    ttd1:ttd3,
    mmse1:mmse3
  )

# ---- prepare-for-mplus ---------------------
# prepare data to be read by MPlus
ds_mplus <- sapply(ds_wide,as.numeric) %>% as.data.frame()
ds_mplus[is.na(ds_mplus)] <- -9999 # replace NA with a numerical code
ds_mplus %>% dplyr::glimpse()




# ---- save-r-data -------------------
# this chunk save data in a general location, so you can copy-paste it to others
# tranformed data with supplementary variables
saveRDS(ds_wide,paste0(path_output,".rds"))
# prepared for Mplus
write.table(ds_mplus, paste0(path_output,".dat"), row.names=F, col.names=F)
write(names(ds_mplus), paste0(path_output,"-variable-names.txt"), sep=" ")


# ---- save-to-estimation ----------------
# this chunk saves data to a specific location, so you have greater accountability
custom_path <- "./manipulation/estimation/newcastle-2/newcastle_terminal_decline"
write.table(ds_mplus, paste0(custom_path,".dat"), row.names=F, col.names=F)
write(names(ds_mplus), paste0(custom_path,"-variable-names.txt"), sep=" ")

