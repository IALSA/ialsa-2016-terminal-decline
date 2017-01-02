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

# ---- understand-age -----------------
d <- ds0 %>%                    # from Stata file 
  dplyr::mutate(
    id                = person,
    age_at_bl         = bage,   # bage  = (date1-dob)/365.25
    age_at_bl_c       = bagec,  # bagec = bage-85.47
    age_at_death      = aged,   # aged  = (dateofdeath-dob)/365.25
    age_at_death_c    = agedc,  # agedc = aged-9.91
    years_to_death_1  = ttd1,   # ttd1  = (date1- dateofdeath)/365.25
    years_to_death_2  = ttd2,   # ttd2  = (date2- dateofdeath)/365.25
    years_to_death_3  = ttd3,   # ttd3  = (date3- dateofdeath)/365.25
    years_to_death_bl = TD1c    # TD1c  = ttd1-(-2.71)
  ) %>%
  dplyr::select(
   # id, age_at_bl, age_at_bl_c
    id, age_at_bl, age_at_death, years_to_death_1, years_to_death_bl
  ) %>% 
  dplyr::mutate(
    # age_center = age_at_bl -age_at_bl_c
    t = years_to_death_bl - 2.71
  ) 


head(d)

# ----- revert-tranformations ----------------------
ds_wide <- ds0 %>% 
  dplyr::mutate(
    id                = person,
    female            = mujer,
    # educc             = yrsedu - 7,
    SESgrp            = socclass,
    # SESgrp            = highsoccl,
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

temp <- ds_wide


# ---- prepare-for-mplus ---------------------
# prepare data to be read by MPlus
ds_mplus <- sapply(ds_wide,as.numeric) %>% as.data.frame()
ds_mplus[is.na(ds_mplus)] <- -9999 # replace NA with a numerical code
ds_mplus %>% dplyr::glimpse()




# ---- save-r-data -------------------
# tranformed data with supplementary variables
saveRDS(ds_wide,paste0(path_output,".rds"))
# prepared for Mplus
write.table(ds_mplus, paste0(path_output,".dat"), row.names=F, col.names=F)
write(names(ds_mplus), paste0(path_output,"-variable-names.txt"), sep=" ")


