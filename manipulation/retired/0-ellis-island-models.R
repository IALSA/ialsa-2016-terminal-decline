rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")
# source("./scripts/mplus/extraction-functions.R")   # parsing functions
source("./scripts/mplus/mplus.R")                  # working with Mplus object

source("./scripts/graphs/model-anatomy-functions.R")# treating single outputs
# source("./scripts/table-assembly-functions.R")      # working with compound catalogs

source("./scripts/graphs/graph-presets.R") # pre-sets and options for graphing
# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2)
library(MplusAutomation)
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")


# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

path_data_newc  <- "./data/unshared/raw/newcastle/newcastle_terminal_decline.rds"
path_newc_out   <- "./manipulation/estimation/newcastle/newcastle-mmse-k-2.out"
path_newc_gh5   <- "./manipulation/estimation/newcastle/newcastle-mmse-k-2.gh5"


path_data_octo   <- "./data/unshared/raw/octo/octo_terminal_decline.rds"
path_octo_out <- "./manipulation/estimation/octo/octo-mmse-k-2.out"
path_octo_gh5 <- "./manipulation/estimation/octo/octo-mmse-k-2.gh5"

modeled <- MplusAutomation::getSavedata_Data(path_octo_out)
modeled <- MplusAutomation::getSavedata_Data(path_newc_out)

# ---- load-data ---------------------------------------------------------------
ds_newc <- readRDS(path_data_newc)
ds_octo<- readRDS(path_data_octo)


# ---- estract-estimates -----------------------------------
get_model_results <- function(path){
  # path <- path_octo_out
  # path <- path_newc_out
  modeled <- MplusAutomation::getSavedata_Data(path)
  estimates <- MplusAutomation::extractModelParameters(target=path_octo_out)$unstandardized
  info <- MplusAutomation::extractModelSummaries(target=path_octo_out)
  ls <- list(
    "path"     = path,       # path to the modeled object
    "modeled"  = modeled,    # data recovered from SAVEDATA command
    "estimates"= estimates,  # tabulated coefficients
    "info"     = info        # model preformance
  )
  return(ls)
}
ls_octo <- get_model_results(path_octo_out)
ls_newc <- get_model_results(path_newc_out)

lapply(ls_octo, names)  
lapply(ls_newc, names)  

# ------ standardization-options ----------------------

# ---- groom-recovered-data --------------

##### OCTO
age_center <- 85
dtime_adjustment <- 0
edu_center <- 7
death_center <- 6 # years to death from study entry

ds_wide <- ls_octo$modeled
head(ds_wide)
ds_wide$id <- 1:nrow(ds_wide) # create person id
ds_wide <- ds_wide %>% dplyr::arrange(id) # sort for visual inspection
head(ds_wide); nrow(ds_wide)
(variables_longitudinal <- c(paste0("Y",1:5),paste0("DTIMEC",1:5)))
(variables_static <- setdiff(colnames(ds_wide), variables_longitudinal))
ds_long <- ds_wide %>% 
  tidyr::gather_(key="g",value="value",variables_longitudinal) %>% 
  dplyr::mutate(
    varname = gsub("(\\w+)(\\d+)$", "\\1", g, perl=T),
    wave    = gsub("(\\w+)(\\d+)$", "\\2", g, perl=T)
  ) %>% 
  dplyr::select(-g) %>% 
  dplyr::arrange(id) %>% 
  tidyr::spread(key=varname, value=value) %>%
  # compute predicted scores
  # dplyr::mutate(
  #   y_fs_1 = I + S*DTIMEC + Q*DTIMEC,
  #   y_fs_2 = C_I + C_S*DTIMEC + C_Q*DTIMEC,
  #   y_fs   = ifelse(C==1,y_fs_1,ifelse(C==2,y_fs_2,NA))
  # ) %>%
  # recenter and recover time
  dplyr::mutate(
    age_bl             = BPAGEC + age_center,
    years_to_death_bl  = YTDEADC - death_center,
    years_to_death     = DTIMEC - dtime_adjustment,
    time               = -(years_to_death_bl - years_to_death),
    age                = age_bl + time,
    age_death          = age_bl - years_to_death_bl,
    edu                = EDUC + edu_center
  ) %>% 
  # compute predicted scores
  dplyr::mutate(
    y_fs_1 = I + S*time + Q*time,
    y_fs_2 = C_I + C_S*time + C_Q*time,
    y_fs   = ifelse(C==1,y_fs_1,ifelse(C==2,y_fs_2,NA))
  ) %>%
  dplyr::select(-y_fs_1, -y_fs_2) %>% 
  # dplyr::select(-BPAGEC,-YTDEADC,-DTIMEC) %>% 
  # rename for consistency
  dplyr::rename(
    id_twin           = PAIRID,
    female            = FEMALE,
    ses_group         = SESGRP,
    dementia_ever     = DEMEVER,
    level_class_1     = I,
    slope_class_1     = S,
    quadratic_class_1 = Q,
    level_class_2     = C_I,
    slope_class_2     = C_S,
    quadratic_class_2 = C_Q,
    prob_class_1      = CPROB1,
    prob_class_2      = CPROB2,
    class             = C,
    y_observed        = Y,
    y_modeled         = y_fs
  ) %>%  
  tidyr::gather_(key="source","value",c("y_observed","y_modeled")) %>% 
  dplyr::mutate(
    source = gsub("y_","",source)
  ) %>% 
  dplyr::select(
    id,id_twin, 
    age_bl,age_death,years_to_death_bl,
    female, ses_group, edu, dementia_ever,
    class,
    level_class_1, slope_class_1, quadratic_class_1,
    level_class_2, slope_class_2, quadratic_class_2,
    prob_class_1, prob_class_2,    
    wave,age, time, years_to_death,
    source, value
  )
# temp <- ds_long
# head(ds_long)
length(unique(ds_long$id)) # should match nrow(ds_wide)
# temp <- ds_long # for review in the window
ls_octo[["ds_long"]] <- ds_long


##### NEWCASTLE
age_center <- 85
dtime_adjustment <- 0
edu_center <- 7
death_center <- 6 # years to death from study entry

ds_wide <- ls_newc$modeled
head(ds_wide)
ds_wide$id <- 1:nrow(ds_wide) # create person id
ds_wide <- ds_wide %>% dplyr::arrange(id) # sort for visual inspection
head(ds_wide); nrow(ds_wide)
(variables_longitudinal <- c(paste0("Y",1:3),paste0("DTIMEC",1:3)))
(variables_static <- setdiff(colnames(ds_wide), variables_longitudinal))
ds_long <- ds_wide %>% 
  tidyr::gather_(key="g",value="value",variables_longitudinal) %>% 
  dplyr::mutate(
    varname = gsub("(\\w+)(\\d+)$", "\\1", g, perl=T),
    wave    = gsub("(\\w+)(\\d+)$", "\\2", g, perl=T)
  ) %>% 
  dplyr::select(-g) %>% 
  dplyr::arrange(id) %>% 
  tidyr::spread(key=varname, value=value) %>%
  # # compute predicted scores
  # dplyr::mutate(
  #   y_fs_1 = I + S*DTIMEC,# + Q*DTIMEC,
  #   y_fs_2 = C_I + C_S*DTIMEC,# + C_Q*DTIMEC,
  #   y_fs   = ifelse(C==1,y_fs_1,ifelse(C==2,y_fs_2,NA))
  # ) %>% 
  # recenter and recover time
  dplyr::mutate(
    age_bl             = BPAGEC + age_center,
    years_to_death_bl  = YTDEADC - death_center,
    years_to_death     = DTIMEC - dtime_adjustment,
    time               = -(years_to_death_bl - years_to_death),
    age                = age_bl + time,
    age_death          = age_bl - years_to_death_bl,
    edu                = EDUC + edu_center
  ) %>% 
  # compute predicted scores
  dplyr::mutate(
    y_fs_1 = I + S*time,# + Q*time,
    y_fs_2 = C_I + C_S*time,# + C_Q*time,
    y_fs   = ifelse(C==1,y_fs_1,ifelse(C==2,y_fs_2,NA))
  ) %>%
  dplyr::select(-y_fs_1, -y_fs_2) %>% 
  # dplyr::select(-BPAGEC,-YTDEADC,-DTIMEC) %>% 
  # rename for consistency
  dplyr::rename(
    female            = FEMALE,
    ses_group         = SESGRP,
    dementia_ever     = DEMEVER,
    level_class_1     = I,
    slope_class_1     = S,
    level_class_2     = C_I,
    slope_class_2     = C_S,
    prob_class_1      = CPROB1,
    prob_class_2      = CPROB2,
    class             = C,
    y_observed        = Y,
    y_modeled         = y_fs
  ) %>%  
  tidyr::gather_(key="source","value",c("y_observed","y_modeled")) %>% 
  dplyr::mutate(
    source = gsub("y_","",source)
  ) %>% 
  dplyr::select(
    id,
    age_bl,age_death,years_to_death_bl,
    female, ses_group, edu, dementia_ever,
    class,
    level_class_1, slope_class_1,
    level_class_2, slope_class_2,
    prob_class_1, prob_class_2,    
    wave,age, time, years_to_death,
    source, value
  )
# head(ds_long)
length(unique(ds_long$id)) # should match nrow(ds_wide)
# temp <- ds_long # for review in the window
ls_newc[["ds_long"]] <- ds_long



# ---- save-to-disk ------------------------------------------------------------
# attach the raw data to the objects
# ls_newc[["raw"]] <- ds_newc
# ls_octo[["raw"]] <- ds_octo 

dto <- list(
  "octo" = ls_octo,
  "newc" = ls_newc
)

# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/dto.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/dto.rds")
# each element this list is another list:
lapply(dto,names)
lapply(dto[["octo"]],names)



