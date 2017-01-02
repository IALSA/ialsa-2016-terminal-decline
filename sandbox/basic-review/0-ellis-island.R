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

path_data_newc  <- "./data/unshared/raw/newcastle/NewcastleTDclasses.txt"
path_newc_out   <- "./manipulation/estimation/newcastle/newcastle-mmse.out"
path_newc_gh5   <- "./manipulation/estimation/newcastle/newcastle-mmse.gh5"


path_data_octo   <- "./data/unshared/raw/octo/OCTO-Twin_full.dat"
path_octo_gh5 <- "./manipulation/estimation/octo/octo-mmse.gh5"
path_octo_out <- "./manipulation/estimation/octo/octo-mmse.out"



# ---- load-data ---------------------------------------------------------------
ds_newc <- read.table(path_data_newc, quote="\"", comment.char="",header = T,stringsAsFactors=FALSE)
ds_octo<- read.table(path_data_octo, quote="\"", comment.char="", stringsAsFactors=FALSE)


# ----- get-octo-varnames -----------------------
string <- "Case PairID TwinID Zygosity Female
Educyrs EducCat SESgrp Resgrp Smoke
SESChild SESChOth HlthLif CogPrev
LonelyE Dead DeadAge YTDead TotDem
DemEver DemType DemAge YTDem
time1 time2 time3 time4 time5
dedtime1 dedtime2 dedtime3 dedtime4
dedtime5 tvdead1 tvdead2 tvdead3 tvdead4
tvdead5 demtime1 demtime2 demtime3 demtime4
demtime5 tvdem1 tvdem2 tvdem3 tvdem4 tvdem5
CompAge1 CompAge2 CompAge3 CompAge4 CompAge5
Marital1 Marital2 Marital3 Marital4 Marital5
weight1 weight2 weight3 weight4 weight5
height1 height2 height3 height4 height5
bmi1 bmi2 bmi3 bmi4 bmi5 SRhlth1 SRhlth2
SRhlth3 SRhlth4 SRhlth5 living1 living2
living3 living4 living5 SR2hlth1 SR2hlth2
SR2hlth3 SR2hlth4 SR2hlth5 SROhlth1 SROhlth2
SROhlth3 SROhlth4 SROhlth5 HlthPrv1 HlthPrv2
HlthPrv3 HlthPrv4 HlthPrv5 HlthTra1 HlthTra2
HlthTra3 HlthTra4 HlthTra5 CogPres1 CogPres2
CogPres3 CogPres4 CogPres5 AGames1 AGames2
AGames3 AGames4 AGames5 ACross1 ACross2
ACross3 ACross4 ACross5 ALiter1
ALiter2 ALiter3 ALiter4 ALiter5
AWrite1 AWrite2 AWrite3 AWrite4
AWrite5 AStudy1 AStudy2 AStudy3
AStudy4 AStudy5 AOther1 AOther2
AOther3 AOther4 AOther5 LonelyP1
LonelyP2 LonelyP3 LonelyP4
LonelyP5 FrTalk1 FrTalk2 FrTalk3
FrTalk4 FrTalk5 FrPart1 FrPart2
FrPart3 FrPart4 FrPart5 LackCo1
LackCo2 LackCo3 LackCo4 LackCo5
Abandon1 Abandon2 Abandon3 Abandon4
Abandon5 NumFr1 NumFr2 NumFr3
NumFr4 NumFr5 NumPref1 NumPref2
NumPref3 NumPref4 NumPref5 SeeChGr1
SeeChGr2 SeeChGr3 SeeChGr4
SeeChGr5 pek1 pek2 pek3 pek4 pek5
gripp1 gripp2 gripp3 gripp4 gripp5
chr5tim1 chr5tim2 chr5tim3
chr5tim4 chr5tim5 padl1 padl2 padl3
padl4 padl5 iadl1 iadl2 iadl3 iadl4
iadl5 digspf1 digspf2 digspf3 digspf4
digspf5 digspb1 digspb2 digspb3 digspb4
digspb5 prose1 prose2 prose3 prose4 prose5
block1 block2 block3 block4 block5
info1 info2 info3 info4 info5
synnum1 synnum2 synnum3 synnum4
synnum5 figure1 figure2 figure3 figure4
figure5 digsym1 digsym2 digsym3 digsym4
digsym5 psif1 psif2 psif3 psif4 psif5
mirnam1 mirnam2 mirnam3 mirnam4 mirnam5
mirrcl1 mirrcl2 mirrcl3 mirrcl4 mirrcl5
mirrcg1 mirrcg2 mirrcg3 mirrcg4 mirrcg5
mircor1 mircor2 mircor3 mircor4 mircor5
clock1 clock2 clock3 clock4 clock5 mmse1
mmse2 mmse3 mmse4 mmse5 mismmse1 mismmse2
mismmse3 mismmse4 mismmse5 filtmmse1 filtmmse2
filtmmse3 filtmmse4 filtmmse5 highBP1 highBP2
highBP3 highBP4 highBP5 sbp1 sbp2
sbp3 sbp4 sbp5 dbp1 dbp2 dbp3 dbp4 dbp5
diab1 diab2 diab3 diab4 diab5 heart1 heart2
heart3 heart4 heart5 stroke1 stroke2 stroke3
stroke4 stroke5 p_cncr1 p_cncr2 p_cncr3
p_cncr4 p_cncr5 b_cncr1 b_cncr2 b_cncr3 b_cncr4
b_cncr5 cncr1 cncr2 cncr3 cncr4 cncr5 bpever
diabever hrtever strkever cncrever comorbid
cesdm1 cesdm2 cesdm3 cesdm4 cesdm5 ExamDate
BPever2 ageHBP1 ageHBP2 ageHBP3 ageHBP4
ageHBP5 AgeHBP TimeHBP TTHBP1 TTHBP2
TTHBP3 TTHBP4 TTHBP5
"
varnames_octo <- strsplit(string," |(\r\n|\r|\n)")[[1]]

# ---- tweak-data --------------------------------------------------------------
colnames(ds_octo) <- varnames_octo
colnames(ds_newc)



# ---- estract-estimates -----------------------------------
get_model_results <- function(path){
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
age_center <- 83
dtime_adjustment <- 2
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
  dplyr::mutate(
    y_fs_1 = I + S*DTIMEC + Q*DTIMEC,
    y_fs_2 = C_I + C_S*DTIMEC + C_Q*DTIMEC,
    y_fs   = ifelse(C==1,y_fs_1,ifelse(C==2,y_fs_2,NA))
  ) %>% 
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

  ) 

# verification
temp <- ds_long %>% dplyr::select(
  id, wave,BPAGEC, YTDEADC,DTIMEC,years_to_death, years_to_death_bl, age_bl, time, age,age_death
) %>% dplyr::filter(id==2)

ds_octo %>%
  dplyr::select(Case,PairID,DeadAge,YTDead, time1:time5,dedtime1:dedtime5,CompAge1:CompAge5, mmse1) %>%
  dplyr::filter(Case==2) %>% t()

head(ds_long)
length(unique(ds_long$id)) # should match nrow(ds_wide)
temp <- ds_long # for review in the window
ls_octo[["ds_long"]] <- ds_long


##### NEWCASTLE
age_center <- 85.4
dtime_adjustment <- 2
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
  # compute predicted scores
  dplyr::mutate(
    y_fs_1 = I + S*DTIMEC,# + Q*DTIMEC,
    y_fs_2 = C_I + C_S*DTIMEC,# + C_Q*DTIMEC,
    y_fs   = ifelse(C==1,y_fs_1,ifelse(C==2,y_fs_2,NA))
  ) %>% 
  dplyr::mutate(
    age_bl             = BPAGEC + age_center
    ,years_to_death     = DTIMEC - dtime_adjustment
    ,years_to_death_bl  = YTDEADC# - death_center # adjusted prior to fitting
    ,time               = -(years_to_death_bl - years_to_death)
    ,age                = age_bl + time
    ,age_death          = age_bl - years_to_death
    # ,edu                = EDUC + edu_center
  ) 

# verification 
# temp <- ds_long %>% dplyr::select(
#   id, wave, age_bl, years_to_death_bl,years_to_death,time, age,age_death,BPAGEC, YTDEADC,DTIMEC
# ) %>% dplyr::filter(id==2)
# 
# ds_newc %>% 
#   dplyr::select(person, bage, bagec, ttd1:ttd3, TD1c, aged, agedc) %>% 
#   dplyr::filter(person==2) %>% t()

head(ds_long)
length(unique(ds_long$id)) # should match nrow(ds_wide)
temp <- ds_long # for review in the window
ls_newc[["ds_long"]] <- ds_long



# ---- save-to-disk ------------------------------------------------------------
# attach the raw data to the objects
ls_newc[["raw"]] <- ds_newc
ls_octo[["raw"]] <- ds_octo 

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



