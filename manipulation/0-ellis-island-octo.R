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

path_input  <- "./data/unshared/raw/octo/OCTO-Twin_full.dat"
path_output <- "./data/unshared/raw/octo/octo_terminal_decline"


# ---- load-data ---------------------------------------------------------------
ds0 <- read.table(path_input, quote="\"", comment.char="", stringsAsFactors=FALSE)
ds0[ds0 == -9999] <- NA
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
colnames(ds0) <- varnames_octo
ds0[1,"Case"]<-1 # character glitch

head(ds0)
temp <- ds0

# ----- revert-tranformations ----------------------
ds_wide <- ds0 %>% 
  dplyr::mutate(
    Case                = Case,
    PairID            = PairID,
    female            = Female,
    yrsedu           = Educyrs,
    SESgrp            = SESgrp,
    Demever           = DemEver,
    bage              = CompAge1,
    years_to_death_bl = YTDead,
    dead              = Dead,
    ytdead            = YTDead
  ) %>% 
  dplyr::select(
    Case,
    PairID,
    female,
    Demever,
    SESgrp,
    yrsedu,
    ytdead,
    dead,
    tvdem1,
    bage,
    dedtime1:dedtime5,
    mmse1:mmse5
  )

temp <- ds_wide


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
custom_path <- "./manipulation/estimation/octo-2/octo_terminal_decline"
write.table(ds_mplus, paste0(custom_path,".dat"), row.names=F, col.names=F)
write(names(ds_mplus), paste0(custom_path,"-variable-names.txt"), sep=" ")

