# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/functions-common.R") # used in multiple reports
source("./scripts/graphs/graph-presets.R") # fonts, colors, themes 
source("./scripts/graphs/graphs-general.R")
# source("./scripts/graphs/graphs-specific.R")

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(ggplot2)
library(TabularManifest)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals ---------------------------------------------------------
print_format = "pandoc"
# ---- load-data ---------------------------------------------------------------
dto <- readRDS("./data/unshared/derived/dto.rds")
# each element this list is another list:
lapply(dto,names)
names(dto[["newc"]][["modeled"]]) # data recoved from SAVEDATA command
names(dto[["newc"]][["estimates"]]) # parameters of the model solution
names(dto[["newc"]][["info"]]) # fit and performance indices
names(dto[["newc"]][["ds_long"]])
# dto[["newc"]][["raw"]] # original data before estimation
modeled <- dto$newc$modeled
# ---- inspect-data -------------------------------------------------------------
temp <- dto$newc$ds_long

path <- dto$newc$path
model <- MplusAutomation::readModels(path)



# ---- tweak-data --------------------------------------------------------------
ds <- dto$newc$ds_long %>% 
  dplyr::mutate(
    educat        = car::Recode(edu,"
                              0:9  = '-1'; 
                             10:11 =  '0';
                             12:30 =  '1';
                             "),
    educat        = factor(educat,
                     levels = c(-1,             0,             1), 
                     labels = c("0-9 years", "10-11 years", ">11 years")),
    
    female        = factor(female,levels=c(0,1),labels=c("male","female")),
    ses_group     = factor(ses_group, levels=c(0,1),labels=c("low","high")),
    edu           = as.integer(edu),
    dementia_ever = factor(dementia_ever, levels = c(0,1),labels=c("Yes","No")),
    class         = factor(class)
  )%>% 
  dplyr::rename(mmse=value)


ds_bl <- ds %>% 
  dplyr::filter(wave==1,source=="observed") %>% 
  dplyr::select(-wave,-age, -time, -years_to_death, -source) 
  

# ---- descriptives --------------------------------------------------------------
cat("\n## female\n")
ds_bl %>% histogram_discrete("female")
cat("\n")
cat("\n## edu\n")
ds_bl %>% histogram_continuous("edu",bin_width = 1,rounded_digits = 2)
cat("\n")
cat("\n## educat\n")
ds_bl %>% histogram_discrete("educat")
cat("\n")
cat("\n## ses_group\n")
ds_bl %>% histogram_discrete("ses_group")
cat("\n")
cat("\n## dementia_ever\n")
ds_bl %>% histogram_discrete("dementia_ever")
cat("\n")
cat("\n## age_bl\n")
ds_bl %>% histogram_continuous("age_bl",bin_width = 1,rounded_digits = 2)
cat("\n")
cat("\n## years_to_death_bl\n")
ds_bl %>% histogram_continuous("years_to_death_bl",bin_width = 1,rounded_digits = 2)
cat("\n")
cat("\n## age_death\n")
ds_bl %>% histogram_continuous("age_death",bin_width = 1,rounded_digits = 2)
cat("\n")
# ---- solution-table ---------------------------
# MplusAutomation::LatexSummaryTable(model)
t <- model$summaries %>%  
  t() %>%
  as.data.frame() %>% 
  knitr::kable(col.names = c("")) %>% 
  print()

cat("\n")
ds_table <- model$parameters$unstandardized
cat("\n## Class 1 \n")
ds_table %>% 
  dplyr::filter(LatentClass=="1") %>% 
  knitr::kable(caption = "Solution for latent class 1", format = print_format)
cat("\n## Class 2 \n")
ds_table %>% 
  dplyr::filter(LatentClass=="2") %>% 
  knitr::kable(caption = "Solution for latent class 2", format = print_format)
cat("\n## Latent \n")
ds_table %>% 
  dplyr::filter(LatentClass=="Categorical.Latent.Variables") %>% 
  knitr::kable(caption = "Solution for Categorical Latent Variables", format = print_format)
cat("\n")

# ---- class-separation-info ----------------------------------
cat("\n")
model$class_counts
ds_bl %>% histogram_discrete("class",main_title = "Membership in latent classes")
# cat("\n")
# class_counts <- model$class_counts
# for(i in seq_along(class_counts)){
#   # i <- 1
#   cat("\n ",names(class_counts[i]))
#   class_counts[[i]] %>% knitr::kable() %>% print()
# }

# ---- class-separation-graphs ----------------------------------
cat("\n")
class_prob <- function(d,xaxis,group,group_label){
  # xaxis = "age_bl"
  # predictor = "ses_group"
  g <- ggplot2::ggplot(d, aes_string(x=xaxis,y="prob_class_2", fill = group))+
    geom_point(size=4, alpha = .5, shape=21)+
    facet_grid(class~.) + 
    labs(fill = group_label)+
    main_theme
  return(g)
}
# class_prob(ds_bl,"age_bl","female","Gender")

class_facet <- function(d,group,group_label){
  pm <- GGally::ggmatrix(
    list(
      "g1" = class_prob(d,"age_bl",group,group_label),
      "g2" = class_prob(d,"age_death",group,group_label),
      "g3" = class_prob(d,"years_to_death_bl",group,group_label)
    ), 
    nrow =1, ncol = 3, 
    xAxisLabels = c("Age at baseline","Age at death","Years to death at baseline"), 
    legend = 1,
    title = "Probability of membership in class 2"
  ) + theme(
    legend.position = "bottom"
    )
  pm
}
cat('\n##Gender\n')
class_facet(ds_bl, "female","Gender")
cat("\n")
cat('\n##SES\n')
class_facet(ds_bl, "ses_group","SES group")
cat("\n")
cat('\n##Dementia\n')
class_facet(ds_bl, "dementia_ever","History of Dementia") 
cat("\n")
cat('\n##Education\n')
class_facet(ds_bl, "educat","Years of Education") 
cat("\n")

# ---- observed-trajectories --------------------------------------------------------------
set.seed(42)
ids <- sample(unique(ds$id),50)
d_small <- ds %>% dplyr::filter(id %in% ids)
d_small <- ds

trajectory_matrix <- function(d,source,group,group_label){ 
  pm <- GGally::ggmatrix(   
    list(
      d %>% proto_line("age","mmse", source,group,group_label),
      d %>% proto_line("time","mmse", source,group,group_label),
      d %>% proto_line("years_to_death","mmse", source,group,group_label)
    ),
    nrow =1, 
    ncol = 3, 
    xAxisLabels = c("Age at visit","Time in study","Years to death"), 
    legend = 1,
    title = paste0("MMSE scores: ",toupper(source))
  ) + theme(
    legend.position = "bottom" 
  )  
  pm
}
cat("\n##Gender\n")
trajectory_matrix(d_small,"observed","female","Gender")
cat("\n")
cat("\n##SES\n")
trajectory_matrix(d_small,"observed", "ses_group","SES group")
cat("\n")
cat("\n##Dementia\n")
trajectory_matrix(d_small,"observed", "dementia_ever","History of Dementia") 
cat("\n")
cat("\n##Education\n")
trajectory_matrix(d_small,"observed", "educat","Years of Education") 
cat("\n")

# ---- modeled-trajectories --------------------------------------------------------------

cat("\n##Gender\n")
trajectory_matrix(d_small,"modeled","female","Gender")
cat("\n")
cat("\n##SES\n")
trajectory_matrix(d_small,"modeled", "ses_group","SES group")
cat("\n")
cat("\n##Dementia\n")
trajectory_matrix(d_small,"modeled", "dementia_ever","History of Dementia") 
cat("\n")
cat("\n##Education\n")
trajectory_matrix(d_small,"modeled", "educat","Years of Education") 
cat("\n")


# ---- factor-scores ------------------

# ---- publisher ---------------------------------------
path_report_1 <- "./reports/basic-review/review-newcastle.Rmd"
allReports <- c(
  path_report_1
  # ,path_report_2
  # , ...
)
pathFilesToBuild <- c(allReports) ##########
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}
