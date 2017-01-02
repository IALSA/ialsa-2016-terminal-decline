
modeled_wide <- structure(
  list(
    id = 2L, 
    BPAGEC = -0.279, 
    YTDEADC = -0.982, 
    DTIMEC1 = -1.692,
    DTIMEC2 = -0.261,
    DTIMEC3 = 1.307
  ), 
  class = "data.frame", 
  row.names = c(NA,-1L), 
  .Names = c("id", "BPAGEC", "YTDEADC", "DTIMEC1", "DTIMEC2", "DTIMEC3")
)

modeled_long <- modeled_wide %>% 
  tidyr::gather_(key="g",value="value",c("DTIMEC1","DTIMEC2","DTIMEC3")) %>% 
  dplyr::mutate(
    varname = gsub("(\\w+)(\\d+)$", "\\1", g, perl=T),
    wave    = gsub("(\\w+)(\\d+)$", "\\2", g, perl=T)
  ) %>% 
  dplyr::rename(DTIMEC = value) %>% 
  dplyr::select(id, BPAGEC, YTDEADC,wave,DTIMEC)


observed_wide <- structure(
  list(
    person = 2L, 
    bage = 85.12095,
    bagec = -0.3490512, 
    ttd1 = -3.692191, 
    ttd2 = -2.261465, 
    ttd3 = -0.6926762, 
    TD1c = -0.9821914, 
    aged = 88.81314, 
    agedc = 78.90314
  ), 
  class = "data.frame", 
  row.names = c(NA, -1L), 
  .Names = c("person", "bage", "bagec", "ttd1", "ttd2", "ttd3", "TD1c", "aged", "agedc")
)

observed_long <- observed_wide %>% 
  tidyr::gather_(key="g",value="value",c("ttd1","ttd2","ttd3")) %>% 
  dplyr::mutate(
    varname = gsub("(\\w+)(\\d+)$", "\\1", g, perl=T),
    wave    = gsub("(\\w+)(\\d+)$", "\\2", g, perl=T)
  ) %>% 
  dplyr::rename(ttd = value) %>% 
  dplyr::select(person, bage, bagec, TD1c,aged, agedc,wave, ttd)


# assign aliases
ow <- observed_wide
ol <- observed_long
mw <- modeled_wide
ml <- modeled_long

# Below is an outtake from the Mplus input file

# DEFINE: BPagec=bage-85.4;
# dtimec1=ttd1+2;
# dtimec2=ttd2+2;
# dtimec3=ttd3+2;
# educ=educc;
# ytdeadc=TD1c;

# Above is an outtake from the Mplus input file

print(observed_long)
# recover age at baseline
ml$age_bl <- ml$BPAGEC + 85.4
ml; 
ol$bage[1]
# recover time to death
ml$time_to_death <- ml$DTIMEC -2
ml

# recover years to death at study entry
ml$time_to_death_bl <- ml$YTDEADC + 6
ml

# compute time after study entry in the observed data
ol$time <- ol$aged + ol$ttd - ol$bage
ol


