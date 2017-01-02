get_class_size <- function(ls,study,class){
  # ls <- lst
  # study = "octo"
  # class = 1
  
  class_ <- as.character(class)
  class_size <- ls[[study]]$model[["class_counts"]][["mostLikely"]] %>% 
    dplyr::filter(class==class_) %>% 
    dplyr::select(count) %>% 
    as.integer()
  return(class_size)
}
# get_class_size(lst,"octo",1)

class_size_pretty <- function(lst,study,class){
  # study = "octo"
  class_ <- class
  class_size <- lst[[study]]$class_size[[class_]]
  total_size <- sum(as.numeric(lst[[study]]$class_size))
  class_pct <- round(class_size/total_size*100,0)
  sprintf(
    "(N= %4s, %2s%%)",class_size, class_pct
  )
}
# class_size_pretty(lst,"octo",2)

numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

get_estimate_table <- function(ls,study,class){
  # ls <- lst
  # class <- 1
  # study = "octo"
  
  class_ <- as.character(class)
  d1 <- lst[[study]]$model[["parameters"]][["unstandardized"]] %>% 
    dplyr::filter(LatentClass==class_)
  d2 <- lst[[study]]$stencil %>% 
    dplyr::left_join(d1,by=c("paramHeader","param")) %>% 
    dplyr::mutate(
      est_pretty  = numformat( est),
      se_pretty   = numformat( se),
      pval_pretty = ifelse(pval<.001,"<.001",numformat(pval)),
      dense = sprintf("%4s(%4s), %5s",est_pretty, se_pretty, pval_pretty),
      dense = ifelse(is.na(est),"",dense)
      
    )
  # temp <- d1
  # temp <- d2
  
  return(d2)
}