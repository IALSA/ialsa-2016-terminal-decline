plot_trajectories <- function(d,sample_size=200){
  set.seed(42)
  ids <- sample(unique(d$id),sample_size)
  g <-  d %>% 
    ggplot2::ggplot(aes(x=DTIMEC,y=Y,color=class)) +
    geom_line(aes(group=id), size=.8, alpha=.7)+
    labs(y="MMSE",x="Time until death",color="Latent class")+
    scale_colour_manual(values=colors_classes)+
    main_theme#+
  # theme(text = element_text(size=baseSize+6))
}

class_trajectories <- function(d,group,group_label,color_scale,sample_size=200){
  # d <- ds_long_newc
  # group <- "female" 
  set.seed(42)
  ids <- sample(unique(d$id),sample_size)
  g <-  d %>% dplyr::filter(id %in% ids) %>% 
    ggplot2::ggplot(aes_string(x="DTIMEC",y="Y",color=group)) +
    geom_line(aes(group=id),size=.8,alpha=.7)+
    facet_grid(.~class)+
    scale_color_manual(values = color_scale)+
    labs(y="MMSE",x="Time until death", color = group_label)+
    main_theme+
    theme(text = element_text(size=baseSize+4))
  g
}

class_traj_matrix <- function(newc,octo,group,group_label,color_scale){
  pm <- GGally::ggmatrix(
    list(
      "newc" = class_trajectories(newc,group,group_label,color_scale),
      "octo" = class_trajectories(octo,group,group_label,color_scale)
    ),
    nrow = 1, ncol = 2,
    title = "MMSE trajectories plotted as a function of years to death\n(intercept placed at two years before death)", 
    xAxisLabels = c("Newcastle 85+","OCTO-Twin"), 
    yAxisLabels = "MMSE score",
    legend = 2
  ) + theme(
    legend.position = "bottom"
    
  )
  pm
}


# ------ black-and-white-version ------------------------
plot_trajectories_bw <- function(
  d,
  sample_size=200
){
  # d <- ds_long_newc
  # d <- ds_long_octo
  if(sample_size=="max"){
    ids <- unique(d$id)
    sample_size_ <- length(ids)
  }else{
    set.seed(42)
    ids <- sample(unique(d$id),sample_size)
  }
  dd <- d %>% dplyr::filter(id %in% ids)
  g <-  dd %>% 
    ggplot2::ggplot(aes(x=DTIMEC,y=Y,color=class)) +
    geom_line(aes(group=id), size=.8, alpha=.3, color="black")+
    facet_grid(class~.) +
    # coord_cartesian(xlim=c(-15,2))+
    labs(y="MMSE",x="Time until death")+
    # scale_colour_manual(values=colors_classes)+
    main_theme#+
  
  # theme(text = element_text(size=baseSize+6))
}
# plot_trajectories_bw(ds_long_newc ) %>% print()
# plot_trajectories_bw(ds_long_newc, facet_="vertical") %>% print()



# class_trajectories_bw <- function(d,group,sample_size="max"){
class_trajectories_bw <- function(d,sample_size="max"){
  # d <- ds_long_newc
  # group <- "female" 
  if(sample_size=="max"){
    ids <- unique(d$id)
    sample_size_ <- length(ids)
  }else{
    set.seed(42)
    ids <- sample(unique(d$id),sample_size)
  }
  dd <- d %>% dplyr::filter(id %in% ids)
  g <- dd %>% 
    ggplot2::ggplot(aes_string(x="DTIMEC",y="Y")) +
    # ggplot2::ggplot(aes_string(x="DTIMEC",y="Y",color=group)) +
    geom_line(aes(group=id),size=.8,alpha=.3, color="black")+
    facet_grid(.~class)+
    # scale_color_manual(values = color_scale)+
    labs(y="MMSE",x="Time until death")+
    main_theme+
    theme(text = element_text(size=baseSize+4))
  g

}


class_traj_matrix_bw <- function(newc,octo){
  pm <- GGally::ggmatrix(
    list(
      "newc" = class_trajectories_bw(newc),
      "octo" = class_trajectories_bw(octo)
    ),
    nrow = 1, ncol = 2,
    title = "Figure 3: MMSE trajectories plotted as a function of years to death by class\n(intercept placed at two years before death)", 
    xAxisLabels = c("Newcastle 85+","OCTO-Twin"), 
    yAxisLabels = "MMSE score"
    # legend = 2
  ) + theme(
    legend.position = "bottom"
    
  )
  pm
}