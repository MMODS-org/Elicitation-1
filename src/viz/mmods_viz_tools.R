# Collection of plotting functions associated with the MMODS Process


######################
# Preamble
# --------------------
require(tidyverse)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(reshape2)
require(formattable)

#### general utilities ####
# to add inset plot to one facet (used in aggregate plot)
# from: stackoverflow.com/questions/37867758/insetting-on-facet-grided-and-grid-arrangeed-plot
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

# to create empty grob
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

# extract legend from myggplot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#### MMODS specific figures ####

## plot pairwise comparisons, no ranges 
# df: data.frame, input data in long, format
# plot_val: string, name of column in df that contains values to be plotted
# facet_by: string, name of column in df to facet by
# facet_labs: vector, contains display names of facet variables
# range_lower: string, name of column in df that contains lower values of range to be plotted
# range_upper: string, name of column in df that contains lower values of range to be plotted
# flip_y_axis_text: binary, 1 if y axis text should be rotated 90 degrees
plot_pairwise_ranges = function(df, plot_val, facet_by, facet_labs,range_lower = NA, range_upper = NA, flip_y_axis_text = 0){
  df_cast = dcast(df, id ~ get(facet_by), value.var = plot_val)
  df_cast_lower = dcast(df, id ~ get(facet_by), value.var = range_lower)
  df_cast_upper = dcast(df, id ~ get(facet_by), value.var = range_upper)
  # create list of pairs to plot
  facets_unique = data.frame(names(facet_labs))
  num_facets = nrow(facets_unique)
  facets = expand.grid(x = 1:num_facets, y = 1:num_facets)
  facets = facets[facets$x  > facets$y,]
  # set plot specifications
  if(flip_y_axis_text == 1){angle = 90; hjust = 0.5}
  else{angle = 0; hjust = 1}
  lims = c(min(cbind(df_cast[,-1], df_cast_upper[,-1],df_cast_lower[,-1]),na.rm = TRUE),
           max(cbind(df_cast[,-1], df_cast_upper[,-1],df_cast_lower[,-1]),na.rm = TRUE))
  lims = lims*1.01
  # plot each pairwise comparison 
  pairwise_plots = list()
  counter = 1
  #browser()
  for(fct in 1:nrow(facets)){
    x_fct = facets[fct,"x"]
    y_fct = facets[fct,"y"]
    # create df to plot
    subset = df_cast[,c("id",as.character(facets_unique[x_fct,]),as.character(facets_unique[y_fct,]))]
    names(subset)[2:3] = c("x","y")
    subset$x_lower = df_cast_lower[,as.character(facets_unique[x_fct,])]
    subset$x_upper = df_cast_upper[,as.character(facets_unique[x_fct,])]
    subset$y_lower = df_cast_lower[,as.character(facets_unique[y_fct,])]
    subset$y_upper = df_cast_upper[,as.character(facets_unique[y_fct,])]
    # set axes specifications based on plot position
    x.axis.lab = element_blank()
    y.axis.lab = ""
    axis.text.x = element_text(color = "white")
    axis.text.y = element_text(color = "white", angle = angle)
    axis.ticks = element_line(color = "white")
    if(y_fct == 1){
      x.axis.lab = facet_labs[x_fct]
    }
    if(x_fct - y_fct == 1){
      axis.text.x = element_text()
      axis.text.y = element_text(angle = angle, hjust = hjust)
      axis.ticks = element_line()
      y.axis.lab = facet_labs[y_fct]
    }
    line_df = data.frame(x = 1:lims[2])
    if(round(lims[2]) == 1){line_df = data.frame(x = seq(0,lims[2], by =0.1))}
    #browser()
    pairwise_plots[[counter]] <- ggplot(data = subset, aes(x, y))+
      geom_line(data = line_df,aes(x = x, y = x), color = "darkgrey", linetype = "dotted")+
      geom_segment(aes(x = x_lower, xend = x_upper, y = y, yend = y), alpha = 0.5, color = "blue", size = 1)+
      geom_segment(aes(x = x, xend = x, y = y_lower, yend = y_upper), alpha = 0.5, color = "blue", size = 1)+
      geom_point(size = 3, alpha = 0.5, color = "blue")+
      ggtitle(x.axis.lab)+
      labs( y = y.axis.lab)+
      scale_x_continuous(limits = lims,labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
      scale_y_continuous(limits = lims,labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
      theme_bw(base_size = 11)+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5,size = 11),
            axis.title.x = element_blank(),
            axis.text.x = axis.text.x,
            axis.text.y = axis.text.y,
            axis.ticks = axis.ticks,
            plot.margin = margin(10,10,10,10))
    counter = counter + 1
  }
  return(pairwise_plots)
}



## function to plot pairwise comparisons, no ranges 
# df: data.frame, input data in long, format
# plot_val: string, name of column in df that contains values to be plotted
# facet_by: string, name of column in df to facet by
# color_by: vector, name of column in df to color data by
# legend_title: string, used to label legend, if included
# facet_labs: vector, contains display names of facet variables
# rev_y_axis: binary, 1 if reverse y axis
# flip_y_axis_text: binary, 1 if y axis text should be rotated 90 degrees
# jitter_flag: binary, 1 to jitter points
plot_pairwise_scatter = function(df, plot_val, facet_by, facet_labs, color_by = NA, legend_title = NA,
                                 rev_y_axis = 0, flip_y_axis_text = 0, jitter_flag = 0){
  if(is.na(color_by)){
    df_cast = dcast(df, id ~ get(facet_by), value.var = plot_val)
  }
  else{
    df_cast = dcast(df, id+get(color_by) ~ get(facet_by), value.var = plot_val)
    names(df_cast)[2] = "group"
    group_unique = data.frame(unique(df[,color_by]))
  }
  # create list of pairs to plot
  facets_unique = data.frame(names(facet_labs))
  num_facets = nrow(facets_unique)
  facets = expand.grid(x = 1:num_facets, y = 1:num_facets)
  facets = facets[facets$x  > facets$y,]
  # set plot specifications
  if(rev_y_axis == 1){y_func<-scale_y_reverse()}
  else {y_func<-scale_y_continuous()}
  if(flip_y_axis_text == 1){angle = 90; hjust = 0.5}
  else{angle = 0; hjust = 1}
  if(jitter_flag == 1){jitter_func<-position_jitter()}
  else{jitter_func<-position_identity()}
  # plot each pairwise comparison 
  pairwise_plots = list()
  counter = 1
  #browser()
  for(fct in 1:nrow(facets)){
    x_fct = facets[fct,"x"]
    y_fct = facets[fct,"y"]
    if(is.na(color_by)){
      subset = df_cast[,c("id",as.character(facets_unique[x_fct,]),as.character(facets_unique[y_fct,]))]
      names(subset)[2:3] = c("x","y")
      subset$group = "same"
    }
    else{
      subset = df_cast[,c("id","group",as.character(facets_unique[x_fct,]),as.character(facets_unique[y_fct,]))]
      names(subset)[3:4] = c("x","y")
    }
    x.axis.lab = element_blank()
    y.axis.lab = ""
    axis.text.x = element_text(color = "white")
    axis.text.y = element_text(color = "white", angle = angle)
    axis.ticks = element_line(color = "white")
    if(y_fct == 1){
      x.axis.lab = facet_labs[x_fct]
    }
    if(x_fct - y_fct == 1){
      axis.text.x = element_text()
      axis.text.y = element_text(angle = angle, hjust = hjust)
      axis.ticks = element_line()
      y.axis.lab = facet_labs[y_fct]
    }
    #browser()
    pairwise_plots[[counter]] <- ggplot(data = subset, aes(x, y, color = group))+
      geom_point(position = position_jitter(0.025),size = 2, alpha = 0.6)+
      ggtitle(x.axis.lab)+
      labs( y = y.axis.lab)+
      scale_color_brewer(palette = "Set1",labels = int.labs)+
      y_func+
      theme_bw(base_size = 14)+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5,size = 14),
            axis.title.x = element_blank(),
            axis.text.x = axis.text.x,
            axis.text.y = axis.text.y,
            axis.ticks = axis.ticks,
            plot.margin=grid::unit(rep(0.1,4), "mm"),
            panel.background = element_rect(size = 5))
    counter = counter + 1
  }
  p = ggplot(data = subset, aes(x, y, color = group))+
    geom_point(position = position_jitter())+
    ggtitle(x.axis.lab)+
    labs(y = y.axis.lab, color = legend_title)+
    scale_color_brewer(palette = "Set1",labels = int.labs)+
    y_func+
    theme_bw(base_size = 16)+
    theme(legend.position = "bottom",
          legend.direction = "vertical")
  #browser()
  return(list(pairwise_plots,p))
}

