#!/usr/bin/env Rscript
# Collection of analysis functions associated with the MMODS Process


######################
# Preamble
# --------------------
require(tidyverse)
require(reshape)
require(ggplot2)
require(dplyr)
require(readr)





#### aggregate ####
# calculate aggregate CDF from multiple individual team CDFs
# dat: data.frame including the following columns
#       1. intervention
#       2. objective
#       3. value
#       4. id

calculate_aggregate = function(dat,ids, objectives, interventions, model_weights, mmods_round){
  # generate interpolated functions for each int-obj-id combod
  interp_functions = create_interp_fns(dat, ids, objectives, interventions)
  # calculate max and min  values across all teams for 
  # each intervention/objective pair
  lims <- dat %>% 
    group_by(intervention, objective) %>% 
    summarise(min = min(value, na.rm = TRUE), 
              max = max(value, na.rm = TRUE), .groups = 'drop')
  lims <- data.frame(lims)
  aggregate_all = calc_aggregate_cdf(dat, lims, interp_functions, ids, objectives, interventions, model_weights, mmods_round)
  aggregate_all = reformat_aggregate(aggregate_all)
  return(aggregate_all)
}

# function 
create_interp_fns <- function(df, ids, objectives, interventions){
  interp_functions <- cdfs <- list()
  for(i in ids){
    # Create lists for storing the CDF, PPF, and dataframe for each team
    interp_functions[[i]] <- list()
    # Loop through objectives
    for(obj in objectives){
      # Create container for each CDF, PPF for each obj/int combination
      interp_functions[[i]][[obj]] <- list()
      # Loop through interventions
      for(int in interventions){
        # Subset for the intervention of interest
        df_sub <- subset(df, intervention == int & id == i & objective == obj)
        # if all quantiles equal to single value, do not call approxfun() 
        # (some groups only entereted value in quantile 50 - thus NA check too)
        if(length(unique(df_sub$value))==1 | 
           (all(df_sub[df_sub$quantile != 50,"value"]==0) & df_sub[df_sub$quantile == 50,"value"]!=0) | 
           any(is.na(df_sub$value))){
          interp_functions[[i]][[obj]][[int]] <- NA
          next
        }
        interp_functions[[i]][[obj]][[int]] <- approxfun(x = df_sub$value, y = df_sub$quantile/100, 
                                                          method = "linear", yleft = 0, yright = 1, rule = 2, ties = mean)
      }
    }
  }
  return(interp_functions)
}


calc_aggregate_cdf <- function(dat, lims, interp_functions, ids, 
                    objectives, interventions, model_weights, mmods_round){
  # Create a list to store output
  out_agg <- list()
  # Loop over objectives
  for(obj in objectives){
    out_agg[[obj]] <- list()
    # Loop interventions
    for(int in interventions){
      cdf_out <- list()
      # Create a vector of values of the objective (of length 1000) over which to interpolate
      # take min/max across all teams for the current obj/int
      limits <- data.frame(lims[lims$intervention == int & lims$objective == obj,])
      x <- seq(limits[,"min"], limits[,"max"], length.out = 1000)
      for(i in ids){
        # for point estimates, assume cdf is step fn. from 0 to 1 at point.est value
        # interp_functions is usually a function, so this checks it is not a function (it'll be an NA)
        if( !is(interp_functions[[i]][[obj]][[int]], "function")){
          tmp <- rep(0, 1000)
          point.est <- subset(dat, id == i & quantile == 50 & intervention == int & objective == obj, "value")
          switch.x <- min(which(x >= as.numeric(point.est)))
          tmp[switch.x:1000] = 1
          cdf_out[[i]] <- tmp
          next
        }
        cdf_out[[i]] <- interp_functions[[i]][[obj]][[int]](x)
      }
      df_cdfs <- as.data.frame(cdf_out)
      df_cdfs[[obj]] <- x
      # Find long dataset (easier for plotting in ggplot)
      df_long <- melt(df_cdfs, id.vars = obj)
      names(df_long) <- c("objective", "variable", "value")
      # Average across team ID, 
      # multiple models from same group weighted equally
      df_long_weight = merge(df_long, model_weights,by.x = c("variable"),by.y = "letter_id")
      gdf_cdf <- df_long_weight %>% 
        group_by(objective) %>% 
        summarise(value = weighted.mean(value, weight), .groups = 'drop')
      # Convert back to a CDF function and interpolate for the quantiles we've asked for
      if(nrow(gdf_cdf) == 1){
        df_agg <- data.frame("quantile" = 0:100, "value" = gdf_cdf$value, 
                             "objective" = obj, "intervention" = int, 
                             "round" = mmods_round, "id" = "aggregate")
      }
      else{
        cdf_agg <- approxfun(x = gdf_cdf$value, y = gdf_cdf$objective, rule = 2, ties = mean)
        x <- 0:100
        df_agg <- data.frame("quantile" = x, "value" = cdf_agg(x/100), 
                             "objective" = obj, "intervention" = int, 
                             "round" = mmods_round, "id" = "aggregate")
      }
      out_agg[[obj]][[int]] <- df_agg
    }
  }
  return(out_agg)
}


reformat_aggregate <- function(out_agg){
  df_out_agg <- do.call(c, out_agg)
  df_out_agg <- do.call(rbind, df_out_agg)
  row.names(df_out_agg) <- NULL
  return(df_out_agg)
}

