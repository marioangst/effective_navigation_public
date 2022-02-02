# conditional effects --------

plot_cond_effects <- function(main_var, cond_var, model_fit, cond_labels, 
                              resp = NULL, log_x = TRUE){
  
  #condition over 80% of observed
    conds <-
      modelr::data_grid(modeling_df,
                        {{cond_var}} := quantile(modeling_df[[cond_var]],
                                                 probs = c(0.1,0.5,0.9)))
  
  if(!is.null(resp)){ #multivariate case
    
    plot_df <-
      brms::conditional_effects(model_fit, effects = as.character(main_var),
                                re_formula = NA, #thus focus on population-level effects
                                categorical = TRUE, prob = .88,
                                conditions = conds)
    if(resp == "ach"){
      p <-
        plot_df[grepl("achievement",names(plot_df))][[1]] %>%
        mutate(effectiveness = factor(cats__,labels = c("low","medium","high"))) %>%
        mutate(condition = factor(cond__, labels = cond_labels)) %>%
        # filter(effectiveness %in% c("low","high")) %>%
        ggplot(aes_string(x = main_var, y = "estimate__",
                          color = "effectiveness", fill = "effectiveness",
                          ymin = "lower__", ymax = "upper__")) +
        geom_line() +
        geom_ribbon(alpha = 0.2) +
        facet_wrap(vars(condition)) +
        scale_color_viridis_d("Goal achievement in \n priority goal", direction = -1) + 
        scale_fill_viridis_d("Goal achievement in \n priority goal", direction = -1)
    }
    
    
    if(resp == "sat"){
      p <-
        plot_df[grepl("ego",names(plot_df))][[1]] %>%
        mutate(satisfaction = factor(cats__,labels = c("lowest","low","high","highest"))) %>%
        mutate(condition = factor(cond__, labels = cond_labels)) %>%
        # filter(satisfaction %in% c("lowest","highest")) %>%
        ggplot(aes_string(x = main_var, y = "estimate__",
                          color = "satisfaction", fill = "satisfaction",
                          ymin = "lower__", ymax = "upper__")) +
        geom_line() +
        geom_ribbon(alpha = 0.2) +
        facet_wrap(vars(condition)) + 
        scale_color_viridis_d("Satisfaction with \n inclusion in \n governance process", direction = -1) + 
        scale_fill_viridis_d("Satisfaction with \n inclusion in \n governance process", direction = -1)
    }
  }
  
  if(is.null(resp)){ #univariate case
    
    plot_df <-
      brms::conditional_effects(model_fit, effects = as.character(main_var),
                                re_formula = NA, #thus focus on population-level effects
                                categorical = TRUE, prob = .88,
                                conditions = conds)
    if(model_fit$formula$resp == "achievement"){
      p <-
        plot_df[[1]] %>%
        mutate(effectiveness = factor(cats__,labels = c("low","medium","high"))) %>%
        mutate(condition = factor(cond__, labels = cond_labels)) %>%
        # filter(effectiveness %in% c("low","high")) %>%
        ggplot(aes_string(x = main_var, y = "estimate__",
                          color = "effectiveness", fill = "effectiveness",
                          ymin = "lower__", ymax = "upper__")) +
        geom_line() +
        geom_ribbon(alpha = 0.2) +
        facet_wrap(vars(condition)) +
        scale_color_viridis_d("Goal achievement in \n priority goal", direction = -1) + 
        scale_fill_viridis_d("Goal achievement in \n priority goal", direction = -1)
    }
    
    
    if(model_fit$formula$resp == "egoinclusion"){
      p <-
        plot_df[[1]] %>%
        mutate(satisfaction = factor(cats__,labels = c("lowest","low","high","highest"))) %>%
        mutate(condition = factor(cond__, labels = cond_labels)) %>%
        # filter(satisfaction %in% c("lowest","highest")) %>%
        ggplot(aes_string(x = main_var, y = "estimate__",
                          color = "satisfaction", fill = "satisfaction",
                          ymin = "lower__", ymax = "upper__")) +
        geom_line() +
        geom_ribbon(alpha = 0.2) +
        facet_wrap(vars(condition)) + 
        scale_color_viridis_d("Satisfaction with \n inclusion in \n governance process", direction = -1) + 
        scale_fill_viridis_d("Satisfaction with \n inclusion in \n governance process", direction = -1)
    }
  }
  
  p <-
    p + ylab("Probability of category") + theme_minimal()
  
  if(log_x){
    p + scale_x_continuous(labels = function(x){round(exp(x))},
                           breaks = c(0,log(2),log(10),log(100)))
  }
  
}

plot_4_interactions <- function(model_fit, resp = NULL){
  bet_labels <- c("Bridging at 10th percentile",
                  "Bridging at median",
                  "Bridging at 90th percentile")
  
  
  bond_labels <- c("Bonding at 10th percentile",
                   "Bonding at median",
                   "Bonding at 90th percentile")
  
  power_labels <- c("Power at 10th percentile",
                    "Power at median",
                    "Power at 90th percentile")
  
  p_power_bond <- 
    plot_cond_effects(main_var = "ascribed_power_log",cond_var = "triangle_count_log",
                      resp = resp,
                      model_fit = model_fit, 
                      cond_labels = bond_labels) +
    xlab("Power") + guides(fill = FALSE, color = FALSE)
  
  p_power_bet <-
    plot_cond_effects(main_var = "ascribed_power_log",cond_var = "bet_log",
                      resp = resp,
                      model_fit = model_fit, 
                      cond_labels = bet_labels) +
    xlab("Power") + guides(fill = FALSE, color = FALSE)
  
  p_bond_bet <-
    plot_cond_effects(main_var = "triangle_count_log",cond_var = "bet_log",
                      resp = resp,
                      model_fit = model_fit, 
                      cond_labels = bet_labels) +
    xlab("Bonding") + ylab("") + guides(fill = FALSE, color = FALSE)
  
  p_bet_bond <- 
    plot_cond_effects(main_var = "bet_log",cond_var = "triangle_count_log",
                      resp = resp,
                      model_fit = model_fit, 
                      cond_labels = bond_labels) +
    xlab("Bridging") + ylab("") 
  
  
  p_power_bond + p_bet_bond + p_power_bet + p_bond_bet
}
