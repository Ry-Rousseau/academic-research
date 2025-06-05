# a couple functions ####
create_conjoint_plot <- function(data, subgroup, method, legend_title,title,plot_position="N", use_subgroup = T) {
  # This function creates a ggplot object visualizing the results of a conjoint analysis.
  # Inputs:
  #   - subgroup: A string representing the name of the subgrouping variable to use in the cj() function
  #   - method: A string representing the type of estimate to be computed (e.g., "mm")
  #   - data: The data.frame containing the variables for the conjoint analysis
  #   - legend_title: A string for the title of the legend in the ggplot object
  #   - plot_position: if the plot is going to be arranged in a ggarrange. adds or removes features depending on F (first),
  #                   M (middle) or L (last). default is N which keeps all. 
  # Output:
  #   - Returns a ggplot object visualizing the results by subgroup, with custom aesthetics
  
  if(use_subgroup){
    est <- cj(data, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
              id =  ~ respondent_id, by =  as.formula(paste("~", subgroup)),
              estimate = method)
  }else{
    est <- cj(data, dv ~ `Wage Subsidies` + `Wage Subsidy Duration` + `Payment Holidays` + `Payment Holiday Duration` + `Central Bank Policies`,
              id =  ~ respondent_id, estimate = method)
  }
  
  
  plot_obj <- plot(est, group = subgroup,vline = ifelse(method=="mm",0.5,0.0))
  
  num_levels <- length(levels(data[[subgroup]]))

  
  if (num_levels == 1)  {
    shape_values <- 19
  } else if (num_levels == 2) {
    shape_values <- c(19, 19)
  } else if (num_levels == 3) {
    shape_values <- c(19, 19, 19)
  } else if  (num_levels == 4){
    shape_values <- c(19, 19, 19, 19)
  } else {
    shape_values <- c(19,19,19,19,19)
  }
  
  plot_obj <- plot_obj + ggplot2::theme_light(base_size = 9) + 
    labs(title = title)+
    if(plot_position=="L"){
      theme(axis.text.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank())
    }else if(plot_position=="M"){
      theme(axis.text.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
    }else if(plot_position=="F"){
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }else if(plot_position=="N"){
      theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.title = element_blank())
    }
  
  return(plot_obj)
}