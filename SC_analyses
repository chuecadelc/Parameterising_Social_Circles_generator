## Required libraries ##

library(tidyverse)
library(ggplot2)
library(ineq)
library(kableExtra)
library(GGally)
library(ggpubr) 
library(gridExtra)
library(grid)
library(cowplot)

# import the node-level data
social_circles_node_level <- read.csv("~/SC_combined_experiments_node_level.csv")

# wrangle and summarise
sc_node_level <- social_circles_node_level %>% 
  group_by(experiment, social_distance) %>% 
  summarize_at(vars(node_deg:local_clust),list(mean = mean, gini = ~ineq::Gini(.))) %>% 
  ungroup()

# Table tranform
# table_latex <- kable(sc_node_level_subset, format = "latex", booktabs = TRUE)

###################################

####### DEGREE DISTRIBUTION #######

###################################

## Violin Plots Function ##

violin_plots <- function(data,type,filtering_var,title_names) {
  
  ## for Two circle experiments changing social reaches ##
  if (type == 1) {

  # Subset and create new grouping variables for echa experimental condition
    sc_node_violins <- data %>% 
      filter(experiment %in% c(4,5,8),run_number == 15) %>% 
      mutate(sc_reaches = factor(case_when(
        experiment == 4  ~ "[15 30]",
        experiment == 8  ~ "[20 40]",
        experiment == 5  ~ "[30 50]"
      )),experiment = factor(experiment, levels = c(4, 8, 5)))

    experiment_label <- c("4" = "Reaches: 15 and 30","8" = "Reaches: 20 and 40", "5" ="Reaches: 30 and 50")
    
    violin_output <- ggplot(sc_node_violins, aes(x = as.factor(social_distance), y = node_deg)) +
      geom_violin(scale = "count",fill = "grey90")+
      geom_boxplot(width = 0.09, position = position_dodge(width = 0.9), fill="black")+
      scale_fill_brewer(palette = "Set2")+
      labs(x = "Agent social reach", y = "Node degree") +
      facet_wrap(~experiment, labeller = as_labeller(experiment_label))+
      ggtitle(paste("Distribution of node degrees and social reaches in Two circle experiments", sep = " "))+ 
      theme_bw() +
      theme(panel.grid = element_blank())
    
    # violin_output
    
    # Save plot
    ggsave(filename = paste0(title_names, ".jpeg"), plot = violin_output, width = 8,height = 4)
    
  }  
  ## Two circles, changing distributions ##
  else if (type == 2) {

    # Subset and create new grouping variables for echa experimental condition
    sc_node_violins <- data %>% 
      filter(experiment %in% c(8,9,10),run_number == 15) %>% 
      mutate(prop_agents = factor(case_when(
        experiment == 8  ~ "[75 25 0]",
        experiment == 9 ~ "[25 75 0]",
        experiment == 10 ~ "[90 10 0]",
      )),experiment = factor(experiment, levels = c(10, 8, 9)))
    
      experiment_label <- c("9" = "Dist: 25% SR and 75% LR","8" = "Dist: 75% SR and 25% LR", "10" ="Dist: 90% SR and 10% LR")
   
      violin_output <- ggplot(sc_node_violins, aes(x = as.factor(social_distance), y = node_deg, fill = prop_agents)) +
      geom_violin(scale = "count",fill = "grey90")+
      geom_boxplot(width = 0.09, position = position_dodge(width = 0.9),fill="black")+
      scale_fill_brewer(palette = "Set2")+
      labs(x = "Agent social reach", y = "Node degree", fill = "Proportion agents") +
      facet_wrap(~experiment, labeller = as_labeller(experiment_label))+
      ggtitle(paste("Distribution of node degrees and social reaches in Two circle experiments", sep = " "))+
      theme_bw() +
      theme(panel.grid = element_blank())
    
    # violin_output 

    # Save plot
    ggsave(filename = paste0(title_names, ".jpeg"), plot = violin_output, width = 8,height = 4)

    
  }
   ## Three circles, changing distributions ##
  else {

    # Subset and create new grouping variables for echa experimental condition
    sc_node_violins <- data %>% 
      filter(experiment %in% c(6,7,11),run_number == 15) %>% 
      mutate(prop_agents = factor(case_when(
        experiment == 6 ~ "[70 20 10]",
        experiment == 7 ~ "[34 33 33]",
        experiment == 11 ~ "[10 20 70]")),
        experiment = factor(experiment, levels = c(6,7,11)))
    
    experiment_label <- c("6" = "Dist: 70% SR, 20% MR, 10% LR","7" = "Dist: 34% SR, 33% MR, 33% LR", "11" ="Dist: 10% SR, 20% MR, 70% LR")
    
    violin_output <- ggplot(sc_node_violins, aes(x = as.factor(social_distance), y = node_deg, fill = prop_agents)) +
      geom_violin(scale = "count",fill = "grey90")+
      geom_boxplot(width = 0.09, position = position_dodge(width = 0.9),fill="black")+
      scale_fill_brewer(palette = "Set2")+
      labs(x = "Agent social reach", y = "Node degree", fill = "Proportion agents") +
      facet_wrap(~experiment, labeller = as_labeller(experiment_label))+
      ggtitle(paste("Distribution of node degrees and social reaches in Three circle experiments", sep = " "))+
      theme_bw() +
      theme(panel.grid = element_blank())

    # violin_output
    
    ggsave(filename = paste0(title_names, ".jpeg"), plot = violin_output, width = 8,height = 4)
    
  }
  
}

# Create violin plots for our 3 sets of experiments 
violin_plots(social_circles_node_level,2,"experiment","2 circles sr node degree dist")
violin_plots(social_circles_node_level,3,"experiment","2 circles proportions node degree dist")
violin_plots(social_circles_node_level,4,"experiment","3 circles proportions node degree dist")


###################################

## PROPERTY INTERDEPENDENCIES ##

###################################
# Import population-level data
sc_og_experiments <- read.csv("~/SC_proportion_combined_experiments.csv")

# wrangle and summarise
sc_og_experiments_summary <- sc_og_experiments %>% 
  group_by(experiment) %>% 
  select(n_edges,10:25) %>% 
  summarise_all(mean)

# kable(sc_og_experiments_summary, format = "latex", booktabs = TRUE)

## creating the combinatory categories

## creating the combinatory categories

sc_og_exp_filter <- sc_og_experiments %>%
  mutate(prop_agents = factor(
    case_when(
      prop_green == 1000 ~ "[100 0 0]",
      prop_green == 750 ~ "[75 25]",
      prop_green == 700 ~ "[70 20 10]",
      prop_green == 340 ~ "[34 33 33]",
      prop_green == 900 ~ "[90 10]",
      prop_green == 250 ~ "[25 75]",
      prop_green == 100 ~ "[10 20 70]"
    )
  ),
  sc_reaches = factor(
    case_when(
      experiment == 1  ~ "[30]",
      experiment == 2  ~ "[mean 30]",
      experiment == 3  ~ "[range 10 to 50]",
      experiment == 4  ~ "[15 30]",
      experiment == 5  ~ "[30 50]",
      experiment == 6  ~ "[30 40 50]",
      experiment == 7  ~ "[30 40 50]",
      experiment == 8  ~ "[20 40]",
      experiment == 9  ~ "[20 40]",
      experiment == 10  ~ "[20 40]",
      experiment == 11  ~ "[30 40 50]"
    )
  ))
# save combined output
write.csv(sc_og_exp_filter, "SC_filtered.csv", row.names = F)


### SCATTERPLOT FUNCTION ###

scatter_plots <- function(data,type,experiment_num,color_values, plot_name) {

  if (type == 1) {
    
    filtered_data <-
      data %>% filter(experiment %in% experiment_num) %>% 
      mutate(prop_agents = factor(prop_agents, ordered= TRUE,
                                  levels = c("[90 10]","[75 25]","[25 75]")))
    
    p <- ggplot(filtered_data, aes(x=Assortativity,y=Degree_mean))+
      geom_point(aes(size=sc_reaches, color = prop_agents))+ 
      scale_size_manual("Reaches", values = c(0.5,1.25, 2))+
      scale_color_manual(values = c("lightgrey","darkgrey","black"))+
      labs(y="Average Degree", color= "SR LR mix")+
      theme_classic()
    
    p1 <- ggplot(filtered_data, aes(x=Assortativity,y=Geodesic_mean))+
      geom_point(aes(size=sc_reaches, color = prop_agents))+ 
      scale_size_manual("Reaches", values = c(0.5,1.25, 2))+
      scale_color_manual(values = c("lightgrey","darkgrey","black"))+
      labs(y="Average geodesic",  color= "SR LR mix")+
      theme_classic()
    
    p2 <- ggplot(filtered_data, aes(x=Geodesic_mean, y=Degree_mean))+
      geom_point(aes(size=sc_reaches, color = prop_agents))+ 
      scale_size_manual("Reaches", values = c(0.5,1.25, 2))+
      scale_color_manual(values = c("lightgrey","darkgrey","black"))+
      labs(y="Average Degree", x="Average geodesic",  color= "SR LR mix")+
      theme_classic()
    
    leg <- get_legend(p1)
    
  }
  # for the 3-C experiments
  else {
    
    filtered_data <-
      data %>% filter(experiment %in% experiment_num) %>% 
      mutate(prop_agents = factor(prop_agents, ordered= TRUE,
                                  levels = c("[70 20 10]","[34 33 33]","[10 20 70]")))
  
  p <- ggplot(filtered_data, aes(x=Assortativity,y=Degree_mean,size=prop_agents, color = prop_agents))+
    geom_point()+ 
    scale_size_manual("SR MR LR mix", values = c(0.5,1.25, 2))+
    scale_color_manual(values = c("lightgrey","darkgrey","black"))+
    labs(y="Average Degree")+
   theme_classic()
  
  p1 <- ggplot(filtered_data, aes(x=Assortativity,y=Geodesic_mean,size=prop_agents, color = prop_agents))+
    geom_point()+ 
    scale_size_manual("SR MR LR mix", values = c(0.5,1.25, 2))+
    scale_color_manual("SR MR LR mix",values = c("lightgrey","darkgrey","black"))+
    labs(y="Average geodesic")+
     theme_classic()
  
  p2 <- ggplot(filtered_data, aes(x=Geodesic_mean, y=Degree_mean, size=prop_agents, color = prop_agents))+
    geom_point()+ 
    scale_size_manual("SR MR LR mix", values = c(0.5,1.25, 2))+
    scale_color_manual(values = c("lightgrey","darkgrey","black"))+
    labs(y="Average Degree")+
    theme_classic()
    
  leg <- get_legend(p1)
  
  }

  # combining the three plots together
  common_plot <- ggpubr::ggarrange(p,p2,p1, ncol=3,legend.grob = leg, legend="right")

  common_plot <- annotate_figure(common_plot, top = text_grob(paste0("Property interdependencies across ", plot_name),
                                              color = "black", face = "bold", size = 14))
  
  ggsave(filename =paste0("Property interdependencies across ", plot_name, ".jpeg"), plot = common_plot, width = 9, height = 6)
  
 # print(common_plot)

}
# Create one for the 2-Circles and another for the 3-Circles
experiment_numbs <- c(4,5,8,9,10)
scatter_plots(sc_og_exp_filter,1,experiment_numbs,colors_1,"Two Circle experiments")  

experiment_numbs1 <- c(6,7,11)
scatter_plots(sc_og_exp_filter,2,experiment_numbs1,colors_2,"Three Circle experiments")  












