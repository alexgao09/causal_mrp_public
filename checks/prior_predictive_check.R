library(brms)
library(bayesplot)
library(tidyverse)
library(DeclareDesign)
library(survey)
library(brms)
#library(dbarts)
library(parallel)

library(foreach)
library(doParallel)

# have population probabilities for each subgroup
school_achievement_levels = paste0("school",
                                   1:3) # used to be 1:4

minority_composition_levels = paste0("minority",
                                     1:2) # used to be 1:3

gender_levels = c("female", "male")

race_eth_levels = c("asian", "black", "hispanic", "white", "other")

maternal_edu_levels = c("no", "yes")


school_type_ps = expand.grid(1:length(minority_composition_levels),
                             1:length(school_achievement_levels))
colnames(school_type_ps) = c("minority_composition",
                             "school_achievement")
school_type_ps[1:2,1] = 0 # zero is combined minority composition
school_type_ps_final = school_type_ps[-1,]

pop_draw = readRDS("pop_draw.rds")
PS_mat = readRDS("PS_mat.rds")

# stratified cluster sampling function ----------------------------------------
stratifiedschoolsample_function = function(data, target_strata_samplesize) {
  # data is the population dataframe
  # target_strata_samplesize is a length-5 vector on number of schools to sample in each strata. From NLSM methodology pdf it should be c(28,34,32,19,27) (last column T2.3)
  strata1 = data %>% filter(school_achievement_index==1) %>% as.data.frame()
  strata2 = data %>% filter(school_achievement_index==2, minority_composition_index == 1) %>% as.data.frame()
  strata3 = data %>% filter(school_achievement_index==2, minority_composition_index == 2) %>% as.data.frame()
  strata4 = data %>% filter(school_achievement_index==3, minority_composition_index == 1) %>% as.data.frame()
  strata5 = data %>% filter(school_achievement_index==3, minority_composition_index == 2) %>% as.data.frame()
  
  # stratified sampling of schools of each strata 
  
  # simple random sample of schools with strata 1
  strata1_indices = sample(x = unique(strata1$school),
                           size = target_strata_samplesize[1],
                           replace = FALSE)
  
  strata2_indices = sample(x = unique(strata2$school),
                           size = target_strata_samplesize[2],
                           replace = FALSE)
  
  strata3_indices = sample(x = unique(strata3$school),
                           size = target_strata_samplesize[3],
                           replace = FALSE)
  
  strata4_indices = sample(x = unique(strata4$school),
                           size = target_strata_samplesize[4],
                           replace = FALSE)
  
  strata5_indices = sample(x = unique(strata5$school),
                           size = target_strata_samplesize[5],
                           replace = FALSE)
  
  # refer to page 18/191 to go from 140 schools to 65
  
  # the 140 schools sampled 
  data_140 = rbind(strata1 %>% filter(school %in% strata1_indices),
                   strata2 %>% filter(school %in% strata2_indices),
                   strata3 %>% filter(school %in% strata3_indices),
                   strata4 %>% filter(school %in% strata4_indices),
                   strata5 %>% filter(school %in% strata5_indices)) %>% as.data.frame()
  
  # the 65 schools that responded
  data_65_indices = sample(x = unique(data_140$school),
                           size = min(rpois(n=1,lambda=65), 140), # the number of schools that respond.
                           replace = FALSE)
  
  data_65 = data_140 %>% filter(school %in% data_65_indices) %>% as.data.frame()
  
  
  data_65$student_sampling_prob = rbinom(length(data_65$prev_gpa),
                                         size=1,
                                         prob=brms::inv_logit_scaled(data_65$prev_gpa)) # sample students with probability according to this. Mean response rate should be around 92%
  
  print(data_65 %>% group_by(school_achievement_index) %>% summarise(sum(student_sampling_prob)/n()))
  print(sum(data_65$student_sampling_prob)/dim(data_65)[1])
  data_65[data_65$student_sampling_prob==1,]
}

my_sampling_custom_ = declare_sampling(handler = stratifiedschoolsample_function, 
                                       target_strata_samplesize=c(28,34,32,19,27))



stratified_cluster_sample_first = my_sampling_custom_(pop_draw)
# fit prelim brms models


# step 2 model -----------------

step_2_model_check = brms::brm(formula = prev_gpa | trunc(lb=0, ub = 4.33) ~ maternal_edu + gender + (1|race_eth) + (1|school) + (1|minority_composition_index * school_achievement_index),
                               prior = c(
                                 prior(normal(0, .25), class=b),
                                 prior(normal(2.7,.25), class=Intercept), # change sd to 2
                                 prior(normal(0, .25), class = sd),
                                 prior(normal(0, .25), class = sigma)
                               ),
                               data = stratified_cluster_sample_first,
                               chains = 1,
                               core = 1,
                               control = list(adapt_delta = 0.99), sample_prior = "only")


yrep_fit_posterior <- posterior_predict(step_2_model_check)
table(complete.cases(yrep_fit_posterior))

complete_yrep_fit_posterior=yrep_fit_posterior[complete.cases(yrep_fit_posterior),]

# 200 prior simulations
complete_yrep_fit_posterior_f=complete_yrep_fit_posterior[1:200,]

# below is from bayesplot
prevgpamodel_priorpredictiveplot = ppc_dens_overlay(y = stratified_cluster_sample_first$prev_gpa,
                                           yrep = complete_yrep_fit_posterior_f) +
  xlab("Prev-GPA") + ylab("Density") +
  theme_bw() +
  ylim(c(0,2.25)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=13*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(13*1.6,"line"),
        legend.key.height = unit(3*1.6,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent")
  )



# Step three model ---------------------
# 

step_3_model_check = brms::brm(formula = post_gpa | trunc(lb=0, ub=4.33)  ~ offset(prev_gpa) +  Z*maternal_edu + Z*gender + (Z|race_eth) + (Z|school) + (Z|minority_composition_index * school_achievement_index),
                               prior = c(
                                 prior(normal(0, .125), class=b),
                                 prior(normal(0, .125), class=Intercept),
                                 prior(normal(0, .125), class = sd),
                                 prior(normal(0, .125), class = sigma)
                               ),
                               data = stratified_cluster_sample_first,
                               chains = 1,
                               core = 1,          
                              sample_prior = "only"
)





yrep_fit_posterior_3 <- posterior_predict(step_3_model_check)

complete_yrep_fit_posterior_3=yrep_fit_posterior_3[complete.cases(yrep_fit_posterior_3),]
table(complete.cases(yrep_fit_posterior_3))

# 200 prior simulations
complete_yrep_fit_posterior_f_3=complete_yrep_fit_posterior_3[1:200,]

# below is from bayesplot
postgpamodel_priorpredictiveplot = ppc_dens_overlay(y = stratified_cluster_sample_first$post_gpa,
                                                    yrep = complete_yrep_fit_posterior_f_3) +
  xlab("Post-GPA") + ylab("Density") +
  theme_bw() +
  ylim(c(0,2.25)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=13*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(13*1.6,"line"),
        legend.key.height = unit(3*1.6,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent")
  )


legend = cowplot::get_legend(postgpamodel_priorpredictiveplot)
  


png(filename = "priorpredictivecheck_plot.png",
    width = 1800,
    height = 600)

cowplot::plot_grid(prevgpamodel_priorpredictiveplot + theme(legend.position="none"),
                   postgpamodel_priorpredictiveplot + theme(legend.position="none") + ylab(""),
                   ncol=2
)

dev.off()




