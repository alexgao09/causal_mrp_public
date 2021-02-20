rm(list=ls())
gc()



# build population ---------------------------------------------------------------------

library(tidyverse)
library(DeclareDesign)

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

# returns previous GPA population vector for a student 
prev_gpa_function = function(N, school_achievement_index) {
  # N is a number indicating number to sample
  # school_achievement_index is the population vector of values 1,2, or 3
  # page 31/72 in the appendix gives pre-intervention core GPA mean and sd for full, low, high achieving schools. 
  m = c(2.1, 2.8, 3.5)[school_achievement_index] # mean vector from appendix
  s = c(0.8, 1, 0.6)[school_achievement_index] # sd vector from appendix
  
  return(truncnorm::rtruncnorm(n = N, mean = m, sd = s,
                               a=0, b=4.33)) # rtruncnorm
}


# returns the population vector race/eth index of the student: 1,2,3,4,5 == asian, black, hisp, white, other
race_eth_function = function(school_achievement_index, minority_composition_index) {
  # N is a number indicating number to sample
  # minority_composition_index is the population vector of  1 or 2
  # school_achievement_index is the population  vector 1,2, or 3
  
  school_achievement_index_helper = function(combined_values) {
    school_achievement_index_value = combined_values[1]
    minority_composition_index_value = combined_values[2]
    
    if (school_achievement_index_value==1) {
      P =  c(2.6, 12.7, 28, 36.7, 20)/100
    }else {
      if (school_achievement_index_value==2) {
        P = c(3.8, 11.2, 24.4, 43, 17.6)/100
      }else {
        P = c(5.1, 9.6, 20.5, 49.9, 14.9)/100
      }
    }
    
    if (minority_composition_index_value==1) { # low minority comp (black + hisp. + other)
      P[c(2,3,5)] = P[c(2,3,5)] - .01 # black, hisp., other
      P[-c(2,3,5)] = P[-c(2,3,5)] + 0.015
    } else {
      if (minority_composition_index_value==2) { # high minority comp
        P[c(2,3,5)] = P[c(2,3,5)] + .01 # black, hisp., other
        P[-c(2,3,5)] = P[-c(2,3,5)] - 0.015
      }
    }
    
    return(sample(x = 1:5, size = 1, replace = TRUE, prob = P))
  }
  
  
  return(apply(X=cbind(school_achievement_index, minority_composition_index), 
               FUN=school_achievement_index_helper,
               MARGIN = 1)
  )
  
}

# returns the population vector of maternal_edu of students. 0 or 1
maternal_edu_function = function(school_achievement_index, minority_composition_index) {
  # minority_composition_index is the population vector of  1 or 2
  # school_achievement_index is the population  vector 1,2, or 3
  
  # maternal_edu_helper takes in school_achievement_index_value and minority_composition_index_value and returns a single sample of 0 or 1
  maternal_edu_helper = function(combined_values) {
    school_achievement_index_value = combined_values[1]
    minority_composition_index_value = combined_values[2]
    
    maternal_edu_P = c(0.218, 0.289, 0.366)[school_achievement_index_value]
    
    if (minority_composition_index_value==1){ # low minority comp
      maternal_edu_P = maternal_edu_P + 0.02 # upweight maternal education probability
    } else {
      if (minority_composition_index_value==2) {
        maternal_edu_P = maternal_edu_P - 0.02
      }
    }
    
    return(sample(x = c(0,1), size = 1, replace = TRUE, prob = c(maternal_edu_P, 1- maternal_edu_P)))
  }
  
  return(apply(X=cbind(school_achievement_index, minority_composition_index), 
               FUN=maternal_edu_helper,
               MARGIN = 1)
  )
}


#set.seed(20)
# GPA is on a 0 - 4.33 scale
school_noise_u_fixed = list()
school_noise_v_fixed = list()
number_of_schools_vector = c(2806, 3040, 2570, 2239, 566)

for (k in 1:length(number_of_schools_vector)) {
  school_noise_u_fixed[[toString(number_of_schools_vector[k])]] = rnorm(n=number_of_schools_vector[k], 0, sd=0.04)
  school_noise_v_fixed[[toString(number_of_schools_vector[k])]] = rnorm(n=number_of_schools_vector[k], 0, sd=0.04)
}

saveRDS(school_noise_u_fixed, "school_noise_u_fixed.rds")
saveRDS(school_noise_v_fixed, "school_noise_v_fixed.rds")


# Use declare design to generate population -----------------------------------------------------

design = declare_population(school_type = add_level(N=dim(school_type_ps_final)[1], 
                                                    minority_composition_index = school_type_ps_final$minority_composition,
                                                    school_achievement_index = school_type_ps_final$school_achievement,
                                                    number_of_schools = number_of_schools_vector # page 15/191 in NSLM Methodology report
),

# add mean 0, sd=0.2 for every school.
school = add_level(N=number_of_schools, 
                   school_size = rpois(n=N,lambda=200),
                   school_noise_u = unlist(school_noise_u_fixed),
                   school_noise_v = unlist(school_noise_v_fixed)
                   # school_noise_u = school_noise_u_fixed[[toString(N)]],
                   # school_noise_v = school_noise_v_fixed[[toString(N)]]
                   # school_noise_u = rnorm(n=N, 0, 0.04),
                   # school_noise_v = rnorm(n=N, 0, 0.04)
                   ),   

student = add_level(N=school_size, 
                    gender=sample(size=N, x=c(0,1), replace=TRUE, prob=c(0.49, 0.51)), # 0 is female. 1 is male. page 31/72 in appendix.
                    
                    prev_gpa = prev_gpa_function(N = N, 
                                                 school_achievement_index = school_achievement_index), # previous GPA
                    
                    race_eth = race_eth_function(school_achievement_index = school_achievement_index,
                                                 minority_composition_index = minority_composition_index),
                    
                    maternal_edu = maternal_edu_function(school_achievement_index = school_achievement_index,
                                                         minority_composition_index = minority_composition_index)
                    
) 
)


potential_outcomes = declare_potential_outcomes(post_gpa ~ truncnorm::rtruncnorm(n=N, a=0, b = 4.33,
                                                                                 mean = pmax(
                                                                                   pmin(prev_gpa + 
                                                                                          (rnorm(c(0.1, 0.07, 0.01), 
                                                                                                 mean=c(0.1, 0.07, 0.01), 
                                                                                                 sd=0))[school_achievement_index]*Z +
                                                                                          (rnorm(c(0, -0.01, 0.01), 
                                                                                                 mean=c(0, -0.01, 0.01),
                                                                                                 sd=0))[(minority_composition_index + 1)]*Z + # index of 1 corresponds to both low and high comp. in lowschoolach.
                                                                                          (rnorm(c(0.01, 0.01), 
                                                                                                 mean=c(0.01, 0.01), 
                                                                                                 sd=0))[(gender + 1)]*Z +
                                                                                          (rnorm(c(0.02, 0.1, 0.07, 0.1, 0.02), 
                                                                                                 mean=c(0.02, 0.1, 0.07, 0.1, 0.02), 
                                                                                                 sd=0))[race_eth]*Z +
                                                                                          (rnorm(c(0.01, 0), 
                                                                                                 mean=c(0.01, 0),
                                                                                                 sd=0))[(maternal_edu + 1)]*Z + 
                                                                                          school_noise_u + school_noise_v*Z,
                                                                                        4.33),
                                                                                   0),
                                                                                 sd=0.6))

assignment = declare_assignment(assignment_variable = "Z")

estimand_ate = declare_estimand(ATE = mean(post_gpa_Z_1 - post_gpa_Z_0))
estimand_cate_school_achievement_1 = declare_estimand(CATE_school_achievement_1 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (school_achievement_index==1))
estimand_cate_school_achievement_2 = declare_estimand(CATE_school_achievement_2 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (school_achievement_index==2))
estimand_cate_school_achievement_3 = declare_estimand(CATE_school_achievement_3 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (school_achievement_index==3))
estimand_cate_race_eth_1 = declare_estimand(CATE_race_eth_1 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (race_eth==1))
estimand_cate_race_eth_2 = declare_estimand(CATE_race_eth_2 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (race_eth==2))
estimand_cate_race_eth_3= declare_estimand(CATE_race_eth_3 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (race_eth==3))
estimand_cate_race_eth_4 = declare_estimand(CATE_race_eth_4 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (race_eth==4))
estimand_cate_race_eth_5 = declare_estimand(CATE_race_eth_5 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (race_eth==5))
estimand_cate_maternal_edu_0 = declare_estimand(CATE_maternal_edu_0 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (maternal_edu==0))
estimand_cate_maternal_edu_1 = declare_estimand(CATE_maternal_edu_1 = mean(post_gpa_Z_1 - post_gpa_Z_0), subset = (maternal_edu==1))

estimator_ate = declare_estimator(post_gpa ~ Z, label = "ATE", estimand = estimand_ate)
estimator_cate_school_achievement_1 = declare_estimator(post_gpa ~ Z, subset = (school_achievement_index==1), label = "CATE_school_achievement_1", estimand = estimand_cate_school_achievement_1)
estimator_cate_school_achievement_2 = declare_estimator(post_gpa ~ Z, subset = (school_achievement_index==2), label = "CATE_school_achievement_2", estimand = estimand_cate_school_achievement_2)
estimator_cate_school_achievement_3 = declare_estimator(post_gpa ~ Z, subset = (school_achievement_index==3), label = "CATE_school_achievement_3", estimand = estimand_cate_school_achievement_3)
estimator_cate_race_eth_1 = declare_estimator(post_gpa ~ Z, subset = (race_eth==1), label = "CATE_race_eth_1", estimand = estimand_cate_race_eth_1)
estimator_cate_race_eth_2 = declare_estimator(post_gpa ~ Z, subset = (race_eth==2), label = "CATE_race_eth_2", estimand = estimand_cate_race_eth_2)
estimator_cate_race_eth_3= declare_estimator(post_gpa ~ Z, subset = (race_eth==3), label = "CATE_race_eth_3", estimand = estimand_cate_race_eth_3)
estimator_cate_race_eth_4 = declare_estimator(post_gpa ~ Z, subset = (race_eth==4), label = "CATE_race_eth_4", estimand = estimand_cate_race_eth_4)
estimator_cate_race_eth_5 = declare_estimator(post_gpa ~ Z, subset = (race_eth==5), label = "CATE_race_eth_5", estimand = estimand_cate_race_eth_5)
estimator_cate_maternal_edu_0 = declare_estimator(post_gpa ~ Z, subset = (maternal_edu==0), label = "CATE_maternal_edu_0", estimand = estimand_cate_maternal_edu_0)
estimator_cate_maternal_edu_1 = declare_estimator(post_gpa ~ Z, subset = (maternal_edu==1), label = "CATE_maternal_edu_1", estimand = estimand_cate_maternal_edu_1)

temp = design + potential_outcomes + assignment + 
  estimand_ate +
  estimand_cate_school_achievement_1 +
  estimand_cate_school_achievement_2 +
  estimand_cate_school_achievement_3 +
  estimand_cate_race_eth_1 +
  estimand_cate_race_eth_2 +
  estimand_cate_race_eth_3 +
  estimand_cate_race_eth_4 +
  estimand_cate_race_eth_5 +
  estimand_cate_maternal_edu_0 +
  estimand_cate_maternal_edu_1 +
  estimator_ate +
  estimator_cate_school_achievement_1 +
  estimator_cate_school_achievement_2 +
  estimator_cate_school_achievement_3 +
  estimator_cate_race_eth_1 +
  estimator_cate_race_eth_2 +
  estimator_cate_race_eth_3 +
  estimator_cate_race_eth_4 +
  estimator_cate_race_eth_5 +
  estimator_cate_maternal_edu_0 +
  estimator_cate_maternal_edu_1

pop_draw = draw_data(temp)

cor(pop_draw$post_gpa, pop_draw$prev_gpa)

sum(unlist(unique(pop_draw[,c("school","number_of_schools","school_noise_u","school_noise_v")]) %>% 
             filter(number_of_schools==2806) %>% 
             select(school_noise_u)) == school_noise_u_fixed[["2806"]])

sum(unlist(unique(pop_draw[,c("school","number_of_schools","school_noise_u","school_noise_v")]) %>% 
             filter(number_of_schools==3040) %>% 
             select(school_noise_u)) == school_noise_u_fixed[["3040"]])

sum(unlist(unique(pop_draw[,c("school","number_of_schools","school_noise_u","school_noise_v")]) %>% 
             filter(number_of_schools==2570) %>% 
             select(school_noise_u)) == school_noise_u_fixed[["2570"]])

# define poststrat matrix -----------------------------------------
# SCATE_strata is sample CATE for a strata

SCATE_school = pop_draw %>% group_by(school) %>% summarise(SCATE_school = mean(post_gpa_Z_1 - post_gpa_Z_0))

PS_mat = pop_draw %>% 
  group_by(maternal_edu, gender, race_eth, school, minority_composition_index, school_achievement_index) %>% 
  summarise(Freq=n(), SCATE_strata = mean(post_gpa_Z_1 - post_gpa_Z_0)) %>% as.data.frame

PS_mat = inner_join(PS_mat, SCATE_school, by="school")



saveRDS(pop_draw, "pop_draw.rds")
saveRDS(PS_mat, "PS_mat.rds")

#rm(list=ls())
#gc()

#sims_temp = diagnose_design(temp,sims = 2, bootstrap_sims = FALSE)


