# v1 contains school_noise_u, school_noise_v and max(min,...)

library(truncnorm)

school_noise_u = readRDS("school_noise_u_fixed.rds")
school_noise_t = readRDS("school_noise_v_fixed.rds")

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


index_df = expand.grid(strata_m_a = unique(paste0(school_type_ps$minority_composition, "_", school_type_ps$school_achievement)),
                       gender = 1:2,
                       race_eth = 1:5,
                       maternal_edu = 1:2
)
# strsplit("0_1","_")[[1]][2]
# truncnorm::rtruncnorm(n=1,mean=1,sd=10e-10,a=1.5,b=3)
school_achievement_treatment_vector = c(0.1, 0.07, 0.01)
minority_comp_treatment_vector = c(0, -0.01, 0.01)
gender_treatment_vector = c(0.01, 0.01)
race_eth_treatment_vector = c(0.02, 0.1, 0.07, 0.1, 0.02)
maternal_edu_treatment_vector = c(0.01, 0)

s_a_prob_vector = c(2806, 3040, 2570, 2239, 566)/sum(c(2806, 3040, 2570, 2239, 566))
gender_prob_vector = c(0.49, 0.51)
# given school achievement and minority comp of individual
race_eth_prob_vector_function = function(school_achievement_index_value, minority_composition_index_value) {
  # N is a number indicating number to sample
  # minority_composition_index is the population vector of  1 or 2
  # school_achievement_index is the population  vector 1,2, or 3
  
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
  
  return(P)
}
# given school achievement and minority comp of individual
maternal_edu_prob_vector_function = function(school_achievement_index_value, minority_composition_index_value) {
  # minority_composition_index is the population vector of  1 or 2
  # school_achievement_index is the population  vector 1,2, or 3
  
  
  maternal_edu_P = c(0.218, 0.289, 0.366)[school_achievement_index_value]
  
  if (minority_composition_index_value==1){ # low minority comp
    maternal_edu_P = maternal_edu_P + 0.02 # upweight maternal education probability
  } else {
    if (minority_composition_index_value==2) {
      maternal_edu_P = maternal_edu_P - 0.02
    }
  }
  
  return(c(maternal_edu_P, 1- maternal_edu_P))
  
}

prev_gpa_mean_sd_function = function(school_achievement_index) {
  # N is a number indicating number to sample
  # school_achievement_index is the population vector of values 1,2, or 3
  # page 31/72 in the appendix gives pre-intervention core GPA mean and sd for full, low, high achieving schools. 
  m = c(2.1, 2.8, 3.5)[school_achievement_index] # mean vector from appendix
  s = c(0.8, 1, 0.6)[school_achievement_index] # sd vector from appendix
  
  return(c(m, s))
}

E_Y_Z_1_matrix = c()
E_Y_Z_0_matrix = c()

schools_ineach_strata = data.frame(strata_m_a = levels(index_df$strata_m_a), num_schools = c(2806, 3040, 2570, 2239, 566))

E_Y_Z_1 = 0
E_Y_Z_0 = 0

for (s in levels(index_df$strata_m_a)) {
  temp_df = index_df %>% filter(strata_m_a==s)
  school_achievement_level_temp = as.numeric(strsplit(s,"_")[[1]][2])
  minority_composition_level_temp = as.numeric(strsplit(s,"_")[[1]][1])
  
  num_schools = schools_ineach_strata %>% filter(strata_m_a == s) %>% select(num_schools) %>% as.numeric
  
  
  
  for (i in 1:dim(temp_df)[1]) {
    E_Y_Z_1_i = 0 # the CATE for subgroup i out of the 100 subgroups
    E_Y_Z_0_i = 0
    prob_i =         (race_eth_prob_vector_function(school_achievement_index_value = school_achievement_level_temp,
                                                    minority_composition_index_value = minority_composition_level_temp))[(temp_df$race_eth)[i]] *
      (maternal_edu_prob_vector_function(school_achievement_index_value = school_achievement_level_temp,
                                         minority_composition_index_value = minority_composition_level_temp))[(temp_df$maternal_edu)[i]] *
      gender_prob_vector[(temp_df$gender)[i]] *
      s_a_prob_vector[which(s == levels(index_df$strata_m_a))]
    
    for (k in 1:num_schools) {
      current_school_noise_u = school_noise_u[[toString(num_schools)]][k]
      current_school_noise_t = school_noise_t[[toString(num_schools)]][k]
      
      integral_temp_Z_1 = integrate(function(x) {etruncnorm(a=0, b=4.33, mean=(pmax(pmin(x +
                                                                                         school_achievement_treatment_vector[school_achievement_level_temp] +
                                                                                         minority_comp_treatment_vector[minority_composition_level_temp + 1]  +
                                                                                         0.01 +
                                                                                         race_eth_treatment_vector[(temp_df$race_eth)[i]] +
                                                                                         maternal_edu_treatment_vector[(temp_df$maternal_edu)[i]] +
                                                                                         current_school_noise_u + current_school_noise_t,
                                                                                       4.33),
                                                                                   0)
      ), sd=0.6) * dtruncnorm(x, a=0, b=4.33,
                              mean=prev_gpa_mean_sd_function(school_achievement_level_temp)[1],
                              sd=prev_gpa_mean_sd_function(school_achievement_level_temp)[2]
      )},
      lower=0, upper=4.33,
      rel.tol = .Machine$double.eps^0.6)
      
      
      integral_temp_Z_0 = integrate(function(x) {etruncnorm(a=0, b=4.33, mean=pmax(pmin(x + current_school_noise_u,
                                                                                        4.33),
                                                                                   0), sd=0.6) * dtruncnorm(x, a=0, b=4.33,
                                                                                                            mean=prev_gpa_mean_sd_function(school_achievement_level_temp)[1],
                                                                                                            sd=prev_gpa_mean_sd_function(school_achievement_level_temp)[2]
                                                                                   )},
                                    lower=0, upper=4.33,
                                    rel.tol = .Machine$double.eps^0.6)
      
    
      
      integral_temp_Z_1_value = integral_temp_Z_1$value
      integral_temp_Z_0_value = integral_temp_Z_0$value
      
      
      E_Y_Z_1 = E_Y_Z_1 + integral_temp_Z_1_value * 
        (race_eth_prob_vector_function(school_achievement_index_value = school_achievement_level_temp,
                                       minority_composition_index_value = minority_composition_level_temp))[(temp_df$race_eth)[i]] *
        (maternal_edu_prob_vector_function(school_achievement_index_value = school_achievement_level_temp,
                                           minority_composition_index_value = minority_composition_level_temp))[(temp_df$maternal_edu)[i]] *
        gender_prob_vector[(temp_df$gender)[i]] *
        s_a_prob_vector[which(s == levels(index_df$strata_m_a))] * (1/num_schools)
      
      
      E_Y_Z_0 = E_Y_Z_0 + integral_temp_Z_0_value * 
        (race_eth_prob_vector_function(school_achievement_index_value = school_achievement_level_temp,
                                       minority_composition_index_value = minority_composition_level_temp))[(temp_df$race_eth)[i]] *
        (maternal_edu_prob_vector_function(school_achievement_index_value = school_achievement_level_temp,
                                           minority_composition_index_value = minority_composition_level_temp))[(temp_df$maternal_edu)[i]] *
        gender_prob_vector[(temp_df$gender)[i]] *
        s_a_prob_vector[which(s == levels(index_df$strata_m_a))]  * (1/num_schools)
      
      E_Y_Z_1_i = E_Y_Z_1_i + integral_temp_Z_1_value * (1/num_schools)
      E_Y_Z_0_i = E_Y_Z_0_i + integral_temp_Z_0_value * (1/num_schools)
    }
    
    E_Y_Z_1_matrix = rbind(E_Y_Z_1_matrix, data.frame(strata_m_a = s,
                                                      gender = temp_df$gender[i],
                                                      race_eth = temp_df$race_eth[i],
                                                      maternal_edu = temp_df$maternal_edu[i],
                                                      E_Y_Z = E_Y_Z_1_i,
                                                      p = prob_i
    ))
    E_Y_Z_0_matrix = rbind(E_Y_Z_0_matrix, data.frame(strata_m_a = s,
                                                      gender = temp_df$gender[i],
                                                      race_eth = temp_df$race_eth[i],
                                                      maternal_edu = temp_df$maternal_edu[i],
                                                      E_Y_Z = E_Y_Z_0_i,
                                                      p = prob_i
    ))
    
    
  }
  
  
}

paste("ATE FROM FORMULA:",E_Y_Z_1 - E_Y_Z_0)


# ATE AND CATES FROM MATRIX

E_Y_Z_mat = inner_join(x=E_Y_Z_1_matrix,y=E_Y_Z_0_matrix,by=c("strata_m_a", "gender", "race_eth", "maternal_edu"))

E_Y_Z_mat %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x))
E_Y_Z_mat %>% filter(strata_m_a=="1_3") %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA3 MC1 CATE
E_Y_Z_mat %>% filter(strata_m_a=="0_1",race_eth==2) %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA1 RE2 CATE

E_Y_Z_mat %>% filter(strata_m_a=="1_3", race_eth==1) %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA3 MC1 RE1 CATE
E_Y_Z_mat %>% filter(strata_m_a=="1_3", race_eth==2) %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA3 MC1 RE2 CATE
E_Y_Z_mat %>% filter(strata_m_a=="1_3", race_eth==3) %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA3 MC1 RE3 CATE
E_Y_Z_mat %>% filter(strata_m_a=="1_3", race_eth==4) %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA3 MC1 RE4 CATE
E_Y_Z_mat %>% filter(strata_m_a=="1_3", race_eth==5) %>% summarise(sum((E_Y_Z.x - E_Y_Z.y)*p.x)/sum(p.x)) # SA3 MC1 RE5 CATE



# 
