# Code for reproducing results in "Treatment effect estimation with Multilevel Regression and Poststratification"

## Authors

- Yuxiang Gao
- Lauren Kennedy
- Daniel Simpson

Arxiv preprint: https://arxiv.org/abs/2102.10003

## Generating the target population 

1. In the dg_process directory, run generate_population_v1.R to produce the 4 rds files (school level noise, school level noise, target population of 2.2M individuals, poststratification matrix):

```
school_noise_u_fixed.rds
school_noise_v_fixed.rds
pop_draw.rds
PS_mat.rds
```
