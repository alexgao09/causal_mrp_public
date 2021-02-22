# Code for reproducing results in "Treatment effect estimation with Multilevel Regression and Poststratification"

## Authors

- Yuxiang Gao
- Lauren Kennedy
- Daniel Simpson

Arxiv preprint: https://arxiv.org/abs/2102.10003

## Generating the target population with files in the dg_process directory

1. Run generate_population_v1.R to produce the 4 rds files (school level noise, school level noise, target population of 2.2M individuals, poststratification matrix):

```
school_noise_u_fixed.rds
school_noise_v_fixed.rds
pop_draw.rds
PS_mat.rds
```

2. Run analytic_formula_for_TE_v1.R to calculate the true Average Treatment Effect (ATE) and true Conditional Average Treatment Effects (CATEs) studied in the manuscript for the superpopulation in generate_population_v1.R

3. Run rct_simulation_trueCATE_school_v1.R to calculate true CATEs for all 11221 in the superpopulation, which is stored in ```CATE_school_v4.rds```

## Prior predictive checking

1. 

