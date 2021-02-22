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

## Prior predictive checking with files in the checks directory

1. Run prior_predictive_check.R to perform prior predictive checks for the Post-GPA and Prev-GPA hierarchical models. The sampling design studied in the manuscript is incorporated in prior_predictive_check.R. This step requires the files generated in the first subsection:

```
pop_draw.rds
PS_mat.rds
```

2. Run subgroupsizes_simulation.R to get the average sample size for the subgroups of the CATEs studied in the manuscript, as well as the relative size of such subgroups in the target population. This step requires the file ```pop_draw.rds```

## Simulation study with files in the simulation directory

1. ate_calculation_pipeline_v8_8_test.R runs a simulation iteration on an individual CPU. Line 4 represents the number of simulation iterations and line 7 represents the number of CPUs to use. These two numbers should match. In this case, we're running 20 simulation iterations (100 simulation iterations were done in the manuscript). After running ate_calculation_pipeline_v8_8_test.R, the file containing results of 20 simulation iterations is generated: ```20_20_1000_v8_8.RData```

