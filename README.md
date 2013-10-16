## Sequential Matching Simulation Code and Results

By Adam Kapelner and Abba Krieger

October 15, 2013


We provide the simulation code and results for the Biometrics paper "Matching on-the-fly: Sequential Allocation with Higher Power and Efficiency."


### Figures and Tables Replication

To replicate the figures and tables found in  "Matching on-the-fly: Sequential Allocation with Higher Power and Efficiency" and its accompanying supplementary materials document, open the file `make_plots_and_tables.R` and source lines 1-100 in `R`. The rest of the script is split up by table and figure; source the relevant portions to create the table or figure.


### Simulation Replication

To replicate the simulations found in Section 3 of the paper, we recommend using a computer cluster powered by a grid engine system. We include the file `run_sim_on_grid.sh` which will run the entire set of 1,998 simulations on a Sun Grid Engine.

If you do not have a cluster, you can replicate by running `R --args iter_num=1 < all_match_sims.R`. You need to vary `iter_num` from 1-1998.


### Custom Simulations

The simulation code was written to be modular and flexible. Below are the parameters that can be changed:

* `Nsim_per_block`: The number of reps to simulate for each allocation and estimation run. This is defaulted to 2,000 to get precise resolution. Feel free to reduce this number.
* `Nsim_exact_test`: Within the exact tests, how many Monte Carlo samples are done? We default this to 1,000 to balance precision and expedience.
* `treatment_effects`: This is a vector of possible parameters `beta_T`, the linear/additive effect of the treatment indicator on the response function, `y`. The default is 0 and 1 to simulate both size and power of the test, respectively.
* `Z_TESTS`: For estimators that make use of the matched set plus repository data structure, this will control whether the T approximation will be employed. Default is TRUE and FALSE so the experimenter will be able to observe the difference between the Z approximation and T approximation. Set to just TRUE to cut the simulation time in half for those runs.
* `prob_trt`: The probability of assigning treatment. Default is 0.5. We did not simulate other values, but it could be an interesting project.
* `ns_to_test`: The vector of the number of subjects in the sequential experiment.
* `prob_match_cutoff_lambdas`: The vector of `lambda`'s - the propensity to match (when applicable).
* `randomization_types`: The vector of methods of allocation treatment and controls to `indic_T` over the course of a sequential experiment. The strings in this vector are X, corresponding to filenames `rand_type_<X>.R`. The implementation of the sequential matching algorithm is located for instance in `rand_type_seq_match_kk.R`.
* `metrics_for_each_run`: The vector of strings which correspond to metrics which are collected from each simulation run.
* `sim_type`: The parameter X in the filename `sim_type_<X>.R` which runs the simulation based on the current `iter_num`.

In addition to parameters, the structure of the simulation i.e. the response models and the covariate distribution can be specified.

* `data_and_err_gen`: The parameter X in the filename `data_and_err_gen_<X>.R` which controls how the fixed design matrix and errors are generated. In this file, you must specify variables `p`, the number of covariates, `x_s`, the design matrix of size n times p, and `errors`, of size n which are the disturbances.
* In the file `sim_type_series_of_quadratics_and_interactions.R`, you can specify the following options:
  * `response_model`: The parameter X in filename `create_response_<X>.R` which controls how the design matrix combines with the errors to create the response. In this file, you must specify `z` as a function of `x_s' and `beta`'s, which is the covariate function. Then you must specify `y` as a function of `z`, `beta_T` , the effect of the treatment and `indic_T`, the vector of allocations.
  * `sim_param_mat`: this is a matrix of beta's which can be varied. The beta's are called upon in the response model to generate `y` (see previous parameter). This matrix is then augmented by `treatment_effects`, `Z_TESTS`, `ns_to_test` and `prob_match_cutoff_lambdas`.

To run a block of simulations, `inner_simulations.R` is called upon. Here, we loop over `Nsim_per_block` where results are recorded for each simulation.
