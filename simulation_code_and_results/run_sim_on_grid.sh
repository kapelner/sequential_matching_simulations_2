#!/bin/bash

#$ -j y
#$ -N seq_rct_matching_power_simulations
####The final task number must be MANUALLY changed based on the number of rows in the parameter matrix
#$ -t 1-1998
#$ -q all.q

echo "starting R for seq_rct_matching power simulation #$SGE_TASK_ID"
R --no-save --args iter_num=$SGE_TASK_ID < all_match_sims.R