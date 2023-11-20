#!/bin/bash

#SBATCH --job-name=measurementError.sh
#SBATCH --time=36:00:00
#SBATCH --partition=caslake
#SBATCH --nodes=1
#SBATCH --array=1
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=48000
#SBATCH --account=pi-jozik

seasonality=seasonal
openness=closed
run=5
R CMD BATCH /home/qizhan/others/PhD/projects/FOI/analysis$run/scripts/actualRuns/$seasonality/$openness/measurementError/measurementError${SLURM_ARRAY_TASK_ID}.R /home/qizhan/others/PhD/projects/FOI/analysis$run/scripts/actualRuns/$seasonality/$openness/measurementError/measurementError${SLURM_ARRAY_TASK_ID}.Rout 