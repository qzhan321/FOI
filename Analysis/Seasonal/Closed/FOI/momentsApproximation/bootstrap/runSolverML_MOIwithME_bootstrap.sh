#!/bin/bash

#SBATCH --job-name=runSolverML_MOIwithME_bootstrap.sh
#SBATCH --time=36:00:00
#SBATCH --partition=caslake
#SBATCH --nodes=1
#SBATCH --array=1-200
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=12000
#SBATCH --account=pi-jozik

seasonality=seasonal
openness=closed
state=withME
run=5

Rscript /home/qizhan/others/PhD/projects/FOI/analysis$run/scripts/actualRuns/$seasonality/$openness/FOI/momentsApproximation/bootstrap/runSolverML_MOI${state}_bootstrap.R ${SLURM_ARRAY_TASK_ID} &> /home/qizhan/others/PhD/projects/FOI/analysis$run/scripts/actualRuns/$seasonality/$openness/FOI/momentsApproximation/bootstrap/outputAndErrors/runSolverML_MOI${state}_bootstrap${SLURM_ARRAY_TASK_ID}.Rout