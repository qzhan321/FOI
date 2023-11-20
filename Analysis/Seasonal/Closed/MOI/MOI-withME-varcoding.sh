#!/bin/bash

#SBATCH --job-name=MOI-withME-varcoding.sh
#SBATCH --time=36:00:00
#SBATCH --partition=caslake
#SBATCH --nodes=1
#SBATCH --array=1
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=38000
#SBATCH --account=pi-jozik

seasonality=seasonal
openness=closed
state=withME-varcoding
run=5
R CMD BATCH /home/qizhan/others/PhD/projects/FOI/analysis$run/scripts/actualRuns/$seasonality/$openness/MOI/MOI-$state.R /home/qizhan/others/PhD/projects/FOI/analysis$run/scripts/actualRuns/$seasonality/$openness/MOI/MOI-$state.Rout 