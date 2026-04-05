#!/bin/bash

#SBATCH --job-name=measurementError.sh
#SBATCH --time=36:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --array=1
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=10000
#SBATCH --account=pi-pascualmm

seasonality=seasonal
openness=closed
roundIndex=3
arrivalType=exponential
folder=eLifeSubAugust2024
R CMD BATCH /home/qizhan/others/PhD/projects/FOI/$folder/round$roundIndex/analysis/scripts/$arrivalType/$seasonality/$openness/measurementError/measurementError${SLURM_ARRAY_TASK_ID}.R /home/qizhan/others/PhD/projects/FOI/$folder/round$roundIndex/analysis/scripts/$arrivalType/$seasonality/$openness/measurementError/measurementError${SLURM_ARRAY_TASK_ID}.Rout 