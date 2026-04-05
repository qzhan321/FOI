#!/bin/bash

#SBATCH --job-name=FOIEst_bootstrap.sh
#SBATCH --time=36:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --array=1-40
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=1500
#SBATCH --account=pi-pascualmm

numReps=5
repStartIndex=$(( $numReps*($SLURM_ARRAY_TASK_ID - 1) + 1 ))
repEndIndex=$(( $numReps*($SLURM_ARRAY_TASK_ID - 1) + $numReps ))
echo $repStartIndex
echo $repEndIndex
sbatch ./FOIEst_bootstrap_template.sh $repStartIndex $repEndIndex
