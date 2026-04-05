#!/bin/bash

#SBATCH --job-name=plot.sh
#SBATCH --time=36:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --array=1
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=1300
#SBATCH --account=pi-jozik

roundIndex=3
folder=eLifeSubAugust2024
arrivalType=exponential
seasonality=seasonal
openness=closed
state=missingData
R CMD BATCH /home/qizhan/others/PhD/projects/FOI/$folder/round$roundIndex/analysis/scripts/$arrivalType/$seasonality/$openness/FOI/estimation/$state/visualizeBootstrapDist/sim.R /home/qizhan/others/PhD/projects/FOI/$folder/round$roundIndex/analysis/scripts/$arrivalType/$seasonality/$openness/FOI/estimation/$state/visualizeBootstrapDist/plot.Rout