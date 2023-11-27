#!/bin/bash

#SBATCH --job-name=FOI-bootstrap-sim.sh
#SBATCH --time=36:00:00
#SBATCH --partition=caslake
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=16000
#SBATCH --account=pi-jozik

round=1
R CMD BATCH /home/qizhan/others/PhD/projects/FOI/writings/round$round/plots/FOI-bootstrap-sim.R /home/qizhan/others/PhD/projects/FOI/writings/round$round/plots/FOI-bootstrap-sim.Rout 
