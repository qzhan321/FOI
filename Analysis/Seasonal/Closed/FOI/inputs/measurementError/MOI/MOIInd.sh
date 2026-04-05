#!/bin/bash

#SBATCH --job-name=MOIInd.sh
#SBATCH --time=36:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --array=1-3
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=1500
#SBATCH --account=pi-pascualmm

seasonality=seasonal
openness=closed
arrivalType=exponential
roundIndex=3
prefix=sim
ageLabel=0-5yrs 
repID=0
state=measurementError
folder=eLifeSubAugust2024
readDir0=/project2/pascualmm/QZ/PhD/projects/FOI/$folder/round$roundIndex/files/$arrivalType/FOI/inputs/MOIInd/MOIEstInputs
saveDir0=/project2/pascualmm/QZ/PhD/projects/FOI/$folder/round$roundIndex/files/$arrivalType/FOI/inputs/MOIInd/MOIEst/$seasonality
if [ -d "$saveDir0" ]; then
  echo "folder exists!"
else
  mkdir $saveDir0
fi

saveDir1=$saveDir0/$openness
if [ -d "$saveDir1" ]; then
  echo "folder exists!"
else
  mkdir $saveDir1
fi

saveDir2=$saveDir1/$state
if [ -d "$saveDir2" ]; then
  echo "folder exists!"
else
  mkdir $saveDir2
fi

saveDir3=$saveDir2/${prefix}_${SLURM_ARRAY_TASK_ID}
if [ -d "$saveDir3" ]; then
  echo "folder exists!"
else
  mkdir $saveDir3
fi

MOIEstDir=/home/qizhan/others/PhD/projects/BayesianMOI/eLifeRevisionMay2024/major/reviewer1/point1
repertoireSizeDistDir=/project2/pascualmm/QZ/PhD/projects/FOI/$folder/round$roundIndex/files/$arrivalType/FOI/inputs/MOIInd/repertoireSizeDist
nRealizations=200
if [ $SLURM_ARRAY_TASK_ID -eq 1 ] 
then 
  for time in 71580 71820 72660 72900
  do
    for nRealization in $(seq 1 $nRealizations); do
      Rscript $MOIEstDir/MOI_estimation.R -i $readDir0/$seasonality/$openness/$state/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_time_${time}_${ageLabel}_realization${nRealization}.csv -m 30 -r $repertoireSizeDistDir/repertoireSizeDistribution.csv -t "count" -p "uniform" -s "medium" -v TRUE -a "mixtureDist" -o $saveDir3/r${repID}_time_${time}_${ageLabel}_realization${nRealization}.RData
    done
  done
else 
  for time in 72660 72900
  do 
    for nRealization in $(seq 1 $nRealizations); do
      Rscript $MOIEstDir/MOI_estimation.R -i $readDir0/$seasonality/$openness/$state/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_time_${time}_${ageLabel}_realization${nRealization}.csv -m 30 -r $repertoireSizeDistDir/repertoireSizeDistribution.csv -t "count" -p "uniform" -s "medium" -v TRUE -a "mixtureDist" -o $saveDir3/r${repID}_time_${time}_${ageLabel}_realization${nRealization}.RData
    done
  done
fi