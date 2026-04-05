#!/bin/bash

#SBATCH --job-name=FOIEst_bootstrap_template.sh
#SBATCH --time=36:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --array=3
#SBATCH --ntasks-per-node=1 
#SBATCH --mem-per-cpu=4500
#SBATCH --account=pi-jozik

repStartIndex=$1
echo $repStartIndex
repEndIndex=$2
echo $repEndIndex

seasonality=seasonal
openness=closed
state=measurementError
roundIndex=3
arrivalType=exponential
prefix=sim
repID=0
ageLabel=0-5yrs 
method=TwoMomentApproximation
folder=eLifeSubAugust2024
saveDir0=/project2/pascualmm/QZ/PhD/projects/FOI/$folder/round$roundIndex/files/$arrivalType/FOIBootstrap/estimation/$method
readDir0=/project2/pascualmm/QZ/PhD/projects/FOI/$folder/round$roundIndex/files/$arrivalType/FOI/inputs/MOIPop
readDirDur0=/project2/pascualmm/QZ/PhD/projects/FOI/$folder/round$roundIndex/files/$arrivalType/FOI/inputs/dur

if [ -d "$saveDir0" ]; then
  echo "folder exists!"
else
  mkdir $saveDir0
fi

saveDir1=$saveDir0/$seasonality
if [ -d "$saveDir1" ]; then
  echo "folder exists!"
else
  mkdir $saveDir1
fi

saveDir2=$saveDir1/$openness
if [ -d "$saveDir2" ]; then
  echo "folder exists!"
else
  mkdir $saveDir2
fi

saveDir3=$saveDir2/${state}
if [ -d "$saveDir3" ]; then
  echo "folder exists!"
else
  mkdir $saveDir3
fi

saveDir4=$saveDir3/${prefix}_${SLURM_ARRAY_TASK_ID}
if [ -d "$saveDir4" ]; then
  echo "folder exists!"
else
  mkdir $saveDir4
fi

arrayIndices1=[1]
arrayIndices2=[2]
arrayIndices3=[3]
bootstrap=TRUE
scriptDir=/home/qizhan/others/PhD/projects/FOI/$folder/round$roundIndex/analysis/scripts/utils
if [[ ${arrayIndices1[*]} =~ $SLURM_ARRAY_TASK_ID ]]
then
  for layersLabel in Pre-IRS IRS
  do
    params=high
    Rscript $scriptDir/FOIest.R -i $readDir0/${seasonality}/${openness}/${state}/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_${layersLabel}_${ageLabel}.csv -c 30 -b $bootstrap -s $repStartIndex -e $repEndIndex -m $method -p $params -d $readDirDur0/${seasonality}/${openness}/${state}/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_${layersLabel}_${ageLabel}.RData -o $saveDir4/r${repID}_${layersLabel}_bs_${repStartIndex}-${repEndIndex}.RData -t 360
  done
elif [[ ${arrayIndices2[*]} =~ $SLURM_ARRAY_TASK_ID ]]
then
  layersLabel=IRS
  params=high
  Rscript $scriptDir/FOIest.R -i $readDir0/${seasonality}/${openness}/${state}/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_${layersLabel}_${ageLabel}.csv -c 30 -b $bootstrap -s $repStartIndex -e $repEndIndex -m $method -p $params -d $readDirDur0/${seasonality}/${openness}/${state}/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_${layersLabel}_${ageLabel}.RData -o $saveDir4/r${repID}_${layersLabel}_bs_${repStartIndex}-${repEndIndex}.RData -t 360
elif [[ ${arrayIndices3[*]} =~ $SLURM_ARRAY_TASK_ID ]]
then 
  layersLabel=IRS
  params=low
  Rscript $scriptDir/FOIest.R -i $readDir0/${seasonality}/${openness}/${state}/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_${layersLabel}_${ageLabel}.csv -c 30 -b $bootstrap -s $repStartIndex -e $repEndIndex -m $method -p $params -d $readDirDur0/${seasonality}/${openness}/${state}/${prefix}_${SLURM_ARRAY_TASK_ID}/r${repID}_${layersLabel}_${ageLabel}.RData -o $saveDir4/r${repID}_${layersLabel}_bs_${repStartIndex}-${repEndIndex}.RData -t 360
fi
