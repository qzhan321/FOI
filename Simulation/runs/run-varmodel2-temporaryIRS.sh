#!/bin/bash
#SBATCH --job-name=Dec30th
#SBATCH --time=36:00:00
#SBATCH --output=/home/qizhan/others/PhD/projects/FOI/eLifeSubAugust2024/round1/simulation/scripts/exponential/seasonal/closed/outputAndErrors/temporaryIRS/Dec30th_%A_%a.out
#SBATCH --error=/home/qizhan/others/PhD/projects/FOI/eLifeSubAugust2024/round1/simulation/scripts/exponential/seasonal/closed/outputAndErrors/temporaryIRS/Dec30th_%A_%a.err
#SBATCH --array=1-3
#SBATCH --tasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=12000
#SBATCH --partition=broadwl
#SBATCH --account=pi-jozik
#SBATCH --mail-type=ALL
#SBATCH --mail-user=qizhan@uchicago.edu

# Print this sub-job's task ID
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
module load gcc/6.1 # module load gcc/7.4.0 
module load python/cpython-3.7.0 # module load python/anaconda-2020.07 
module load R/3.6.1
# SLURM_ARRAY_TASK_ID=4
filePrefix=sim
seasonality=seasonal
openness=closed
code=varmodel2-master
runPrepDir=runPrepFiles
modelDir=runModelsDir
sqliteDir=sqlitesDir
arrivalType=exponential
roundIndex=1
remoteDir=/scratch/midway2/qizhan/PhD/projects/FOI/eLifeSubAugust2024/round$roundIndex/simulation/$arrivalType/$seasonality/$openness/${filePrefix}_${SLURM_ARRAY_TASK_ID}
cp -r /home/qizhan/others/PhD/projects/FOI/eLifeSubAugust2024/round$roundIndex/simulation/codes/$arrivalType/$code $remoteDir
cp -r /home/qizhan/others/PhD/projects/FOI/eLifeSubAugust2024/round$roundIndex/simulation/scripts/$arrivalType/$seasonality/$openness/runInputFiles $remoteDir/$runPrepDir
cd $remoteDir
cd $runPrepDir
python writeParameters.py -p ${filePrefix}_param_temporaryIRS.csv -i parameters-template.py -n $SLURM_ARRAY_TASK_ID -r 1 -x $filePrefix -s
cd ..

mkdir $modelDir
mkdir $sqliteDir
# build the model, run preIRS first
./build.py -p $runPrepDir/${filePrefix}_${SLURM_ARRAY_TASK_ID}_r0_input.py -d $modelDir/s0
# execute the run
cd $sqliteDir
../$modelDir/s0/bin/varMig
cd ..
