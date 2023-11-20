#!/bin/bash
#SBATCH --job-name=Nov19th
#SBATCH --time=36:00:00
#SBATCH --output=/home/qizhan/others/PhD/projects/FOI/simulation5/scripts/actualRuns/seasonal/closed/outputAndErrors/temporaryIRS/Nov19th_%A_%a.out
#SBATCH --error=/home/qizhan/others/PhD/projects/FOI/simulation5/scripts/actualRuns/seasonal/closed/outputAndErrors/temporaryIRS/Nov19th_%A_%a.err
#SBATCH --array=1-3
#SBATCH --tasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=48000
#SBATCH --partition=caslake
#SBATCH --account=pi-jozik
#SBATCH --mail-type=ALL
#SBATCH --mail-user=qizhan@uchicago.edu

# Print this sub-job's task ID
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
module load gcc/7.4.0 # gcc/6.1
module load python # python/cpython-3.7.0
module load R
# SLURM_ARRAY_TASK_ID=4
filePrefix=sim
seasonality=seasonal
openness=closed
code=varmodel2-master
runCategory=actualRuns
runPrepDir=runPrepFiles
modelDir=runModelsDir
sqliteDir=sqlitesDir
remoteDir=/scratch/midway2/qizhan/PhD/projects/FOI/simulation5/$runCategory/$seasonality/$openness/${filePrefix}_${SLURM_ARRAY_TASK_ID}
cp -r /home/qizhan/others/PhD/projects/FOI/simulation5/codes/$code $remoteDir
cp -r /home/qizhan/others/PhD/projects/FOI/simulation5/scripts/$runCategory/$seasonality/$openness/runInputFiles $remoteDir/$runPrepDir
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
