# FOI estimation: Two-moment Approximation and Little's Law.

## Contents
- [Overview](#Overview)
- [Applying the Two Methods to New Datasets of MOI Information](#Applying-the-Bayesian-Formulation-of-MOI-Estimation-to-New-Datasets)
  - [Running the Script](#Running-the-Script)
    - [The under-sampling of infections issue](#The-under-sampling-of-infections-issue)
    - [Command](#Command)
    - [Example Command](#Example-Command)
    - [Command Arguments](#Command-Arguments)
    - [Efficiency](#Efficiency)
    - [Help](#Help)
- [Contact](#Contact)

## Overview
This branch contains cleaned versions of code to facilitate the application of two methods for FOI estimation: the Two-Moment Approximation and Little's Law. More details regarding the two methods can be found in [Zhan et al., medRxiv, 2024.](https://doi.org/10.1101/2024.02.12.24302148) 

## Applying the Two Methods to New Datasets of MOI Information
The script **[FOIest.R](https://github.com/qzhan321/FOI/blob/FOIEst/FOIest.R)** is needed. 

### Running the Script
**FOIest.R** estimates FOI values based on MOI values. We illustrate the application of the methods with MOI estimates obtained via a recently published approach known as “*var*coding” and based on molecular data from the multigene family *var* which encodes the major variant surface antigen during the blood stage of infection [Tiedje and Zhan et al., *eLife*, 2023.](https://doi.org/10.7554/eLife.91411.1) But the methods can be applied to MOI estimates obtained in different ways, not necessarily the one adopted here.

#### The under-sampling of infections issue
The empirical MOI estimates in many epidemiological studies, rely on individuals who are microscopy-positive. Given the sensitivity of microscopy, a significant fraction of individuals who carry infections are not detected. In other cases, MOI estimates are obtained for individuals who are PCR-positive. PCR is considerably more sensitive than microscopy, detecting a higher fraction, if not 100%, of individuals with *P. falciparum* infections. However, a small fraction of individuals who are infected but whose infections are not detected, i.e., MOI = 0. 

**FOIest.R** requires a .csv file which contains MOI information of the sampled population as the input. The input dataframe has either two or three columns. The first column has MOI values of all sampled individual hosts, ranging from 0, 1, ..., up to some carrying capacity of blood-stage infection. The second column has the corresponding number or proportion/probability of sampled individuals with any specific MOI value. For the latter, a third column is needed which records the total number of individuals sampled. Examples of the inputs are given in the following paragraph. 

When applying our methods, users should address the under-sampling of infections in estimates of MOI in empirical surveys. Users can impute MOI values for individuals who are infected but whose infections are not detected by the sequencing and typing procedure, i.e., their MOI values are falsely assigned to be 0. One straightforward way for this imputation is to sample from MOI values of those individuals with their infections detected. Consider the following toy example. Let's assume we sample 10 individuals in a population. We use microscopy to detect how many of the 10 individuals are positively infected, which is 4. We then sequence and type *var* genes from these 4 indidividuals, and estimate their MOI values to be 1,2,3,4 respectively. If we have information about the detection power of microscopy, i.e., the probability of a positively infected individuals being detected by microscopy as positive, we can calculate the number of individuals among the rest 6 ones who are positively infected but whose infections are not detected and whose MOI values are falsely assigned to be 0. Assuming the detection power to be 0.8, then one individual among the remaining 6 is actually positively infected. We can sample a MOI value from the aforementioned four individuals whose MOI values are 1,2,3,4 respectively, to be the MOI value of this individual with missing MOI information. Let's say 3. The MOI information for these 10 indivduals are: 

| MOI | Count |
| :--: | :--: | 
| `0` | 5 |
| `1` | 1 |
| `2` | 1 |
| `3` | 2 |
| `4` | 1 |

| MOI | Prob | N |
| :--: | :--: | :--: | 
| `0` | 0.5 | 10 |
| `1` | 0.1 | 10 |
| `2` | 0.1 | 10 |
| `3` | 0.2 | 10 |
| `4` | 0.1 | 10 |

#### Command
```bash
Rscript FOIest.R -i "path/to/directory/inputFile" -c 30 -b TRUE -s 1 -e 10 -m "TwoMomentApproximation" -p "high" -o "/path/to/directory/outFile"
```

#### Example Command 
```bash
Rscript FOIest.R -i "/Users/John/Downloads/survey_1_MOI.csv" -c 30 -b TRUE -s 1 -e 10 -m "TwoMomentApproximation" -p "high" -o "/Users/John/Downloads/survey_1_FOI.RData" 
```

#### Command Arguments
|  Name | Description |
|  :-:  | :---------: | 
|  `i`  | inputFile. The full path and name of the input file containing MOI information. It is in .csv file format with two columns: MOI values "MOI", and the number of individuals, or the proportion of individuals with specific MOI values "Count" or "Prob". |
|  `c`  | bloodStageCarryingCapacity. The carrying capacity for blood-stage infections, i.e., the maximum value for blood-stage MOI. |
|  `b`  | bootstrap. Whether running bootstrap analysis for FOI estimation or not to generate confidence intervals for the estimated FOI values. When FALSE, estimate FOI values based on the original MOI information. When TRUE, sample with replacement from the original MOI information, and then perform FOI estimation; repeat with many replicates to derive the confidence intervals of the estimated FOI values. |
|  `s`  | replicateStartIndexBootstrap. When running bootstrap analysis, specifying the start index of the replicates performed. |
|  `e`  | replicateEndIndexBootstrap. When running bootstrap analysis, specifying the end index of the replicates performed. |
|  `m`  | method. Which method for FOI estimation; either "TwoMomentApproximation" or "LittlesLaw". |
|  p`  | paramRange. Four options: "verylow", "low", "medium", "high". This parameter specifies users' belief on the transmission intensity or force of infection of the sampled population, which can help narrow down the parameter space searched for FOI estimation. |
|  `o`   | The full path to the directory where the output will be saved and the name of the output file. For example, "FOI.RData". |


#### Efficiency
Running bootstrap analysis to derive the confidence intervals of the estimated FOI values can take time. Users should run replicates in parallel. 200 replicates has been tested and proven to approach a similar coefficient of variation than a higher number of replicates. For example, let's assume we run 200 replicates. Users can embed the above command line to a bash script and run it on a computational cluster. One can request a node for the first 10 replicates, and a second node for the second 10 replicates, and so on.
##### file1.sh
```bash
#SBATCH --array=1
#SBATCH --tasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2000
Rscript FOIest.R -i "path/to/directory/inputFile" -c 30 -b TRUE -s 1 -e 10 -m "TwoMomentApproximation" -p "high" -o "/path/to/directory/outFile"
```
##### file2.sh
```bash
#SBATCH --array=1
#SBATCH --tasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2000
Rscript FOIest.R -i "path/to/directory/inputFile" -c 30 -b TRUE -s 11 -e 20 -m "TwoMomentApproximation" -p "high" -o "/path/to/directory/outFile"
```
......
##### file20.sh
```bash
#SBATCH --array=1
#SBATCH --tasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2000
Rscript FOIest.R -i "path/to/directory/inputFile" -c 30 -b TRUE -s 191 -e 200 -m "TwoMomentApproximation" -p "high" -o "/path/to/directory/outFile"
```


#### Help
Run the command below to print out help page.
```bash
Rscript FOIest.R --help
```
Users can refer to the help page for the definition of each parameter, their default values, and all the possible options for their values.

## Contact
If you run into any issues using the method, feel free to open a new [issue](https://github.com/qzhan321/FOI/issues).


