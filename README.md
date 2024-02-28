# FOI estimation: Two-moment Approximation and Little's Law.

## Contents
- [Overview](#Overview)
- [Applying the Two Methods to New Datasets of MOI Information](#Applying-the-Bayesian-Formulation-of-MOI-Estimation-to-New-Datasets)
  - [Running the Script](#Running-the-Script)
    - [The Under-sampling of Infections Issue](#The-Under-sampling-of-Infections-Issue)
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

**FOIest.R** requires a .csv file which contains the MOI information of the sampled population as input. The input dataframe has either two or three columns. The first column contains MOI values of all sampled individual hosts, ranging from 0, 1, ..., up to some carrying capacity of blood-stage infection, i.e., the maximum possible value for MOI. The second column contains the corresponding number/count, or proportion/probability of sampled individuals with any specific MOI value. When the second column contains the proportion, a third column should be provided which records the total number of individuals sampled. Examples of the inputs are given in the following [section](#The-Under-sampling-of-Infections-Issue). 

#### The Under-sampling of Infections Issue
The empirical MOI estimates in many epidemiological studies, rely on individuals who are microscopy-positive. Given the sensitivity of microscopy, a significant fraction of individuals who carry infections are not detected. In other cases, MOI estimates are obtained for individuals who are PCR-positive. PCR is considerably more sensitive than microscopy, detecting a higher fraction, if not 100%, of individuals with *P. falciparum* infections. However, a small fraction of individuals who are infected with a single infection or multiple infections can still be undetected and have 0 assigned to be their MOI values. 

When applying our FOI estimation methods, users should address the under-sampling of infections issue in estimates of MOI in empirical surveys. Users can impute MOI values for individuals who are infected but whose infections are not detected by the sequencing and typing procedure, i.e., their MOI values are falsely assigned to be 0. One straightforward way to do this imputation is to sample from MOI values of individuals who are positively infected and detected and have non-zero MOI values. 

Consider the following toy example. Let's assume we sample 10 individuals in a population. We use microscopy to detect how many of the 10 individuals are positively infected, which turns out to be 4. We then sequence and type *var* genes from these 4 indidividuals, and estimate their MOI values to be 1,2,3,4 respectively. Given the detection power of microscopy, i.e., the probability of a positively infected individual being detected by microscopy, we can calculate the total number of individuals who are positively infected. Assuming that the detection power is 0.8, then there are in total 5 individuals who are positively infected. So, there is 1 individual among the remaining 6 individuals who are positively infected but undetected by microscopy and whose MOI value is falsely assigned to be 0. We then sample a value from the aforementioned 4 individuals (whose MOI values are 1,2,3,4 respectively), to be the MOI estimate of this individual with missing MOI information. Let's say 3. Thus, the MOI information for all 10 indivduals are: 

| MOI | Count |
| :--: | :--: | 
| 0 | 5 |
| 1 | 1 |
| 2 | 1 |
| 3 | 2 |
| 4 | 1 |

| MOI | Prob | N |
| :--: | :--: | :--: | 
| 0 | 0.5 | 10 |
| 1 | 0.1 | 10 |
| 2 | 0.1 | 10 |
| 3 | 0.2 | 10 |
| 4 | 0.1 | 10 |

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
|  `i`  | inputFile. The full path and name of the input file containing MOI information. It is in .csv file format with two or three columns: MOI values ("MOI"), the number of individuals or the proportion of individuals with specific MOI values ("Count" or "Prob"), and the total number of sampled individuals (only applicable if the second column cotains the proportion information) ("N"). |
|  `c`  | bloodStageCarryingCapacity. The carrying capacity of blood-stage infections, i.e., the maximum possible value for blood-stage MOI. |
|  `b`  | bootstrap. Whether running bootstrap analysis for FOI estimation or not. Bootstrap analysis generates confidence intervals for the estimated FOI values. When set to be FALSE, estimation of FOI values is based on the original MOI information. When set to be TRUE, the program first samples with replacement from the original MOI information, and then performs FOI estimation, which will repeat as many times as users specify in the following two parameters (`s` and `e`). |
|  `s`  | replicateStartIndexBootstrap. When running bootstrap analysis, specifying the start index of the replicates performed. |
|  `e`  | replicateEndIndexBootstrap. When running bootstrap analysis, specifying the end index of the replicates performed. For example, when performing 200 replicates, to speed things up, users can divide the 200 replicates into 20 jobs, i.e., 10 replicates per job, and then run these jobs in parallel. For the first job, the `s` is 1 and `e` is 10, and for the second job, the `s` is 11 and `e` is 20, and so on. More details are included in the following [section](#Efficiency). |
|  `m`  | method. Which method for FOI estimation; either "TwoMomentApproximation" or "LittlesLaw". |
|  p`  | paramRange. Four options: "verylow", "low", "medium", "high". This parameter specifies users' belief on the transmission intensity or force of infection of the sampled population. It can be some rough range: "high" corresponds to annual mean FOI per individual host > 4; "medium" corresponds to annual mean FOI per individual host within [2,4]; "low" corresponds to annual mean FOI per individdual host within [1,2]; "verylow" corresponds to annual mean FOI per individual host <1. This parameter can help define the parameter space searched when estimating FOI values. |
|  `o`  | The full path to the directory where the output will be saved and the name of the output file. For example, "FOI.RData". |


#### Output
The above example command will output a matrix containing FOI information. 

When set the method `m` to be "LittlesLaw" without boostrap analysis:
| FOI | 
| :--: 
| 4.878125 | 

When set the method `m` to be "LittlesLaw" with 5 replicates for boostrap analysis:
| FOI | rep |
| :--: | :--: | 
| 4.553247 | 1 |
| 4.868205 | 2 |
| 5.230282 | 3 |
| 4.992204 | 4 |
| 4.781406 | 5 |

When set the method `m` to be "TwoMomentApproximation" without boostrap analysis, the program returns the top 10 FOI estimates which minimize the negative log likelihood of observing the empirical MOI distribution. Users can use the top FOI estimate, or the average of top 3 estimates, to be the final estimated FOI values. 
| negLogLikelihood | meanInterarrival" | VarInterArrival | FOI |
| :--: | :--: | :--: | :--: | 
| 645.4679 | 72 | 77500 | 5.069444 |
| 645.4686 | 72 | 77000 | 5.069444 |
| 645.4703 | 72 | 78000 | 5.069444 |
| 645.4724 | 72 | 76500 | 5.069444 |
| 645.4731 | 73 | 78500 | 5 |
| 645.4736 | 73 | 78000 | 5 |
| 645.4756 | 73 | 79000 | 5 |
| 645.4758 | 72 | 78500 | 5.069444 |
| 645.4773 | 73 | 77500 | 5 |
| 645.4794 | 72 | 76000 | 5.069444 |

When set the method `m` to be "TwoMomentApproximation" with 5 replicates for boostrap analysis:
| negLogLikelihood | meanInterarrival" | VarInterArrival | rep | FOI |
| :--: | :--: | :--: | :--: | :--: |  
| 630.3208 | 78 | 82000 | **1** | 4.679487 |
| 630.3210 | 78 | 81500 | **1** | 4.679487 |
| 630.3233 | 78 | 82500 | **1** | 4.679487 |
| 630.3239 | 78 | 81000 | **1** | 4.679487 |
| 630.3247 | 79 | 83000 | **1** | 4.620253 |
| 630.3248 | 79 | 82500 | **1** | 4.620253 |
| 630.3271 | 79 | 83500 | **1** | 4.620253 |
| 630.3277 | 79 | 82000 | **1** | 4.620253 |
| 630.3284 | 78 | 83000 | **1** | 4.679487 |
| 630.3297 | 78 | 80500 | **1** | 4.679487 |
......
| 676.8139 | 64 | 64500 | **10** | 5.703125 |
| 676.8151 | 64 | 65000 | **10** | 5.703125 |
| 676.8174 | 64 | 64000 | **10** | 5.703125 |
| 676.8208 | 64 | 65500 | **10** | 5.703125 |
| 676.8243 | 65 | 65500 | **10** | 5.615385 |
| 676.8256 | 64 | 63500 | **10** | 5.703125 |
| 676.8264 | 65 | 66000 | **10** | 5.615385 |
| 676.8266 | 65 | 65000 | **10** | 5.615385 |
| 676.8268 | 63 | 64000 | **10** | 5.793651 |
| 676.8268 | 63 | 63500 | **10** | 5.793651 |

#### Efficiency
Running bootstrap analysis to derive the confidence intervals of the estimated FOI values can take time. Users should divide all replicates into different jobs and run these jobs in parallel. 200 replicates has been tested and proven to approach a similar coefficient of variation than a higher number of replicates [Efron et al., 1994](https://doi.org/10.1201/9780429246593). Thus, let's assume users want to run 200 replicates. They can embed the above command line to bash scripts and run them on a computational cluster. Specifically, users can submit a first job which request a node and some memory allocation for the first 10 replicates, a second job which another node and some memory allocation for the second 10 replicates, and so on. By running these 10 jobs in parallel, users reduce the amount of running time for bootstrap analysis.

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


