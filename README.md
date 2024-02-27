# FOI estimation: Two-moment Approximation and Little's Law.

## Contents
- [Overview](#Overview)
- [Applying the Two Methods to New Datasets of MOI Information](#Applying-the-Bayesian-Formulation-of-MOI-Estimation-to-New-Datasets)
  - [Running the Script](#Running-the-Script)
    - [The under-sampling of infections issue](#The-under-sampling-of-infections-issue)
    - [Command](#Command)
    - [Example Command](#Example-Command)
    - [Command Arguments](#Command-Arguments)
    - [Output](#Output)
    - [Help](#Help)
- [Contact](#Contact)

## Overview
This branch contains cleaned versions of code to facilitate the application of two methods for FOI estimation: the Two-Moment Approximation and Little's Law. More details regarding the two methods can be found in [Zhan et al., medRxiv, 2024.](https://doi.org/10.1101/2024.02.12.24302148). 

## Applying the Two Methods to New Datasets of MOI Information
The script **[FOIest.R](https://github.com/qzhan321/FOI/blob/FOIEst/FOIest.R)** is needed. 

### Running the Script
**FOIest.R** estimates FOI values based on MOI values. It requires a .csv file which contains MOI information of the sampled population as the input. We illustrate the application of the methods with MOI estimates obtained via a recently published approach known as “varcoding” and based on molecular data from the multigene family var which encodes the major variant surface antigen during the blood stage of infection [Tiedje and Zhan et al., *eLife*, 2023.](https://doi.org/10.7554/eLife.91411.1). But the methods can be applied to MOI estimates obtained in different ways, not necessarily the one adopted here.

#### Pre-processing
The empirical MOI estimates in many epidemiological studies, including ours in Bongo District from northern Ghana, rely on individuals who are microscopy-positive \citep{bayesianmoi, MOI4}. Given the sensitivity of microscopy, a significant fraction of individuals who carry infections are not detected. A subset of the Ghana surveys includes in addition submicroscopic infections detected by PCR \citep{bayesianmoi, MOI4}. PCR is considerably more sensitive than microscopy, detecting a higher fraction, if not 100\%, of individuals with \textit{P. falciparum} infections. Using surveys for which both microscopy and PCR detection were used, we estimated a conversion factor between the proportion of hosts that are microscopy-positive and those that are PCR-positive within the targeted age group, i.e., children of 1-5 years old, which is 0.711329.

The input dataframe has several columns, with the first one being host IDs, or any type of identifiers of hosts, and the second column being the number of **non-upsA** DBLα types sequenced and typed in each individual corresponding host. Below is an example of the first two rows of an input matrix:



