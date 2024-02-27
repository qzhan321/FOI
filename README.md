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
**FOIest.R** estimates FOI values based on MOI values. It requires a .csv file which contains MOI information of the sampled population as the input. We illustrate the application of the methods with MOI estimates obtained via a recently published approach known as “*var*coding” and based on molecular data from the multigene family *var* which encodes the major variant surface antigen during the blood stage of infection [Tiedje and Zhan et al., *eLife*, 2023.](https://doi.org/10.7554/eLife.91411.1). But the methods can be applied to MOI estimates obtained in different ways, not necessarily the one adopted here.

#### The under-sampling of infections issue
The empirical MOI estimates in many epidemiological studies, rely on individuals who are microscopy-positive. Given the sensitivity of microscopy, a significant fraction of individuals who carry infections are not detected. In other cases, MOI estimates are obtained for individuals who are PCR-positive. PCR is considerably more sensitive than microscopy, detecting a higher fraction, if not 100%, of individuals with *P. falciparum* infections. However, a small fraction of individuals who are infected but whose infections are not detected, i.e., MOI = 0. 

The input dataframe has two columns. The first column has MOI values of all sampled individual hosts, ranging from 0, 1, ..., up to some carrying capacity of blood-stage infection. The second column has the corresponding number or proportion/probability of sampled individuals with any specific MOI value. Examples of the inputs are given in the following [section](#Command). 

When applying our methods, users should address the under-sampling of infections in estimates of MOI in empirical surveys. For example, users can impute individuals who are infected but whose infections are not detected by the sequencing and typing. One straightforward way to    




