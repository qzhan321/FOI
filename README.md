# Overview

This repository documents the simulation code, experimental design, and analysis scripts used to generate the figures for From multiplicity of infection to force of infection for sparsely sampled *Plasmodium falciparum* populations at high transmission [Zhan et al, 2024, *eLife*](https://doi.org/10.7554/eLife.100076.3).


# FOI Estimation from MOI Data

Queueing theory approaches are implemented for inferring force of infection (FOI) from multiplicity of infection (MOI) data.

Two inference methods are implemented:

- **Two-Moment Approximation**
- **Little’s Law**

The framework supports:
- bootstrap uncertainty estimation,
- finite blood-stage carrying capacity (for the Two-Moment Approximation method),
- non-Poisson infection arrival processes,
- non-exponential infection durations,
- and user-defined transmission intensity search ranges.

---

# Methodological Background

MOI measures the number of genetically distinct parasite strains co-infecting a host. Because MOI reflects the balance between infection arrivals and infection duration, queueing theory can be used to infer FOI from MOI distributions.

This repository implements two approaches:

## 1. Little’s Law

FOI is estimated directly using:

```text
FOI = mean(MOI) / mean infection duration (in days) × days per year
```

This method is simple and computationally efficient.

---

## 2. Two-Moment Approximation

This method:
- models MOI as a queue-length distribution,
- searches over candidate values for the mean and variance of inter-arrival times between infection events,
- compares expected and observed MOI distributions,
- and identifies parameter combinations that best fit the data.

---

# Requirements

## R version

Tested with:

```text
R 3.6.1
```

---

## Required packages

Install required packages with:

```r
install.packages(c(
  "dplyr",
  "optparse",
  "RSQLite"
))
```

The script imports:

```r
library(dplyr)
library(optparse)
library(RSQLite)
```

---

# Input File Format

The input file must be a CSV containing MOI information.

Two formats are supported.

---

## Option 1: MOI counts

```csv
MOI,Count
0,20
1,35
2,18
3,7
```

where:
- `MOI` = multiplicity of infection,
- `Count` = number of hosts with that MOI.

---

## Option 2: Probabilities

```csv
MOI,N,Prob
0,100,0.20
1,100,0.35
2,100,0.18
3,100,0.07
```

where:
- `MOI` = multiplicity of infection,
- `N` = sample size,
- `Prob` = frequency/probability.

---

# Usage

```bash
Rscript FOIest.R [options]
```

---

# Command-Line Arguments

| Argument | Description |
|---|---|
| `-i`, `--inputFile` | Path to input CSV containing MOI information |
| `-c`, `--bloodStageCarryingCapacity` | Carrying capacity for blood-stage infections (default = 30) |
| `-b`, `--bootstrap` | Whether to run bootstrap analysis (`TRUE` or `FALSE`) |
| `-s`, `--replicateStartIndexBootstrap` | Start index for bootstrap replicates |
| `-e`, `--replicateEndIndexBootstrap` | End index for bootstrap replicates |
| `-m`, `--method` | FOI estimation method: `TwoMomentApproximation` or `LittlesLaw` |
| `-p`, `--paramRange` | Transmission intensity search range |
| `-d`, `--durationInformation` | Optional `.RData` file containing infection duration statistics |
| `-o`, `--output` | Output `.RData` file |
| `-t`, `--T_YEAR` | Number of days per year (default = 365) |

---

# Transmission Intensity Ranges and Grid Search

For the `TwoMomentApproximation` method, FOI is inferred by searching over candidate values of:

- the mean inter-arrival time between infection events,
- and the variance of inter-arrival times.

The argument:

```bash
--paramRange
```

controls the search grid.

---

## Available transmission intensity ranges

| `paramRange` | `meanInterarrivals` grid | `VarInterArrivals` grid | Intended use |
|---|---:|---:|---|
| `high` | `seq(1, 150, 1)` | `seq(0, 160000, 100)` | High-transmission settings |
| `medium` | `seq(1, 300, 1)` | `seq(0, 360000, 100)` | Moderate-transmission settings |
| `low` | `seq(1, 700, 1)` | `seq(0, 2250000, 500)` | Low-transmission settings |
| `verylow` | `seq(1, 2100, 2)` | `seq(0, 6250000, 2000)` | Very low-transmission settings |

---

## Relationship between inter-arrival time and FOI

FOI is calculated as:

```text
FOI = T_YEAR / meanInterarrival
```

For example, assuming:

```text
T_YEAR = 365
```

| Mean inter-arrival time | Approximate FOI |
|---:|---:|
| 30 days | 12.17 infections/year |
| 100 days | 3.65 infections/year |
| 365 days | 1 infection/year |
| 730 days | 0.5 infections/year |

Shorter inter-arrival times correspond to higher transmission intensity.

---

## Interpretation of variance in inter-arrival times

`VarInterArrivals` controls the variability of infection arrivals.

- Small variance corresponds to relatively regular arrivals.
- Large variance corresponds to heterogeneous or bursty arrivals.

This flexibility allows the method to accommodate departures from homogeneous Poisson infection processes.

---

## Choosing a transmission intensity range

Use:
- `high` for high-transmission regions,
- `medium` for moderate-transmission settings,
- `low` or `verylow` for regions with sparse infections.

If transmission intensity is uncertain, `medium` is a reasonable starting point.

---

# Customizing Transmission Intensity Search Grids

Users may customize the parameter search grids used in the Two-Moment Approximation method by editing the `runTwoMomentsApproximation()` function.

The predefined transmission intensity settings (`high`, `medium`, `low`, and `verylow`) correspond to different ranges and resolutions for:
- the mean inter-arrival times between infection events (`meanInterarrivals`),
- and the variance of inter-arrival times (`VarInterArrivals`).

The current implementation contains:

```r
if (paramRange == "high") {
  VarInterArrivals = seq(0,160000,100)
  meanInterarrivals = seq(1,150,1)

} else if (paramRange == "medium") {
  VarInterArrivals = seq(0,360000,100)
  meanInterarrivals = seq(1,300,1)

} else if (paramRange == "low") {
  VarInterArrivals = seq(0,2250000,500)
  meanInterarrivals = seq(1,700,1)

} else if (paramRange == "verylow") {
  VarInterArrivals = seq(0,6250000,2000)
  meanInterarrivals = seq(1,2100,2)
}
```

Users may:
- modify the ranges or step sizes of the predefined transmission intensity settings,
- redefine the search resolution,
- or add entirely new custom search grids.

For example, users may redefine the `medium` setting:

```r
} else if (paramRange == "medium") {
  VarInterArrivals = seq(0,500000,250)
  meanInterarrivals = seq(1,400,2)
}
```

Alternatively, users may add a completely new option:

```r
} else if (paramRange == "custom") {
  VarInterArrivals = seq(0,1000000,250)
  meanInterarrivals = seq(10,500,2)
}
```

and then run:

```bash
Rscript estimateFOI.R \
  -i moi_distribution.csv \
  -m TwoMomentApproximation \
  -p custom \
  -b TRUE \
  -s 1 \
  -e 100 \
  -o FOI_custom.RData
```

# Runtime Considerations

The total number of parameter combinations is approximately:

```text
length(meanInterarrivals) × length(VarInterArrivals)
```

multiplied by the number of bootstrap replicates.

For example, the `medium` grid evaluates approximately:

```text
300 × 3601 = 1,080,300
```

parameter combinations.

Larger search grids, defined by wider parameter ranges and/or smaller step sizes, increase runtime substantially because the algorithm evaluates more combinations of mean and variance values for inter-arrival times.

---

## Strategies for reducing runtime

Users can reduce computation time by:
- narrowing the inter-arrival time range,
- increasing step sizes,
- reducing variance ranges,
- reducing bootstrap replicates,
- or first performing non-bootstrap exploratory runs.

A recommended workflow is:

1. Run a broad grid without bootstrap.
2. Identify plausible parameter regions.
3. Define a narrower custom grid.
4. Run bootstrap analysis on the narrowed region.

---

# Infection Duration Information

If no duration file is supplied, the script uses default estimates derived from historical malaria therapy datasets:

```r
meanServiceT <- 208.4681
VarServiceT <- 10817.6
```

Users may alternatively provide a custom `.RData` object containing:

```r
meanDuration
varDuration
```

Example:

```bash
--durationInformation durationData.RData
```

---

# Bootstrap Analysis

When bootstrap is enabled:

```bash
--bootstrap TRUE
```

the script:
1. resamples MOI observations with replacement,
2. re-estimates FOI,
3. and generates bootstrap distributions for uncertainty estimation.

Bootstrap replicates are controlled using:

```bash
--replicateStartIndexBootstrap
--replicateEndIndexBootstrap
```

Example:

```bash
--replicateStartIndexBootstrap 1 \
--replicateEndIndexBootstrap 100
```

---

# Examples

## Example 1: Two-Moment Approximation

```bash
Rscript estimateFOI.R \
  -i moi_distribution.csv \
  -m TwoMomentApproximation \
  -p medium \
  -b TRUE \
  -s 1 \
  -e 100 \
  -o FOI_two_moment.RData
```

---

## Example 2: Little’s Law

```bash
Rscript estimateFOI.R \
  -i moi_distribution.csv \
  -m LittlesLaw \
  -b TRUE \
  -s 1 \
  -e 100 \
  -o FOI_littles_law.RData
```

---

# Output

The script saves an R object named:

```r
FOI
```

to the specified `.RData` file.

---

## Output for Little’s Law

| Column | Description |
|---|---|
| `FOI` | Estimated force of infection |
| `rep` | Bootstrap replicate index |

---

## Output for Two-Moment Approximation

| Column | Description |
|---|---|
| `negLogLikelihood` | Negative log-likelihood |
| `meanInterarrival` | Mean inter-arrival time |
| `VarInterArrival` | Variance of inter-arrival time |
| `rep` | Bootstrap replicate index |
| `FOI` | Estimated force of infection |

where:

```text
FOI = T_YEAR / meanInterarrival
```

---

# Notes

The script currently sources helper functions from:

```r
source("/home/qizhan/others/PhD/projects/FOI/eLifeSubAugust2024/round1/analysis/scripts/utils/funcs.R")
```

For public release, this should be replaced with a relative path such as:

```r
source("scripts/utils/funcs.R")
```

to improve portability across systems.

---

## License

This project is licensed under the MIT License.
