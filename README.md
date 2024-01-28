<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/cohmathonc/hprcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/drejom/hprcc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# hprcc <img src="man/figures/logo.png" align="right" height="120" alt="" />
  
An R Package to simplify running analyses on City of Hope clusters _Apollo_ and _Gemini_

## Features

**hprcc** configures [targets](https://books.ropensci.org/targets/) pipelines for COH clusters _Apollo_ and _Gemini_. Targets provides a simple but powerful framework for running R code that avoids unnecessary computation for tasks that are up to date, natively supports parallel computing, and abstracts files as R objects. Also included are some handy functions for logging R jobs run via SLURM.

You can easily configure your own SLURM resource requests (CPU, RAM, walltime) to run multiprocess jobs on cluster nodes with `create_controller()` or use pre-configured shortcuts according to the job type:

| Job Type | CPUs | Memory (GB) | Time (minutes) |
|----------|------|-------------|----------------|
| small    | 2    | 20          | 360            |
| medium   | 12   | 80          | 360            |
| large    | 20   | 200         | 720            |
| bigmem   | 10   | 500         | 360            |
| huge     | 40   | 100         | 120            |

For a complete list of configurable settings refer to the [package options](reference/package-options.html).

## Usage

Load the library via `_targets.R` and set resources within `tar_target()` steps. By default, all targets will run on the cluster with a `small` allocation.

```
library(targets)    
tar_script({
    library(hprcc)
    list(
        tar_target(y1, 1 + 1, deployment = "main"),
        tar_target(y2, 1 + 1),
        tar_target(z, y1 + y2, resources = medium)
    )}, ask = FALSE)
tar_make()
```

## Installation

To install this package, you can use the `remotes` package in R:

```r
library(remotes)
remotes::install_github("cohmathonc/hprcc")
```

## Contributing

Contributions are welcome! If you have any suggestions for improvements or new features, please feel free to open an issue or submit a pull request.
