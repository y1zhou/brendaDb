
# brendaDb

<!-- Badges: start -->

[![Build Status](https://travis-ci.com/y1zhou/brendaDb.svg?branch=master)](https://travis-ci.com/y1zhou/brendaDb)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/y1zhou/brendaDb?branch=master&svg=true)](https://ci.appveyor.com/project/y1zhou/brendaDb)
[![codecov](https://codecov.io/gh/y1zhou/brendaDb/branch/master/graph/badge.svg)](https://codecov.io/gh/y1zhou/brendaDb)
[![Commitizen
friendly](https://img.shields.io/badge/commitizen-friendly-brightgreen.svg)](http://commitizen.github.io/cz-cli/)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit)
<!-- Badges: end -->

## Overview

`brendaDb` aims to make importing and analyzing data from the [BRENDA
database](https://www.brenda-enzymes.org) easier. The main functions
include:

  - \[x\] Read [text file downloaded from
    BRENDA](https://www.brenda-enzymes.org/download_brenda_without_registration.php)
    into an R `tibble`
  - \[x\] Retrieve information for specific enzymes
  - \[x\] Query enzymes using their synonyms, gene symbols, etc.
  - \[x\] Query enzyme information for specific
    [BioCyc](https://biocyc.org) pathways

<img src='man/figures/brendaDb.png' align="center" />

## Installation

`brendaDb` is a *Bioconductor* package and can be installed through
`BiocManager::install()`.

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("brendaDb", dependencies=TRUE)
```

Alternatively, install the development version from GitHub.

``` r
if(!requireNamespace("brendaDb")) {
  devtools::install_github("y1zhou/brendaDb")
}
```

After the package is installed, it can be loaded into the *R* workspace
by

``` r
library(brendaDb)
```

## Usage

To learn more about brendaDb, please refer to the
[vignette](https://bioconductor.org/packages/devel/bioc/vignettes/brendaDb/inst/doc/brendaDb.html).

``` r
browseVignettes(package = "brendaDb")
```
