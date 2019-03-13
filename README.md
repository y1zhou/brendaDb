
brendaDb
========

<!-- Badges: start -->
[![Build Status](https://travis-ci.org/y1zhou/brendaDb.svg?branch=master)](https://travis-ci.org/y1zhou/brendaDb) [![codecov](https://codecov.io/gh/y1zhou/brendaDb/branch/master/graph/badge.svg)](https://codecov.io/gh/y1zhou/brendaDb) <!-- Badges: end -->

Overview
--------

`brendaDb` aims to make importing and analyzing data from the [BRENDA database](https://www.brenda-enzymes.org) easier. The main functions include:

-   \[x\] Read [text file downloaded from BRENDA](https://www.brenda-enzymes.org/download_brenda_without_registration.php) into an R `data.table`
-   \[x\] Retrieve information for specific enzymes
-   \[ \] Query enzyme information for specific pathways
-   \[ \] Generate enzyme activity profiles based on temperature, pH, etc.

<img src='man/figures/brendaDb.png' align="center" />

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("y1zhou/brendaDb")
```

Getting Started
---------------

-   Download the BRENDA database as [a text file](https://www.brenda-enzymes.org/download_brenda_without_registration.php).
-   Read the text file into a `data.table`:

``` r
df <- ReadBrenda("/path/to/textfile")
# Reading BRENDA text file...
# Converting text into matrix. This might take a while...
# Converting matrix to data.table and removing duplicated entries...
```

-   Query for a specific enzyme using its EC number:

``` r
res <- QueryBrenda(df, EC = "1.1.1.1")
names(res)
# [1] "nomenclature" "interactions" "parameters"   "molecular"    "stability"    "bibliography"
names(res$nomenclature)
# [1] "ec"               "protein"          "systematic.name"  "recommended.name" "synonyms"
```
