
# brendaDb

<!-- Badges: start -->

[![Build
Status](https://travis-ci.org/y1zhou/brendaDb.svg?branch=master)](https://travis-ci.org/y1zhou/brendaDb)
[![codecov](https://codecov.io/gh/y1zhou/brendaDb/branch/master/graph/badge.svg)](https://codecov.io/gh/y1zhou/brendaDb)
[![Commitizen
friendly](https://img.shields.io/badge/commitizen-friendly-brightgreen.svg)](http://commitizen.github.io/cz-cli/)
<!-- Badges: end -->

## Overview

`brendaDb` aims to make importing and analyzing data from the [BRENDA
database](https://www.brenda-enzymes.org) easier. The main functions
include:

  - \[x\] Read [text file downloaded from
    BRENDA](https://www.brenda-enzymes.org/download_brenda_without_registration.php)
    into an R `tibble`
  - \[x\] Retrieve information for specific enzymes
  - \[ \] Query enzyme information for specific pathways
  - \[ \] Generate enzyme activity profiles based on temperature, pH,
    etc.

<img src='man/figures/brendaDb.png' align="center" />

## Installation

``` r
# install.packages("devtools")
devtools::install_github("y1zhou/brendaDb")
```

## Getting Started

  - Download the BRENDA database as [a text
    file](https://www.brenda-enzymes.org/download_brenda_without_registration.php).
  - Read the text file into a `tibble`:

<!-- end list -->

``` r
df <- ReadBrenda("/path/to/textfile")
# Reading BRENDA text file...
# Converting text into a list. This might take a while...
# Converting list to tibble and removing duplicated entries...
```

  - Make a query using EC numbers:

<!-- end list -->

``` r
# Query for multiple enzymes -----
res <- QueryBrenda(df, EC = c("1.1.1.1", "6.3.5.8"))
# 6.3.5.8 was transferred or deleted.

summary(res)
# A list of 2 brenda.entry object(s) with:
# - 1 regular brenda.entry object(s)
#   1.1.1.1
# - 1 transferred or deleted object(s)
#   6.3.5.8

summary(res[[1]])
# Entry 1.1.1.1
# ├── nomenclature
# |    ├── ec: 1.1.1.1
# |    ├── systematic.name: alcohol:NAD+ oxidoreductase
# |    ├── recommended.name: alcohol dehydrogenase
# |    ├── synonyms: A tibble with 128 rows
# |    ├── reaction: A tibble with 2 rows
# |    └── reaction.type: A tibble with 3 rows
# ├── interactions
# |    ├── substrate.product: A tibble with 772 rows
# |    ├── natural.substrate.product: A tibble with 20 rows
# |    ├── cofactor: A tibble with 7 rows
# |    ├── metals.ions: A tibble with 20 rows
# |    ├── inhibitors: A tibble with 207 rows
# |    └── activating.compound: A tibble with 22 rows
# ├── parameters
# |    ├── km.value: A tibble with 878 rows
# |    ├── turnover.number: A tibble with 495 rows
# |    ├── ki.value: A tibble with 34 rows
# |    ├── pi.value: A tibble with 11 rows
# |    ├── ph.optimum: A tibble with 55 rows
# |    ├── ph.range: A tibble with 28 rows
# |    ├── temperature.optimum: A tibble with 29 rows
# |    ├── temperature.range: A tibble with 20 rows
# |    ├── specific.activity: A tibble with 88 rows
# |    └── ic50: A tibble with 2 rows
# ├── organism
# |    ├── organism: A tibble with 159 rows
# |    ├── source.tissue: A tibble with 63 rows
# |    └── localization: A tibble with 9 rows
# ├── molecular
# |    ├── stability
# |    |    ├── general.stability: A tibble with 15 rows
# |    |    ├── storage.stability: A tibble with 15 rows
# |    |    ├── ph.stability: A tibble with 20 rows
# |    |    ├── organic.solvent.stability: A tibble with 25 rows
# |    |    ├── oxidation.stability: A tibble with 3 rows
# |    |    └── temperature.stability: A tibble with 36 rows
# |    ├── purification: A tibble with 48 rows
# |    ├── cloned: A tibble with 46 rows
# |    ├── engineering: A tibble with 60 rows
# |    ├── renatured: A tibble with 1 rows
# |    └── application: A tibble with 5 rows
# ├── structure
# |    ├── molecular.weight: A tibble with 119 rows
# |    ├── subunits: A tibble with 11 rows
# |    ├── posttranslational.modification: A tibble with 2 rows
# |    └── crystallization: A tibble with 22 rows
# └── bibliography
# |    └── reference: A tibble with 285 rows

# Query specific fields -----
ShowFields(df)
# A tibble: 40 x 2
#    field                     acronym
#    <chr>                     <chr>  
#  1 PROTEIN                   PR     
#  2 RECOMMENDED_NAME          RN     
#  3 SYSTEMATIC_NAME           SN     
#  4 SYNONYMS                  SY     
#  5 REACTION                  RE     
#  6 REACTION_TYPE             RT     
#  7 SOURCE_TISSUE             ST     
#  8 LOCALIZATION              LO     
#  9 NATURAL_SUBSTRATE_PRODUCT NSP    
# 10 SUBSTRATE_PRODUCT         SP     
# … with 30 more rows

res <- QueryBrenda(df, EC = "1.1.1.1", fields = c("PROTEIN", "SUBSTRATE_PRODUCT"))
res$`1.1.1.1`$interactions$substrate.product
# A tibble: 772 x 7
#    proteinID substrate     product    commentarySubstrate        commentaryProdu… reversibility refID
#    <chr>     <chr>         <chr>      <chr>                      <chr>            <chr>         <chr>
#  1 10        n-propanol +… n-propana… NA                         NA               r             120  
#  2 10        2-propanol +… acetone +… NA                         NA               NA            122  
#  3 10        n-hexanol + … n-hexanal… NA                         NA               r             120  
#  4 10        (S)-2-butano… 2-butanon… NA                         NA               r             120  
#  5 10        ethylenglyco… ? + NADH   NA                         NA               r             120  
#  6 10        n-butanol + … butyralde… NA                         NA               NA            122  
#  7 10        n-decanol + … n-decanal… NA                         NA               r             120  
#  8 10        Tris + NAD+   ? + NADH   NA                         NA               r             120  
#  9 10        isopropanol … acetone +… NA                         NA               NA            139,…
# 10 10        5-hydroxymet… (furan-2,… #10# mutant enzyme S109P/… NA               NA            193,…
# … with 762 more rows
```

## Additional Information

By default `QueryBrenda` uses all available cores, but often limiting
`n.core` gives better performance:

``` r
EC.numbers <- head(unique(df$ID), 100)
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 0))  # default
#  user  system elapsed
# 4.420   7.408  34.689
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 1))
#  user  system elapsed 
# 22.952   0.012  22.957
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 2))
#  user  system elapsed 
# 0.316   0.368  14.439
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 4))
#  user  system elapsed
# 0.512   0.760   9.340
```
