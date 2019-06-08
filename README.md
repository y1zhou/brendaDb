
# brendaDb

<!-- Badges: start -->

[![Build
Status](https://travis-ci.org/y1zhou/brendaDb.svg?branch=master)](https://travis-ci.org/y1zhou/brendaDb)
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
  - \[ \] Generate enzyme activity profiles based on temperature, pH,
    etc.

<img src='man/figures/brendaDb.png' align="center" />

## Installation

``` r
# install.packages("devtools")
devtools::install_github("y1zhou/brendaDb")
library(brendaDb)
```

## Getting Started

### Downloading Text File

Download the BRENDA database as [a text
file](https://www.brenda-enzymes.org/download_brenda_without_registration.php)
here. Alternatively, download the file in R (updated April 24, 2019):

``` r
DownloadBrenda(file="/path/to/textfile")
```

Now the text file can be loaded into R into a `tibble`:

``` r
df <- ReadBrenda("/path/to/textfile")
# Reading BRENDA text file...
# Converting text into a list. This might take a while...
# Converting list to tibble and removing duplicated entries...
```

## Making Queries

Since BRENDA is a database for enzymes, all final queries are based on
EC numbers.

### Query for Multiple Enzymes

``` r
res <- QueryBrenda(df, EC = c("1.1.1.1", "6.3.5.8"), n.core = 2)
# 6.3.5.8 was transferred or deleted.

res
# A list of 2 brenda.entry object(s) with:
# - 1 regular brenda.entry object(s)
#   1.1.1.1
# - 1 transferred or deleted object(s)
#   6.3.5.8

res[[1]]
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
```

### Query Specific Fields

``` r
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

### Query Specific Organisms

Note the difference in row numbers in the following example and in the
one where we queried for [all organisms](#query-for-multiple-enzymes).

``` r
res <- QueryBrenda(df, EC = "1.1.1.1", organisms = "Homo sapiens")
res$`1.1.1.1`
# Entry 1.1.1.1
# ├── nomenclature
# |    ├── ec: 1.1.1.1
# |    ├── systematic.name: alcohol:NAD+ oxidoreductase
# |    ├── recommended.name: alcohol dehydrogenase
# |    ├── synonyms: A tibble with 41 rows
# |    ├── reaction: A tibble with 2 rows
# |    └── reaction.type: A tibble with 3 rows
# ├── interactions
# |    ├── substrate.product: A tibble with 102 rows
# |    ├── natural.substrate.product: A tibble with 9 rows
# |    ├── cofactor: A tibble with 2 rows
# |    ├── metals.ions: A tibble with 2 rows
# |    ├── inhibitors: A tibble with 36 rows
# |    └── activating.compound: A tibble with 0 rows
# ├── parameters
# |    ├── km.value: A tibble with 163 rows
# |    ├── turnover.number: A tibble with 64 rows
# |    ├── ki.value: A tibble with 8 rows
# |    ├── pi.value: A tibble with 0 rows
# |    ├── ph.optimum: A tibble with 15 rows
# |    ├── ph.range: A tibble with 2 rows
# |    ├── temperature.optimum: A tibble with 2 rows
# |    ├── temperature.range: A tibble with 0 rows
# |    ├── specific.activity: A tibble with 5 rows
# |    └── ic50: A tibble with 0 rows
# ├── organism
# |    ├── organism: A tibble with 3 rows
# |    ├── source.tissue: A tibble with 21 rows
# |    └── localization: A tibble with 1 rows
# ├── molecular
# |    ├── stability
# |    |    ├── general.stability: A tibble with 1 rows
# |    |    ├── storage.stability: A tibble with 4 rows
# |    |    ├── ph.stability: A tibble with 1 rows
# |    |    ├── organic.solvent.stability: A tibble with 1 rows
# |    |    ├── oxidation.stability: A tibble with 0 rows
# |    |    └── temperature.stability: A tibble with 2 rows
# |    ├── purification: A tibble with 7 rows
# |    ├── cloned: A tibble with 5 rows
# |    ├── engineering: A tibble with 3 rows
# |    ├── renatured: A tibble with 0 rows
# |    └── application: A tibble with 1 rows
# ├── structure
# |    ├── molecular.weight: A tibble with 12 rows
# |    ├── subunits: A tibble with 3 rows
# |    ├── posttranslational.modification: A tibble with 0 rows
# |    └── crystallization: A tibble with 2 rows
# └── bibliography
# |    └── reference: A tibble with 285 rows
```

## Foreign ID Retrieval

### Querying Synonyms

A lot of the times we have a list of gene symbols or enzyme names
instead of EC numbers. In this case, a helper function can be used to
find the corresponding EC
numbers:

``` r
ID2Enzyme(brenda = df, ids = c("ADH4", "CD38", "pyruvate dehydrogenase"))
# A tibble: 14 x 5
#    ID       EC     RECOMMENDED_NAME         SYNONYMS               SYSTEMATIC_NAME           
#    <chr>    <chr>  <chr>                    <chr>                  <chr>                     
#  1 ADH4     1.1.1… NA                       "aldehyde reductase\n… NA                        
#  2 ADH4     1.1.1… NA                       "aldehyde reductase (… NA                        
#  3 CD38     2.4.9… NA                       "#1,3,4,6# CD38 (#1,4… NA                        
#  4 CD38     3.2.2… NA                       "DPNase\nDPN hydrolas… NA                        
#  5 CD38     3.2.2… NA                       "nucleosidase, nicoti… NA                        
#  6 pyruvat… 1.2.1… "pyruvate dehydrogenase… "#1,2# pyruvate:NADP+… NA                        
#  7 pyruvat… 1.2.4… "pyruvate dehydrogenase… "pyruvic dehydrogenas… NA                        
#  8 pyruvat… 1.2.4… NA                       "oxoglutarate decarbo… NA                        
#  9 pyruvat… 1.2.5… "pyruvate dehydrogenase… NA                     NA                        
# 10 pyruvat… 1.8.1… NA                       "dehydrogenase, lipoa… NA                        
# 11 pyruvat… 2.3.1… NA                       "acetyltransferase, l… NA                        
# 12 pyruvat… 2.3.1… NA                       "#1,2,3,4,6,7,8,9,10,… NA                        
# 13 pyruvat… 2.7.1… "[pyruvate dehydrogenas… "kinase (phosphorylat… "ATP:[pyruvate dehydrogen…
# 14 pyruvat… 3.1.3… "[pyruvate dehydrogenas… "phosphatase, pyruvat… "[pyruvate dehydrogenase …
```

The `EC` column can be then handpicked and used in `QueryBrenda()`.

### BioCyc Pathways

Often we are interested in the enzymes involved in a specific
[BioCyc](https://biocyc.org) pathway. Functions `BioCycPathwayEnzymes()`
and `BiocycPathwayGenes()` can be used in this case:

``` r
BiocycPathwayEnzymes(org.id = "HUMAN", pathway = "PWY66-400")
#> Found 10 reactions for HUMAN pathway PWY66-400.
## A tibble: 11 x 2
#    Reaction                 EC      
#    <chr>                    <chr>   
#  1 PGLUCISOM-RXN            5.3.1.9 
#  2 GLUCOKIN-RXN             2.7.1.1 
#  3 GLUCOKIN-RXN             2.7.1.2 
#  4 PEPDEPHOS-RXN            2.7.1.40
#  5 2PGADEHYDRAT-RXN         4.2.1.11
#  6 RXN-15513                5.4.2.11
#  7 PHOSGLYPHOS-RXN          2.7.2.3 
#  8 GAPOXNPHOSPHN-RXN        1.2.1.12
#  9 TRIOSEPISOMERIZATION-RXN 5.3.1.1 
# 10 F16ALDOLASE-RXN          4.1.2.13
# 11 6PFRUCTPHOS-RXN          2.7.1.11
```

``` r
BiocycPathwayGenes(org.id = "HUMAN", pathway = "PWY66-400")
#> Found 25 genes in HUMAN pathway PWY66-400.
# # A tibble: 25 x 4
#    BiocycGene BiocycProtein   Symbol Ensembl        
#    <chr>      <chr>           <chr>  <chr>          
#  1 HS00894    HS00894-MONOMER PFKP   ENSG00000067057
#  2 HS07832    HS07832-MONOMER PFKM   ENSG00000152556
#  3 HS06881    HS06881-MONOMER PFKL   ENSG00000141959
#  4 HS06234    HS06234-MONOMER ALDOB  ENSG00000136872
#  5 HS07647    HS07647-MONOMER ALDOA  ENSG00000149925
#  6 HS03200    HS03200-MONOMER ALDOC  ENSG00000109107
#  7 HS03441    HS03441-MONOMER TPI1   ENSG00000111669
#  8 HS02793    HS02793-MONOMER GAPDHS ENSG00000105679
#  9 HS03433    HS03433-MONOMER GAPDH  ENSG00000111640
# 10 HS02359    HS02359-MONOMER PGK1   ENSG00000102144
# # … with 15 more rows
```

## Additional Information

By default `QueryBrenda` uses all available cores, but often limiting
`n.core` could give better performance as it reduces the overhead. The
following are results produced on a machine with 40 cores (2 Intel Xeon
CPU E5-2640 v4 @ 3.4GHz), and 256G of RAM:

``` r
EC.numbers <- head(unique(df$ID), 100)
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 0))  # default
#  user  system elapsed
# 4.528   7.856  34.567
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 1))
#  user  system elapsed 
# 22.080   0.360  22.438
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 2))
#  user  system elapsed 
# 0.552   0.400  13.597 
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 4))
#  user  system elapsed 
# 0.688   0.832   9.517
system.time(QueryBrenda(df, EC = EC.numbers, n.core = 8))
#  user  system elapsed 
# 1.112   1.476  10.000 
```
