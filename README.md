
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Current state and prospects of R-packages for the design of experiments

This is the repo for the paper titled “Current state and prospects of
R-packages for the design of experiments”. You can find the arxiv
version of the paper in `paper/arxiv/arxiv.pdf`. To reproduce the
results of the paper, you will need to run the analysis by calling

``` r
targets::tar_make()
```

The paper uses the developmental version of the [`cranscrub` package in
the branch
`paper-doe-review`](https://github.com/numbats/cranscrub/tree/paper-doe-review)
which can be installed by

``` r
remotes::install_github("numbats/cranscrub", ref = "paper-doe-review")
```

## Authors

    • - Emi Tanaka, Monash University
    • - Dewi Amaliah, Monash University

## Abstract

Re-running an experiment is generally costly and in some cases
impossible due to limited resources, so the design of an experiment
plays a critical role in increasing the quality of experimental data. In
this paper we describe the current state of the R-packages for the
design of experiments through an exploratory data analysis of package
downloads, package metadata, and the comparison of characteristics with
other topics. We observe that experimental designs in practice appear to
be sufficiently manufactured by a small number of packages and the
development of experimental designs often occur in silos. We discuss
also the interface designs of widely utilised R packages in the field of
experimental design and discuss its future prospects to advance the
field in practice.

## Supplementary material

The supplementary material can be found
[here](https://emitanaka.org/paper-DoE-review/supp.pdf).
