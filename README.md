<!-- badges: start -->
[![GitHub version](https://img.shields.io/github/r-package/v/SzymonNowakowski/hclust1d?color=yellowgreen&label=GitHub&logo=github)](https://github.com/SzymonNowakowski/hclust1d)
[![CRAN version](https://img.shields.io/cran/v/hclust1d?logo=R)](https://cran.r-project.org/package=hclust1d)
[![downloads](https://cranlogs.r-pkg.org/badges/hclust1d)](https://cran.r-project.org/package=hclust1d)
<!-- badges: end -->


# Introduction

`hclust1d` (Hierarchical CLUSTering for 1D) is a suit of algorithms for 1 dimensional hierarchical clustering in $\mathcal{O}(n\log n)$ time. The better algorithmic time complexity paired with its very efficient C++ implementation make `hclust1d` very fast: the computational time beats `stats::hclust` on all sizes of data and is *en par* with `fastcluster::hclust` with small data sizes. However, it is orders of magnitude faster than both multivariate clustering routines on larger data sizes.

The outputs of `hclust1d` are in the same S3 class and format as the outputs of `stats::hclust` or  `fastcluster::hclust` and thus the resulting clustering can be further analysed with standard calls to `print`, `plot`, etc.

## Installing `hclust1d` package

To install the development package version please execute
```
library(devtools)
devtools::install_github("SzymonNowakowski/hclust1d")
```

Alternatively, to install the current stable CRAN version please execute

```
install.packages("hclust")
```

After that, you can load the installed package into memory with a call to `library(hclust1d)`.
