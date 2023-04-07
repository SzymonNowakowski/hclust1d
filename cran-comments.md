
# This is a resubmission of 0.0.1

- The following NOTEs from precheck **has been corrected**:

   1. Package has a VignetteBuilder field but no prebuilt vignette index.

   2. The Title field should be in title case. Current version is:
   'Hierarchical Clustering of Univariate (1D) Data'
   In title case that is:
   'Hierarchical Clustering of Univariate (1d) Data'

   3. The Description field should not start with the package name,
     'This package' or similar.

# Local checks

## local R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies
There seem to be no downstream dependencies:

```{r revdep}
devtools::revdep("hclust1d")
# character(0)
```
