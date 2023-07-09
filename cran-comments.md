# This is the first resubmission of 0.1.0

* checking tests ... [30m/30m] ERROR
  Running ‘testthat.R’ [30m/30m]
  
**MAINTAINER'S COMMENT: I limited the extent of unit tests to make it pass the precheck  rhub and win-devel checks**
  
* checking CRAN incoming feasibility ... [3s/4s] NOTE
Maintainer: ‘Szymon Nowakowski <s.nowakowski@mimuw.edu.pl>’

Found the following (possibly) invalid URLs:
  URL: https://cran.r-project.org/package=hclust1d/vignettes/getting-started.html
    From: inst/doc/replacing-hclust.html
          README.md
    Status: 404
    Message: Not Found
  URL: https://cran.r-project.org/package=hclust1d/vignettes/replacing-hclust.html
    From: inst/doc/getting-started.html
          README.md
    Status: 404
    Message: Not Found
    
**MAINTAINER'S COMMENT: Those URLs will be created during the release process**

# Local checks

## local R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies
There seem to be no downstream dependencies:

```{r revdep}
devtools::revdep("hclust1d")
# character(0)
```
